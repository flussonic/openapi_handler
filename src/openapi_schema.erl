-module(openapi_schema).
-include_lib("kernel/include/logger.hrl").

-export([load_schema/2, type/2]).
-export([process/2]).


-define(IS_SCALAR_TYPE(Scalar), (
  Scalar == <<"integer">> orelse
  Scalar == <<"number">> orelse
  Scalar == <<"string">> orelse
  Scalar == <<"boolean">>
)).

-define(IS_SCALAR(Schema),(
  (is_map_key(type,Schema) andalso ?IS_SCALAR_TYPE(map_get(type,Schema))) orelse
  is_map_key(const,Schema)
)).

-define(AVAILABLE_EXPLAIN_KEYS, [required, effective_schema]).


-spec load_schema(Schema :: map(), Name :: atom()) -> Schema :: map().
load_schema(Schema, Name) ->
  #{components := #{schemas := Schemas}} = Schema,
  [persistent_term:put({openapi_handler_schema,Name,atom_to_binary(Type,latin1)}, prepare_type(TypeSchema)) ||
    {Type,TypeSchema} <- maps:to_list(Schemas)],
  persistent_term:put({openapi_handler_schema,Name},Schema),
  Schema.

-spec type(SchemaName :: atom(), TypeName :: atom()) -> #{}.
type(SchemaName, TypeName) ->
  persistent_term:get({openapi_handler_schema, SchemaName, atom_to_binary(TypeName)}).


process(Input, #{} = Opts) ->
  maps:map(fun
    (schema,_) -> ok;
    (whole_schema,#{}) -> ok;
    (type,T) when is_atom(T) -> ok;
    (name,_) -> ok;
    (array_convert,Flag) when Flag == true; Flag == false -> ok;
    (auto_convert,Flag) when Flag == true; Flag == false -> ok;
    (validators,#{} = V) -> V;
    (query,Flag) when Flag == true; Flag == false -> ok;
    (apply_defaults,Flag) when Flag == true; Flag == false -> ok;
    (patch,Flag) when Flag == true; Flag == false -> ok;
    (extra_obj_key,Flag) when Flag == drop; Flag == error -> ok;
    (required_obj_keys,Flag) when Flag == drop; Flag == error -> ok;
    (access_type,Flag) when Flag == read; Flag == write; Flag == raw -> ok;
    (explain,FlagList) -> check_explain_keys(FlagList);
    (K,V) -> error({unknown_option,K,V})
  end, Opts),
  Schema = case Opts of
    #{schema := Schema_} -> Schema_;
    #{type := Type} -> #{'$ref' => <<"#/components/schemas/", (atom_to_binary(Type,latin1))/binary>>};
    _ -> error(not_specified_schema)
  end,
  DefaultArrayConvert = case maps:get(auto_convert, Opts, true) of
    true -> true;
    false -> false
  end,
  DefaultOpts = #{
    query => false,
    patch => false,
    array_convert => DefaultArrayConvert,
    auto_convert => true,
    extra_obj_key => drop,
    required_obj_keys => drop,
    access_type => read
  },
  Validators = maps:merge(default_validators(), maps:get(validators, Opts, #{})),
  FinalOpts = (maps:merge(DefaultOpts,Opts))#{validators => Validators},
  case encode3(Schema, FinalOpts, Input, []) of
    {error, Error} ->
      {error, Error};
    R ->
      R
  end.



prepare_type(#{allOf := Types} = Type0) ->
  Type0#{allOf := [prepare_type(T) || T <- Types]};
prepare_type(#{anyOf := Types} = Type0) ->
  Type0#{anyOf := [prepare_type(T) || T <- Types]};
prepare_type(#{oneOf := Types} = Type0) ->
  Type0#{oneOf := [prepare_type(T) || T <- Types]};
prepare_type(#{type := <<"object">>, properties := Props} = Type0) ->
  Type0#{properties => maps:map(fun(_, T) -> prepare_type(T) end, Props)};
prepare_type(#{type := <<"array">>, items := Items} = Type0) ->
  Type0#{items => prepare_type(Items)};
prepare_type(#{} = Type0) ->
  % Convert format name to atom. This matches validators syntax
  Type1 = case Type0 of
    #{format := BinFormat} when is_binary(BinFormat) ->
      Type0#{format := binary_to_atom(BinFormat)};
    #{} ->
      Type0
  end,
  % precompile patterns
  Type2 = case Type1 of
    #{pattern := Pattern} ->
      {ok, MP} = re:compile(Pattern, []),
      Type1#{pattern := MP};
    #{} ->
      Type1
  end,
  Type2.


% FIXME: Decide whether default formats should be supported by this library.
% - JSON Schema Validation formats (https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-validation-00#section-7.3)
% - OAS (https://spec.openapis.org/oas/v3.1.0.html#data-types)
% JSON schema defines many non-trivial formats (e.g. RFC 3339 date-time, idn-email, iri-reference, etc.),
% and supporting all of them properly may require lots of maintenance work.
% Formats added by OAS are quite simple though.
default_validators() ->
  #{}.


encode3(_, #{query := true}, not_null, _) ->
  not_null;

encode3(_, #{query := true}, null, _) ->
  undefined;

encode3(#{nullable := true}, _, undefined, _) ->
  undefined;

encode3(#{nullable := true}, _, null, _) ->
  undefined;

encode3(#{}, #{patch := true}, null, _) ->
  undefined;

encode3(Schema, #{} = Opts, Null, Path) when Null == null orelse Null == undefined ->
  error(#{error => must_not_get_here, path => Path, null => Null, opts => Opts, schema => Schema});

encode3(#{'$ref' := <<"#/components/schemas/",Ref/binary>>}, #{} = Opts, Input, Path) ->
  TypeName = binary_to_atom(Ref,latin1), % It is ok, because this is a limited and trusted schema
  Type = case Opts of
    #{whole_schema := #{components := #{schemas := #{TypeName := T}}}} -> T;
    #{name := Name} -> persistent_term:get({openapi_handler_schema,Name,Ref})
  end,
  encode3(Type, Opts, Input, Path ++ [Ref]);

encode3(#{allOf := [Choice]}, Opts, Input, Path) ->
  encode3(Choice, Opts, Input, Path ++ [0]);

encode3(#{allOf := Choices}, Opts, Input, Path) ->
  Encoded = lists:foldl(fun
    (_, {error, _} = E) ->
      E;
    ({N,Choice}, Obj) ->
      case encode3(Choice, Opts, Input, Path ++ [N]) of
        {error, #{extra_keys := _Extrakeys, encoded := Obj1}} ->
          merge_objects(Opts, #{}, [Obj, Obj1]);  % keep processing, all choices have to be processed for allOf
        {error, E} ->
          {error, E};
        #{} = Obj1 ->
          % explain in Obj1 is already defined by encode3, so pass a bogus schema
          merge_objects(Opts, #{}, [Obj, Obj1])
      end
  end, #{}, lists:zip(lists:seq(0,length(Choices)-1),Choices)),
  check_extra_keys(Input, Encoded, Opts);

encode3(#{anyOf := Choices}, Opts, Input, Path) ->
  Count = length(Choices),
  F = fun
    F([], LastError) ->
      {error, LastError};
    F([Choice|List], _) ->
      case encode3(Choice, Opts, Input, Path ++ [Count - length(List) - 1]) of
        {error, #{extra_keys := _Extrakeys, encoded := Encoded}} ->
          % TODO: check anyOf semantics, add explicit tests for that
          Encoded;
        {error, Error} ->
          F(List, Error);
        EncodedItem ->
          EncodedItem
      end
  end,
  Encoded = F(Choices, #{error => unmatched_anyOf, path => Path}),
  check_extra_keys(Input, Encoded, Opts);

% Skip discriminator resolving during of query check
encode3(#{discriminator := _} = Schema, #{query := true} = Opts, Input, Path) ->
  encode3(maps:without([discriminator], Schema), Opts, Input, Path);

encode3(#{discriminator := #{propertyName := DKey, mapping := DMap}} = Schema, Opts0, Input, Path)
  when not is_map_key({skip_discriminator, DKey}, Opts0)
->
  Types = case Schema of
    #{oneOf := OneOfTypes} -> OneOfTypes;
    #{} -> [#{'$ref' => T} || T <- maps:values(DMap)]
  end,
  Opts = Opts0#{{skip_discriminator, DKey} => already_handled},

  % If possible, get the discriminator value as atom (for lookup in mapping)
  ADKey = binary_to_atom(DKey),
  DefaultFun = fun(Input_) ->
    case Input_ of
      #{DKey := DValue} when is_atom(DValue) -> DValue;
      #{ADKey := DValue} when is_atom(DValue) -> DValue;
      #{DKey := DValue} when is_binary(DValue) ->
        try binary_to_existing_atom(DValue) catch error:badarg -> DValue end;
      #{ADKey := DValue} when is_binary(DValue) ->
        try binary_to_existing_atom(DValue) catch error:badarg -> DValue end;
      #{} -> undefined;
      {error, _} -> Input_
    end
  end,
  DiscrInput = maps:with([DKey, ADKey], Input),
  ADvalue1 = DefaultFun(DiscrInput),
  ADvalue2 = case ADvalue1 of
    undefined ->
      Try = encode3(hd(Types), Opts#{apply_defaults => true, required_obj_keys => drop}, DiscrInput, Path),
      DefaultFun(Try);
    _ ->
      ADvalue1
  end,
  DChoice = maps:get(ADvalue2, DMap, undefined),
  case {ADvalue2, DChoice} of
    {undefined, _} when map_get(patch, Opts) == true ->
      encode3(maps:without([discriminator], Schema), Opts, Input, Path);
    {undefined, _} ->
      {error, #{error => discriminator_missing, path => Path, propertyName => DKey}};
    {{error, _}, _} ->
      % this error comes from processing discriminator with first possible type, and may contain useful type error (e.g. not_string)
      ADvalue2;
    {_, undefined} ->
      {error, #{error => discriminator_unmapped, path => Path, propertyName => DKey, value => ADvalue2}};
    {_, _} ->
      Result0 = encode3(#{'$ref' => DChoice}, Opts, Input, Path),
      case is_map(Result0) andalso maps:values(maps:with([DKey, ADKey], Result0)) of
        false ->
          % Error
          Result0;
        [<<_/binary>>] ->
          % discriminator value not described as const, but has type: string
          (maps:without([DKey, ADKey], Result0))#{ADKey => ADvalue2};
        [ADvalue2] ->
          Result0;
        [] ->
          Result0
      end
  end;

encode3(#{oneOf := Choices}, Opts, Input, Path) ->
  EncodedList = lists:map(fun({Choice,I}) ->
    case encode3(Choice, Opts, Input, Path ++ [I]) of
      {error, #{extra_keys := _Extrakeys, encoded := Encoded}} ->
        % Wrong oneOf choice. Will try other ones and check results
        Encoded;
      {error, E} ->
        {error, E};
      V -> {ok, V}
    end
  end, lists:zip(Choices,lists:seq(0,length(Choices)-1))),
  %% If there are several valid results, choose non-empty one
  case [M || {ok, #{} = M} <- EncodedList, map_size(M) > 0] of
    [Encoded|_] -> Encoded;
    _ ->
      case [V || {ok, V} <- EncodedList] of
        [Encoded|_] -> Encoded;
        _ -> hd(EncodedList)
      end
  end;

encode3(#{type := <<"object">>, maxItems := MaxItems}, #{}, #{} = Input, Path) when map_size(Input) > MaxItems ->
  {error, #{error => too_many_items, detail => map_size(Input), path => Path}};

encode3(#{type := <<"object">>, minItems := MinItems}, #{}, #{} = Input, Path) when map_size(Input) < MinItems ->
  {error, #{error => too_few_items, detail => map_size(Input), path => Path}};

encode3(#{type := <<"object">>, properties := Properties} = Schema, #{query := Query} = Opts, #{} = Input, Path) ->
  Artificial = #{
    '$position' => #{type => <<"integer">>},
    '$reset' => #{type => <<"boolean">>},
    '$index' => #{type => <<"integer">>},
    '$delete' => #{type => <<"boolean">>}
  },
  Encoded = maps:fold(fun
    (_, _, {error, _} = E) ->
      E;
    (Field, #{} = Prop, Obj) ->
      FieldBin = atom_to_binary(Field,latin1),

      RequiredKeys = get_required_keys(Schema, Opts),
      IsReadOnly = maps:get(readOnly, Prop, false),
      IsWriteOnly = maps:get(writeOnly, Prop, false),
      IsPrimary = maps:get('x-primary-key', Prop, false),
      IsRequired = (lists:member(FieldBin, RequiredKeys) orelse IsPrimary),
      IsReadAccess = maps:get(access_type, Opts, raw) == read,
      IsWriteAccess = maps:get(access_type, Opts, raw) == write,

      ApplyDefaults = maps:get(apply_defaults, Opts, false),
      EffectiveValue = case {Input, Prop} of
        {#{Field := Value_}, #{}} ->
          {ok, Value_};
        {#{FieldBin := Value_}, #{}} ->
          {ok, Value_};
        {#{}, #{default := DefaultValue}} when ApplyDefaults ->
          {ok, DefaultValue};
        {#{}, #{}} ->
          undefined
      end,

      NullableProp = case Prop of
        #{nullable := true} ->
          true;
        #{oneOf := OneOf} ->
          lists:any(fun(#{type := <<"null">>}) -> true; (_) -> false end, OneOf);
        #{} ->
          false
      end,
      Patching = maps:get(patch, Opts, undefined) == true,
      UpdatedObj = case EffectiveValue of
        {ok, NullFlag} when Query andalso (NullFlag == null orelse NullFlag == not_null) ->
          Obj#{Field => NullFlag};

        % Silently drop undefined values for non-nullable fields
        {ok, Null} when (Null == null orelse Null == undefined) andalso
          not NullableProp andalso not Patching ->
          Obj;
        % Silently drop readOnly fields on write
        {ok, _Value} when IsWriteAccess andalso IsReadOnly andalso (not IsRequired) ->
          Obj;
        % Silently drop writeOnly fields on read
        {ok, _Value} when IsReadAccess andalso IsWriteOnly andalso (not IsRequired) ->
          Obj;
        {ok, Value} ->
          case encode3(Prop#{nullable => NullableProp}, Opts, Value, Path ++ [Field]) of
            {error, _} = E ->
              E;
            Value1 when Query andalso (is_number(Value1) orelse is_atom(Value1) orelse is_binary(Value1)) ->
              Obj#{Field => maps:get(Field,Obj,[]) ++ [Value1]};
            Value1 ->
              Obj#{Field => Value1}
          end;
        undefined ->
          Obj
      end,
      UpdatedObj
  end, #{}, maps:merge(Artificial,Properties)),
  Encoded1 = case check_required_keys(Encoded, Schema, Opts) of
    {error, E} -> {error, E};
    Encoded -> check_extra_keys(Input, Encoded, Opts)
  end,
  merge_objects(Opts, Schema, [Encoded1]);


encode3(#{type := <<"object">>}, _Opts, #{} = Input, _Path) ->
  Input;

encode3(#{type := <<"object">>}, _Opts, Input, Path) ->
  {error, #{error => not_object, path => Path, input => Input}};

encode3(#{type := <<"array">>, maxItems := MaxItems}, _Opts, Input, Path) when is_list(Input) andalso length(Input) > MaxItems ->
  {error, #{error => too_many_items, path => Path, detail => length(Input)}};
encode3(#{type := <<"array">>, minItems := MinItems}, _Opts, Input, Path) when is_list(Input) andalso length(Input) < MinItems ->
  {error, #{error => too_few_items, path => Path, detail => length(Input)}};

encode3(#{type := <<"array">>, items := ItemSpec}, Opts, Input, Path) when is_list(Input) ->
  NullableItems = maps:get(nullable, ItemSpec, undefined) == true,
  Count = length(Input),
  Encoded = lists:foldr(fun
    (_,{error, E}) ->
      {error, E};
    (Null, Acc) when (Null == null orelse Null == undefined) andalso not NullableItems ->
      {error, #{error => null_in_array_of_non_nullable, path => Path ++ [Count - length(Acc)], input => Null}};
    (Item, Acc) ->
      case encode3(ItemSpec, Opts, Item, Path ++ [Count - length(Acc)]) of
        {error, E} -> {error, E};
        EncodedItem -> [EncodedItem|Acc]
      end
  end, [], Input),
  Encoded;

encode3(#{type := <<"array">>} = Spec, #{auto_convert := true, array_convert := true} = O, Input, Path) when is_binary(Input) ->
  encode3(Spec, O, binary:split(Input, <<",">>, [global]), Path);

encode3(#{type := <<"array">>, items := _ItemSpec}, _Opts, Input, Path) when not is_list(Input) ->
  {error, #{error => not_array, path => Path, input => Input}};

encode3(#{type := <<"string">>}, #{query := true}, #{} = Input, _Path) ->
  Input;

encode3(#{} = Schema, #{query := true} = Opts, #{} = Input, Path) when ?IS_SCALAR(Schema) ->
  Encoded = lists:foldl(fun
    (_,{error,E}) ->
      {error,E};
    ({Compare,StrVal}, Acc) ->
      case encode3(Schema, Opts, StrVal, Path ++ [Compare]) of
        {error, E} ->
          {error, E};
        Val1 ->
          Acc ++ [{Compare,Val1}]
      end
  end, [], maps:to_list(Input)),
  case Encoded of
    {error, _} -> Encoded;
    _ -> maps:from_list(Encoded)
  end;

encode3(#{} = Schema, #{query := true} = Opts, [<<_/binary>>|_] = Input, Path) when ?IS_SCALAR(Schema) ->
  lists:foldl(fun
    (_, {error, E}) ->
      {error, E};
    (OneInput, Acc) ->
      case encode3(Schema, Opts, OneInput, Path) of
        {error, E} -> {error, E};
        Value -> Acc ++ [Value]
      end
  end, [], Input);


encode3(#{type := <<"integer">>} = Schema, #{auto_convert := Convert}, Input, Path) ->
  case Input of
    _ when is_integer(Input) -> encode_number(Schema, Input, Path);
    _ when is_binary(Input) andalso Convert == true ->
      case string:to_integer(Input) of
        {IntValue, <<>>} -> encode_number(Schema, IntValue, Path);
        _ -> {error, #{error => not_integer, path => Path, input => Input}}
      end;
    _ -> {error, #{error => not_integer, path => Path, input => Input}}
  end;

encode3(#{type := <<"number">>} = Schema, #{auto_convert := Convert}, Input, Path) ->
  case Input of
    _ when is_integer(Input) orelse is_float(Input) -> encode_number(Schema, Input, Path);
    _ when is_binary(Input) andalso Convert == true ->
      case string:to_integer(Input) of
        {IntValue, <<>>} ->
          encode_number(Schema, IntValue, Path);
        _ ->
          case string:to_float(Input) of
            {FloatValue,<<>>} -> encode_number(Schema, FloatValue, Path);
            _ -> {error, #{error => not_integer, path => Path, input => Input}}
          end
      end;
    _ -> {error, #{error => not_number, path => Path, input => Input}}
  end;

encode3(#{const := Value}, #{auto_convert := Convert}, Input, Path) when is_atom(Input) orelse is_binary(Input) orelse is_integer(Input) ->
  case Input of
    <<Value/binary>> when Convert ->
      binary_to_atom(Value,latin1);
    Value ->
      Input;
    _ when is_integer(Value) ->
      case integer_to_binary(Value) of
        Input -> Value;
        _ -> {error, #{error => not_const3, path => Path, input => Input, value => Value}}
      end;
    _ when is_atom(Input) andalso Convert == true ->
        case atom_to_binary(Input,latin1) of
          Value -> Input;
          _ -> {error, #{error => not_const1, path => Path, input => Input}}
        end;
    _ -> {error, #{error => not_const2, path => Path, input => Input, value => Value}}
  end;

encode3(#{enum := Choices, type := <<"string">>}, #{auto_convert := Convert}, Input, Path) ->
  InputValue = case Input of
    _ when is_binary(Input) -> Input;
    _ when is_atom(Input) -> atom_to_binary(Input, latin1);
    _ -> {error, #{error => not_string, path => Path}}
  end,
  case lists:member(InputValue, Choices) of
    true when is_binary(Input) andalso Convert -> binary_to_atom(Input,latin1);
    true when is_atom(Input) -> Input;
    false -> {error, #{unknown_enum_option => Input, path => Path, available => Choices}}
  end;

encode3(#{type := <<"string">>} = Spec, #{auto_convert := Convert} = Options, Input, Path) ->
  {Input1, InputForValidation} = case Input of
    _ when is_binary(Input) -> {Input, Input};
    _ when is_boolean(Input) -> {{error, #{error => not_string, path => Path, input => Input}}, undefined};
    _ when is_atom(Input) andalso Convert -> {atom_to_binary(Input), atom_to_binary(Input)};
    _ when is_atom(Input) -> {Input, atom_to_binary(Input)};
    _ -> {{error, #{error => not_string, path => Path, input => Input}}, undefined}
  end,
  case Input1 of
    {error, _} ->
      Input1;
    _ ->
      Length = string:length(InputForValidation),
      case Spec of
        #{minLength := MinLength} when Length < MinLength ->
          {error, #{error => too_short, path => Path, input => Input, detail => Length, min_length => MinLength}};
        #{maxLength := MaxLength} when Length > MaxLength ->
          {error, #{error => too_long, path => Path, input => Input, detail => Length, max_length => MaxLength}};
        #{} ->
          Format = maps:get(format, Spec, undefined),
          Validators = maps:get(validators, Options),
          FormatChecked = validate_string_format(InputForValidation, Format, maps:get(Format, Validators, undefined), Convert),
          PatternChecked = validate_string_pattern(FormatChecked, maps:get(pattern, Spec, undefined)),
          case PatternChecked of
            {error, Error} ->
              {error, Error#{path => Path, input => Input1}};
            <<_/binary>> when Convert ->
              PatternChecked;
            <<_/binary>> ->
              Input1
          end
      end
  end;


encode3(#{type := <<"boolean">>}, #{auto_convert := Convert}, Input, Path) ->
  case Input of
    true -> true;
    false -> false;
    <<"true">> when Convert -> true;
    <<"false">> when Convert -> false;
    _ -> {error, #{error => not_boolean, path => Path, input => Input}}
  end;

encode3(#{type := <<"null">>}, #{}, Input, Path) ->
  case Input of
    null -> undefined;
    undefined -> undefined;
    _ -> {error, #{error => not_null, path => Path, input => Input}}
  end;

encode3(#{type := [_| _] = Types} = Schema, Opts, Input, Path) ->
  encode3_multi_types(Types, Schema, Opts, Input, Path).

encode3_multi_types([], _Schema, _Opts, Input, Path) ->
  {error, #{error => invalid_type, path => Path, input => Input}};
encode3_multi_types([Type| Types], Schema, Opts, Input, Path) ->
  case encode3(Schema#{type => Type}, Opts, Input, Path) of
    {error, _ } -> encode3_multi_types(Types, Schema, Opts, Input, Path);
    Encoded -> Encoded
  end.


encode_number(#{minimum := Min}, Input, Path) when Input < Min ->
  {error, #{error => input_out_of_range, path => Path, input => Input, minimum => Min}};

encode_number(#{maximum := Max}, Input, Path) when Input > Max ->
  {error, #{error => input_out_of_range, path => Path, input => Input, maximum => Max}};

encode_number(_Schema, Input, _Path) ->
  Input.


validate_string_format(Input, _, undefined, _) ->
  Input;
validate_string_format(Input, Format, Validator, Convert) ->
  case Validator(Input) of
    {ok, Converted} when (not Convert) andalso Converted /= Input ->
      % The value is mostly ok, but needs to be converted
      {error, #{error => needs_convertation, format => Format}};
    {ok, Input2} ->
      Input2;
    {error, FmtError} ->
      Err0 = #{error => wrong_format, format => Format},
      {error, maps:merge(Err0, FmtError)}
  end.

validate_string_pattern({error, _} = Error, _) ->
  Error;
validate_string_pattern(Input, undefined) ->
  Input;
validate_string_pattern(Input, RegExp) ->
  case re:run(Input, RegExp) of
    {match, _} ->
      Input;
    nomatch when element(1, RegExp) == re_pattern ->
      {error, #{error => nomatch_pattern}};
    nomatch ->
      {error, #{error => nomatch_pattern, pattern => RegExp}}
  end.


check_required_keys(#{} = Encoded, #{} = Schema, #{required_obj_keys := error} = Opts) ->
  Required = get_required_keys(Schema, Opts),
  case Required -- maps:keys(add_binary_keys(Encoded)) of
    [] -> Encoded;
    Missing -> {error, #{missing_required => Missing}}
  end;
check_required_keys(Encoded, _Schema, _Opts) ->
  Encoded.

get_required_keys(#{required := [_| _] = Required, properties := Properties}, #{access_type := Access}) ->
  Properties1 = add_binary_keys(Properties),
  lists:filter(fun(P) ->
    case Properties1 of
      #{P := #{readOnly := true}} when Access == write -> false;
      #{P := #{writeOnly := true}} when Access == read -> false;
      #{P := _} -> true;
      _ -> false
    end
  end, Required);
get_required_keys(_Schema, _Opts) ->
  [].


check_extra_keys(Input, Encoded, #{extra_obj_key := error} = Opts) when is_map(Input) andalso is_map(Encoded) ->
  ExtraKeys = maps:keys(Input) -- maps:keys(add_binary_keys(Encoded)),
  case ExtraKeys of
    ['$explain'] when #{explain => [required]} == Opts -> Encoded;
    [_|_] -> {error, #{extra_keys => ExtraKeys, encoded => Encoded}};
    _ -> Encoded
  end;

check_extra_keys(_Input, Encoded, _Opts) ->
  Encoded.


add_binary_keys(Map) ->
  maps:fold(fun
    (K, V, Acc) when is_atom(K) -> Acc#{atom_to_binary(K) => V};
    (_K, _V, Acc) -> Acc
  end, Map, Map).


merge_objects(Opts, Schema, Objs) ->
  ExplainMap = case Opts of
    #{explain := [required]} ->
      RequiredKeys = lists:foldl(fun
        ({error, _}, Acc) -> Acc;
        (Obj, Acc) -> lists:merge(maps:get(required, maps:get('$explain', Obj, #{}), []), Acc)
      end, get_required_keys(Schema, Opts), Objs),
      #{'$explain' => #{required => RequiredKeys}};
    #{explain := [effective_schema]} ->
      PrevEffSchemas = [ES || #{'$explain' := #{effective_schema := ES}} <- Objs],
      EffectiveSchema = lists:foldl(fun merge_schemas/2, Schema, PrevEffSchemas),
      #{'$explain' => #{effective_schema => EffectiveSchema}};
    _ -> #{}
  end,
  lists:foldl(fun
    (Obj, Acc) when is_map(Obj) andalso is_map(Acc) -> maps:merge(Obj, Acc);
    ({error, E}, _) -> {error, E};
    (_, {error, E}) -> {error, E}
  end, ExplainMap, Objs).

% Merge schemas for e.g. allOf for introspection.
% Lists are concatenated
% Maps are deep-merged
% When given two scalar values, second one has priority
%
% So, attributes with list values (like required) are collected across variants
merge_schemas(S1, S2) when is_map(S1), is_map(S2) ->
  S2m = maps:map(fun(K, V) -> merge_schemas(maps:get(K, S1, undefined), V) end, S2),
  maps:merge(S1, S2m);
merge_schemas(L1, L2) when is_list(L1), is_list(L2) ->
  L1 ++ L2;
merge_schemas(_, S2) when is_map(S2) ->
  S2;
merge_schemas(S1, _) when is_map(S1) ->
  S1;
merge_schemas(_, V) ->
  V.


check_explain_keys(Keys) when is_list(Keys) ->
  [error({unknown_option,explain,Key}) || Key <- Keys, lists:member(Key, ?AVAILABLE_EXPLAIN_KEYS) =/= true],
  ok;

check_explain_keys(Keys) ->
  error({unknown_option,explain,Keys}).
