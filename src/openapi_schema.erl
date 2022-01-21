-module(openapi_schema).
-include_lib("kernel/include/logger.hrl").

-export([process/2]).


-define(IS_SCALAR(Scalar), (Scalar == <<"integer">> orelse Scalar == <<"number">> orelse Scalar == <<"string">> orelse Scalar == <<"boolean">>)).


process(Input, #{} = Opts) ->
  maps:map(fun
    (schema,_) -> ok;
    (whole_schema,#{}) -> ok;
    (type,T) when is_atom(T) -> ok;
    (name,_) -> ok;
    (auto_convert,Flag) when Flag == true; Flag == false -> ok;
    (query,Flag) when Flag == true; Flag == false -> ok;
    (K,V) -> error({unknown_option,K,V})
  end, Opts),
  Schema = case Opts of
    #{schema := Schema_} -> Schema_;
    #{type := Type} -> #{'$ref' => <<"#/components/schemas/", (atom_to_binary(Type,latin1))/binary>>};
    _ -> error(not_specified_schema)
  end,
  case encode3(Schema, maps:merge(#{query => false, auto_convert => true},Opts), Input, []) of
    {error, Error} ->
      {error, Error};
    R ->
      R
  end.

encode3(#{nullable := true}, _, undefined, _) ->
  undefined;

encode3(_, #{query := true}, not_null, _) ->
  not_null;

encode3(_, #{}, null, _) ->
  undefined;

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
        {error, E} -> {error, E};
        #{} = Obj1 -> maps:merge(Obj, Obj1)
      end
  end, #{}, lists:zip(lists:seq(0,length(Choices)-1),Choices)),
  Encoded;

encode3(#{anyOf := Choices}, Opts, Input, Path) ->
  Count = length(Choices),
  F = fun
    F([], LastError) ->
      {error, LastError};
    F([Choice|List], _) ->
      case encode3(Choice, Opts, Input, Path ++ [Count - length(List) - 1]) of
        {error, Error} ->
          F(List, Error);
        EncodedItem ->
          EncodedItem
      end
  end,
  Encoded = F(Choices, #{error => unmatched_anyOf, path => Path}),
  Encoded;

encode3(#{oneOf := Choices}, Opts, Input, Path) ->
  EncodedList = lists:map(fun({Choice,I}) ->
    case encode3(Choice, Opts, Input, Path ++ [I]) of
      {error, E} -> {error, E};
      V -> {ok, V}
    end
  end, lists:zip(Choices,lists:seq(0,length(Choices)-1))),
  %% Может получится несколько валидных ответов, выбираем непустой
  case [M || {ok, #{} = M} <- EncodedList, map_size(M) > 0] of
    [Encoded|_] -> Encoded;
    _ ->
      case [V || {ok, V} <- EncodedList] of
        [Encoded|_] -> Encoded;
        _ -> hd(EncodedList)
      end
  end;

encode3(#{type := <<"object">>, properties := Properties}, #{query := Query} = Opts, Input, Path) ->
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
      ExtractedValue = case Input of
        #{Field := Value_} ->
          {ok, Value_};
        #{FieldBin := Value_} ->
          {ok, Value_};
        #{} ->
          undefined;
        _ ->
          error(#{input => Input, field => Field, prop => Prop, obj => Obj, path => Path})
      end,

      UpdatedObj = case ExtractedValue of
        {ok, NullFlag} when Query andalso (NullFlag == null orelse NullFlag == not_null) ->
          Obj#{Field => NullFlag};          
        {ok, Value} ->
          case encode3(Prop, Opts, Value, Path ++ [Field]) of
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
  Encoded;

encode3(#{type := <<"object">>}, _Opts, #{} = Input, _Path) ->
  Input;

encode3(#{type := <<"array">>, items := ItemSpec}, Opts, Input, Path) when is_list(Input) ->
  Count = length(Input),
  Encoded = lists:foldr(fun
    (_,{error, E}) ->
      {error, E};
    (Item, Acc) ->
      case encode3(ItemSpec, Opts, Item, Path ++ [Count - length(Acc)]) of
        {error, E} -> {error, E};
        EncodedItem -> [EncodedItem|Acc]
      end
  end, [], Input),
  Encoded;

encode3(#{type := <<"array">>, items := _ItemSpec}, _Opts, Input, Path) when not is_list(Input) ->
  {error, #{error => not_array, path => Path, input => Input}};

encode3(#{type := <<"string">>}, #{query := true}, #{} = Input, _Path) ->
  Input;

encode3(#{type := Scalar} = Schema, #{query := true} = Opts, #{} = Input, Path) when ?IS_SCALAR(Scalar) ->
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

encode3(#{type := Scalar} = Schema, #{query := true} = Opts, [<<_/binary>>|_] = Input, Path) when ?IS_SCALAR(Scalar) ->
  lists:foldl(fun
    (_, {error, E}) ->
      {error, E};
    (OneInput, Acc) ->
      case encode3(Schema, Opts, OneInput, Path) of
        {error, E} -> {error, E};
        Value -> Acc ++ [Value]
      end
  end, [], Input);


encode3(#{type := <<"integer">>}, #{auto_convert := Convert}, Input, Path) ->
  case Input of
    _ when is_integer(Input) -> Input;
    _ when is_binary(Input) andalso Convert == true ->
      case string:to_integer(Input) of
        {IntValue, <<>>} -> IntValue;
        _ -> {error, #{error => not_integer, path => Path, input => Input}}
      end;
    _ -> {error, #{error => not_integer, path => Path, input => Input}}
  end;

encode3(#{type := <<"number">>}, #{auto_convert := Convert}, Input, Path) ->
  case Input of
    _ when is_integer(Input) orelse is_float(Input) -> Input;
    _ when is_binary(Input) andalso Convert == true ->
      case string:to_integer(Input) of
        {IntValue, <<>>} ->
          IntValue;
        _ ->
          case string:to_float(Input) of
            {FloatValue,<<>>} -> FloatValue;
            _ -> {error, #{error => not_integer, path => Path, input => Input}}
          end
      end;
    _ -> {error, #{error => not_number, path => Path, input => Input}}
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

encode3(#{type := <<"string">>}, _, Input, Path) ->
  case Input of
    _ when is_binary(Input) -> Input;
    _ when is_atom(Input) -> Input;
    _ -> {error, #{error => not_string, path => Path, input => Input}}
  end;

encode3(#{type := <<"boolean">>}, #{auto_convert := Convert}, Input, Path) ->
  case Input of
    true -> true;
    false -> false;
    <<"true">> when Convert -> true;
    <<"false">> when Convert -> false;
    _ -> {error, #{error => not_boolean, path => Path}}
  end.


