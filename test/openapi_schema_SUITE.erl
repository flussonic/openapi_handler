-module(openapi_schema_SUITE).
-compile([nowarn_export_all, export_all]).

all() ->
  [
    {group, process},
    {group, introspection}
  ].

groups() ->
  [
    {process, [], [ % parallel
      read_default,
      extra_keys_error,
      extra_keys_drop,
      null_in_array,
      nullable_by_oneof,
      boolean_vs_string,
      discriminator,
      discriminator_default_missing_fields,
      discriminator_default_wrong_format,
      recursive_discriminator,
      non_object_validate,
      regexp_pattern,
      external_validators,
      min_max_length,
      max_items_array,
      min_items_array,
      max_items_object,
      min_items_object,
      required_keys,
      required_keys_filter,
      validate_scalar_as_object,
      check_explain,
      check_explain_on_error,
      one_of_integer_const,
      one_of_const_default,
      drop_unidirectional_keys,
      filter_read_only_props
    ]},
    {introspection, [], [
      fetch_type,
      discriminator_effective_schema
    ]}
  ].


init_per_suite(Config) ->
  SchemaPath = code:lib_dir(openapi_handler) ++ "/test/flussonic-230127.json",
  openapi_handler:load_schema(SchemaPath, test_openapi),
  BigOpenapiPath = code:lib_dir(openapi_handler) ++ "/test/redocly-big-openapi.json",
  openapi_handler:load_schema(BigOpenapiPath, big_openapi),
  ExamplePath = code:lib_dir(openapi_handler) ++ "/test/example-openapi.json",
  openapi_handler:load_schema(ExamplePath, example_openapi),
  Config.

end_per_suite(Config) ->
  Config.



read_default(_) ->
  Json = #{<<"name">> => <<"read_default">>},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  #{static := true, name := <<"read_default">>} = 
    openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true}),

  Stream2 = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema}),
  Stream2 = #{name => <<"read_default">>},
  ok.


extra_keys_error(_) ->
  Json = #{<<"name">> => <<"read_default">>, extra_key1 => <<"abc">>, <<"extrakey2">> => def},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  {error,#{
    encoded :=#{
      inputs := [],
      name := <<"read_default">>,
      static := true},
      extra_keys := [extra_key1,<<"extrakey2">>]}} =
        openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true, extra_obj_key => error}),
  ok.


extra_keys_drop(_) ->
  Json = #{<<"name">> => <<"read_default">>, extra_key1 => <<"abc">>, <<"extrakey2">> => def},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  #{inputs := [],name := <<"read_default">>,static := true} = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true}),
  ok.


null_in_array(_) ->
  % When items are not nullable, passing null|undefined as a list element should return an error
  {error, #{error := null_in_array_of_non_nullable}} = openapi_schema:process(
                 [<<"a">>, undefined, <<"b">>],
                 #{schema => #{type => <<"array">>,items => #{type => <<"string">>}}}),
  {error, #{error := null_in_array_of_non_nullable}} = openapi_schema:process(
                 [<<"a">>, null, <<"b">>],
                 #{schema => #{type => <<"array">>,items => #{type => <<"string">>}}}),
  % When array items are nullable, both null and undefined are transformed with no error
  [<<"a">>, undefined, undefined, <<"b">>] = openapi_schema:process(
                 [<<"a">>, null, undefined, <<"b">>],
                 #{schema => #{type => <<"array">>,items => #{type => <<"string">>, nullable => true}}}),
  ok.


nullable_by_oneof(_) ->
  % OAS 3.1 supports all JSON types https://spec.openapis.org/oas/v3.1.0.html#data-types
  % Also 'nullable' is invalid in OAS 3.1, and oneOf with {type: 'null'} is suggested instead
  Props = #{nk => #{oneOf => [#{type => <<"string">>}, #{type => <<"null">>}]}, k2 => #{type => <<"integer">>, default => 42}},
  Schema = #{type => <<"object">>, properties => Props},
  % There was a bug where null value caused extra_keys error
  Expect1 = #{nk => undefined},
  Expect1 = openapi_schema:process(#{nk => null}, #{schema => Schema, extra_obj_key => error}),
  Expect1s = #{nk => <<"hello">>},
  Expect1s = openapi_schema:process(#{nk => <<"hello">>}, #{schema => Schema, extra_obj_key => error}),
  {error, _} = openapi_schema:process(#{nk => 42}, #{schema => Schema, extra_obj_key => error}),
  % Normalize the given object with a nulled key as much as possible
  Expect2 = #{nk => undefined, k2 => 42},
  Expect2 = openapi_schema:process(#{nk => null}, #{schema => Schema, extra_obj_key => error, apply_defaults => true}),
  ok.

boolean_vs_string(_) ->
  % Even when auto_convert is enabled (default behaviour) consider boolean values not valid strings
  {error, _} = openapi_schema:process(true, #{schema => #{type => <<"string">>}}),
  {error, _} = openapi_schema:process(false, #{schema => #{type => <<"string">>}}),
  ok.

discriminator(_) ->
  FooProp = #{dis => #{type => <<"string">>}, k1 => #{type => <<"integer">>}, k3 => #{type => <<"integer">>}},
  BarProp = #{dis => #{type => <<"string">>}, k2 => #{type => <<"integer">>}, k3 => #{type => <<"integer">>}},
  FooType = #{type => <<"object">>, properties => FooProp},
  BarType = #{type => <<"object">>, properties => BarProp},
  DType = #{
    oneOf => [#{'$ref' => <<"#/components/schemas/foo_t">>}, #{'$ref' => <<"#/components/schemas/bar_t">>}],
    discriminator => #{propertyName => <<"dis">>, mapping => #{foo => <<"#/components/schemas/foo_t">>, bar => <<"#/components/schemas/bar_t">>}}},
  DSchema = #{components => #{schemas => #{discr_t => DType, foo_t => FooType, bar_t => BarType}}},

  %% Match type by discriminator
  Foo1 = openapi_schema:process(#{dis => <<"foo">>, k1 => 12, k2 => 34}, #{type => discr_t, whole_schema => DSchema}),
  [dis, k1] = lists:sort(maps:keys(Foo1)), % k2 is deleted because it is valid only for bar_t
  Foo2 = openapi_schema:process(#{<<"dis">> => <<"foo">>, <<"k1">> => 12, <<"k2">> => 34}, #{type => discr_t, whole_schema => DSchema}),
  [dis, k1] = lists:sort(maps:keys(Foo2)), % binary keys work well too
  Foo3 = openapi_schema:process(#{dis => foo, k1 => 12, k2 => 34}, #{type => discr_t, whole_schema => DSchema}),
  [dis, k1] = lists:sort(maps:keys(Foo3)), % atom discriminator value works well
  Bar1 = openapi_schema:process(#{dis => <<"bar">>, k1 => 12, k2 => 34}, #{type => discr_t, whole_schema => DSchema}),
  [dis, k2] = lists:sort(maps:keys(Bar1)), % k1 is deleted because it is valid only for foo_t

  %% Missing or invalid discriminator should lead an error
  {error, #{error := discriminator_missing}} = openapi_schema:process(#{k1 => 12, k2 => 34}, #{type => discr_t, whole_schema => DSchema}),
  {error, #{error := discriminator_unmapped}} = openapi_schema:process(#{dis => <<"nonsense">>, k1 => 12, k2 => 34}, #{type => discr_t, whole_schema => DSchema}),

  %% In case of query check do not fall on discriminator issues
  #{} = openapi_schema:process(#{k1 => 12, k2 => 34}, #{type => discr_t, query => true, whole_schema => DSchema}),
  #{} = openapi_schema:process(#{dis => <<"nonsense">>, k1 => 12, k2 => 34}, #{type => discr_t, query => true, whole_schema => DSchema}),

  %% Missing discriminator allowed for patch requests
  #{k1 := 12} = openapi_schema:process(#{k1 => 12, k2 => 34}, #{type => discr_t, whole_schema => DSchema, patch => true}),

  FooType1 = FooType#{properties := FooProp#{k4 => #{type => <<"integer">>}, dis => #{type => <<"string">>, default => foo}}},
  BarType1 = BarType#{properties := BarProp#{k5 => #{type => <<"integer">>}, dis => #{type => <<"string">>, default => foo}}},
  DSchema1 = #{components => #{schemas => #{discr_t => DType, foo_t => FooType1, bar_t => BarType1}}},

  Foo5 = openapi_schema:process(#{k1 => 12, k2 => 34, k4 => 56}, #{type => discr_t, whole_schema => DSchema1}),
  [k1, k4] = lists:sort(maps:keys(Foo5)), % apply default

  {error, #{error := not_string}} = openapi_schema:process(#{dis => 42}, #{type => discr_t, query => true, whole_schema => DSchema}),

  ok.

% See example without oneOf at https://spec.openapis.org/oas/v3.1.0.html#discriminator-object
% The same happens in redocly-petstore.yaml:
%   Pet:
%     petType: {type: string}
%     discriminator: {propertyName: petType, mapping: {...}}
%   Cat:
%     allOf: [Pet, {type: object, ...}]
%
% > To avoid redundancy, the discriminator MAY be added to a parent schema definition,
%   and all schemas comprising the parent schema in an allOf construct may be used as an alternate schema.
% So, discriminator is handled at top-level component, and later must be ignored
%
% Also the same example shows how oneOf can be omitted.
% openapi_schema has a stricter behaviour, not supporting implicit mapping
recursive_discriminator(_) ->
  % risk of unterminated recursion, thus timetrap
  ct:timetrap(500),

  FooProp = #{k1 => #{type => <<"integer">>, default => 101}},
  BarProp = #{k2 => #{type => <<"integer">>, default => 22}},
  FooOwn = #{type => <<"object">>, properties => FooProp},
  BarOwn = #{type => <<"object">>, properties => BarProp},
  FooType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, FooOwn]},
  BarType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, BarOwn]},
  Mapping = #{foo => <<"#/components/schemas/foo_t">>, bar => <<"#/components/schemas/bar_t">>},

  DSchemaForDis = fun(DisSchema) ->
    BaseProp = #{k3 => #{type => <<"integer">>}, dis => DisSchema},
    DType = #{type => <<"object">>, properties => BaseProp, discriminator => #{propertyName => <<"dis">>, mapping => Mapping}},
    #{components => #{schemas => #{discr_t => DType, foo_t => FooType, bar_t => BarType}}}
  end,

  % Easy case: discriminator is defined as oneOf(const) and passed explicitly
  DSchema1 = DSchemaForDis(#{oneOf => [#{const => <<"foo">>}, #{const => <<"bar">>}]}),
  #{dis := foo, k1 := 101} = openapi_schema:process(#{dis => <<"foo">>}, #{type => discr_t, whole_schema => DSchema1, apply_defaults => true}),
  #{dis := bar, k2 := 22} = openapi_schema:process(#{dis => <<"bar">>}, #{type => discr_t, whole_schema => DSchema1, apply_defaults => true}),

  % Harder case: discriminator is just type: string
  DSchema2 = DSchemaForDis(#{type => <<"string">>}),
  #{dis := foo, k1 := 101} = openapi_schema:process(#{dis => <<"foo">>}, #{type => discr_t, whole_schema => DSchema2, apply_defaults => true}),
  #{dis := bar, k2 := 22} = openapi_schema:process(#{dis => <<"bar">>}, #{type => discr_t, whole_schema => DSchema2, apply_defaults => true}),

  % Hard case: discriminator has default value
  DSchema3a = DSchemaForDis(#{type => <<"string">>, default => <<"foo">>}),
  #{dis := foo, k1 := 101} = openapi_schema:process(#{}, #{type => discr_t, whole_schema => DSchema3a, apply_defaults => true}),
  DSchema3b = DSchemaForDis(#{type => <<"string">>, default => <<"bar">>}),
  #{dis := bar, k2 := 22} = openapi_schema:process(#{}, #{type => discr_t, whole_schema => DSchema3b, apply_defaults => true}),

  {error, #{error := not_string}} = openapi_schema:process(#{dis => 42}, #{type => discr_t, whole_schema => DSchema3b, apply_defaults => true}),

  [] = maps:keys(openapi_schema:process(#{}, #{type => discr_t, whole_schema => DSchema3b})),
  [k2, k3] = maps:keys(openapi_schema:process(#{k2 => 11, k3 => 435}, #{type => discr_t, whole_schema => DSchema3b})),
  % k1 from foo_t is handled like dis=bar
  [] = maps:keys(openapi_schema:process(#{k1 => 43}, #{type => discr_t, whole_schema => DSchema3b})),
  {error, #{extra_keys := [k1]}} = openapi_schema:process(#{k1 => 43}, #{type => discr_t, whole_schema => DSchema3b, extra_obj_key => error}),

  ok.

discriminator_default_missing_fields(_) ->
  FooProp = #{dis => #{type => <<"string">>}, k1 => #{type => <<"integer">>}, k3 => #{type => <<"integer">>}},
  BarProp = #{dis => #{type => <<"string">>}, k2 => #{type => <<"integer">>}, k3 => #{type => <<"integer">>}},
  FooOwn = #{type => <<"object">>, properties => FooProp, required => [<<"k3">>]},
  BarOwn = #{type => <<"object">>, properties => BarProp, required => [<<"k2">>, <<"k3">>]},
  FooType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, FooOwn]},
  BarType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, BarOwn]},
  Mapping = #{foo => <<"#/components/schemas/foo_t">>, bar => <<"#/components/schemas/bar_t">>},
  DSchemaForDis = fun(DisSchema) ->
    BaseProp = #{dis => DisSchema},
    DType = #{type => <<"object">>, properties => BaseProp, discriminator => #{propertyName => <<"dis">>, mapping => Mapping}},
    #{components => #{schemas => #{discr_t => DType, foo_t => FooType, bar_t => BarType}}}
  end,
  DSchema_foo = DSchemaForDis(#{type => <<"string">>, default => <<"foo">>}),
  {error, #{missing_required := [<<"k3">>]}} = openapi_schema:process(#{}, #{type => discr_t, whole_schema => DSchema_foo, required_obj_keys => error}),
  DSchema_bar = DSchemaForDis(#{type => <<"string">>, default => <<"bar">>}),
  {error, #{missing_required := [<<"k2">>]}} = openapi_schema:process(#{k3 => 1}, #{type => discr_t, whole_schema => DSchema_bar, required_obj_keys => error}),
  ok.


discriminator_default_wrong_format(_) ->
  FooProp = #{dis => #{type => <<"string">>}, k1 => #{type => <<"integer">>}, k3 => #{type => <<"integer">>}},
  BarProp = #{dis => #{type => <<"string">>}, k2 => #{type => <<"integer">>}, k3 => #{type => <<"integer">>}},
  FooOwn = #{type => <<"object">>, properties => FooProp, required => [<<"k1">>, <<"k3">>]},
  BarOwn = #{type => <<"object">>, properties => BarProp, required => [<<"k2">>, <<"k3">>]},
  FooType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, FooOwn]},
  BarType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, BarOwn]},
  Mapping = #{foo => <<"#/components/schemas/foo_t">>, bar => <<"#/components/schemas/bar_t">>},
  DSchemaForDis = fun(DisSchema) ->
    BaseProp = #{dis => DisSchema},
    DType = #{type => <<"object">>, properties => BaseProp, discriminator => #{propertyName => <<"dis">>, mapping => Mapping}},
    #{components => #{schemas => #{discr_t => DType, foo_t => FooType, bar_t => BarType}}}
  end,
  DSchema_foo = DSchemaForDis(#{type => <<"string">>, default => <<"foo">>}),
  {error, #{error := not_integer}} = openapi_schema:process(#{k3 => [<<"BAD">>]}, #{type => discr_t, whole_schema => DSchema_foo, required_obj_keys => error}),
  DSchema_bar = DSchemaForDis(#{type => <<"string">>, default => <<"bar">>}),
  {error, #{error := not_integer}} = openapi_schema:process(#{k3 => [<<"BAD">>]}, #{type => discr_t, whole_schema => DSchema_bar, required_obj_keys => error}),
  ok.


non_object_validate(_) ->
  {error, #{error := not_object}} = openapi_schema:process([<<"123">>], #{schema => #{type => <<"object">>}}),
  ok.

regexp_pattern(_) ->

  {error, #{error := nomatch_pattern}} = openapi_schema:process(<<"123">>, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  % {error, #{error := not_string}} = openapi_schema:process(abc, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  <<"abc">> = openapi_schema:process(abc, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  <<"abc">> =  openapi_schema:process(<<"abc">>, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  <<"abc">> =  openapi_schema:process(abc, #{schema => #{type => <<"string">>}}),

  % pattern in loaded schema works well too (regexp is pre-compiled on load)
  #{url := <<"http://foobar/">>} = openapi_schema:process(
    #{name => <<"aaa">>, url => <<"http://foobar/">>}, #{name => test_openapi, type => event_sink_config}),
  {error, #{error := nomatch_pattern}} = openapi_schema:process(
    #{name => <<"aaa">>, url => <<"nonsense://foobar/">>}, #{name => test_openapi, type => event_sink_config}),

  ok.


no_space_validator(<<" ", _/binary>>) ->
  {error, #{detail => leading_space}};
no_space_validator(Input) ->
  {ok, binary:replace(Input, <<" ">>, <<"_">>, [global])}.

external_validators(_) ->
  Schema = #{type => <<"string">>, format => no_space},
  Validators = #{no_space => fun no_space_validator/1},
  % Baseline
  <<" ab cd">> = openapi_schema:process(<<" ab cd">>, #{schema => Schema}),
  % auto_convert by default
  <<"ab_cd">> = openapi_schema:process(<<"ab cd">>, #{schema => Schema, validators => Validators}),
  % Disabled auto_convert -- error instead of converted value
  {error, Err1} = openapi_schema:process(<<"ab cd">>, #{schema => Schema, validators => Validators, auto_convert => false}),
  #{error := needs_convertation, format := no_space} = Err1,
  % Proper value passes validation even witht auto_convert disabled
  <<"ab_cd">> = openapi_schema:process(<<"ab_cd">>, #{schema => Schema, validators => Validators, auto_convert => false}),
  % Improper value
  {error, Err2} = openapi_schema:process(<<" ab cd">>, #{schema => Schema, validators => Validators}),
  #{error := wrong_format, format := no_space, detail := leading_space} = Err2,

  % format validators work with loaded schema (to ensure openapi_schema:prepare_type works well)
  % as simple value
  <<"ab_cd">> = openapi_schema:process(<<"ab cd">>, #{name => example_openapi, type => external_validators_simple, validators => Validators}),
  {error, _} = openapi_schema:process(<<" cd">>, #{name => example_openapi, type => external_validators_simple, validators => Validators}),
  % as array item (nested)
  [[<<"ab_cd">>]] = openapi_schema:process([[<<"ab cd">>]], #{name => example_openapi, type => external_validators_array, validators => Validators}),
  {error, _} = openapi_schema:process([[<<" cd">>]], #{name => example_openapi, type => external_validators_array, validators => Validators}),
  % as object key (nested)
  #{in_object := #{prop1 := <<"ab_cd">>}} =
    openapi_schema:process(#{in_object => #{prop1 => <<"ab cd">>}}, #{name => example_openapi, type => external_validators_object, validators => Validators}),
  {error, _} =
    openapi_schema:process(#{in_object => #{prop1 => <<" cd">>}}, #{name => example_openapi, type => external_validators_object, validators => Validators}),

  % format validator and pattern work simultaneously
  Schema2 = #{type => <<"string">>, format => no_space, pattern => <<"^[a-z]+$">>},
  {error, Err3} = openapi_schema:process(<<" ab cd">>, #{schema => Schema2, validators => Validators}),
  #{error := wrong_format} = Err3,
  {error, Err4} = openapi_schema:process(<<"12 cd">>, #{schema => Schema2, validators => Validators}),
  #{error := nomatch_pattern} = Err4,

  ok.


min_max_length(_) ->
  {error, #{error := too_short, detail := 3}} =
    openapi_schema:process(<<"123">>, #{schema => #{type => <<"string">>, minLength => 5}}),
  {error, #{error := too_long, detail := 3}} =
    openapi_schema:process(<<"123">>, #{schema => #{type => <<"string">>, maxLength => 2}}),

  {error, #{error := too_short, detail := 4}} =
    openapi_schema:process(atom, #{schema => #{type => <<"string">>, minLength => 5}}),
  {error, #{error := too_long, detail := 4}} =
    openapi_schema:process(atom, #{schema => #{type => <<"string">>, maxLength => 2}}),

  UTFString = unicode:characters_to_binary([128578,128579]), % 2 code points, but 8 bytes
  {error, #{error := too_short, detail := 2}} =
    openapi_schema:process(UTFString, #{schema => #{type => <<"string">>, minLength => 3}}),
  {error, #{error := too_long, detail := 2}} =
    openapi_schema:process(UTFString, #{schema => #{type => <<"string">>, maxLength => 1}}),
  ok.


max_items_array(_) ->
  {error, #{error := too_many_items}} = openapi_schema:process([1,2,3],
    #{schema => #{type => <<"array">>, maxItems => 2, items => #{type => <<"integer">>}}}),
  ok.

min_items_array(_) ->
  {error, #{error := too_few_items}} = openapi_schema:process([1],
    #{schema => #{type => <<"array">>, minItems => 2, items => #{type => <<"integer">>}}}),
  ok.

max_items_object(_) ->
  {error, #{error := too_many_items}} = openapi_schema:process(#{a => 1,b => 2, c => 3}, 
    #{schema => #{type => <<"object">>, maxItems => 2, additionalProperties => #{type => integer}}}),
  ok.

min_items_object(_) ->
  {error, #{error := too_few_items}} = openapi_schema:process(#{a => 1},
    #{schema => #{type => <<"object">>, minItems => 2, additionalProperties => #{type => integer}}}),
  ok.

required_keys(_) ->
  Json = #{<<"no_name">> => <<"read_default">>},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  {error, #{missing_required := [<<"name">>]}} = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true, required_obj_keys => error}),
  ok.


required_keys_filter(_) ->
  Schema = #{
    type => <<"object">>,
    properties => #{
      p1 => #{type => <<"integer">>},
      p2 => #{type => <<"integer">>},
      p3 => #{type => <<"integer">>, readOnly => true},
      p4 => #{type => <<"integer">>, readOnly => true},
      p5 => #{type => <<"integer">>, writeOnly => true},
      p6 => #{type => <<"integer">>, writeOnly => true}},
    required => [<<"p1">>, <<"p2">>, <<"p3">>, <<"p4">>, <<"p5">>, <<"p6">>]},
  Obj = #{p1 => 1, p3 => 3, <<"p5">> => 5},
  Opts = #{schema => Schema, required_obj_keys => error},
  % required keys with readOnly=true are ignored in requests
  {error, #{missing_required := [<<"p2">>, <<"p6">>]}} = openapi_schema:process(Obj, Opts#{access_type => write}),
  % required keys with writeOnly=true are ignored in responses
  {error, #{missing_required := [<<"p2">>, <<"p4">>]}} = openapi_schema:process(Obj, Opts#{access_type => read}),
  % default access_type=read
  {error, #{missing_required := [<<"p2">>, <<"p4">>]}} = openapi_schema:process(Obj, Opts),
  ok.


validate_scalar_as_object(_) ->
  Json = #{<<"inputs">> => [<<"udp://239.0.0.1">>]},
  Opts = #{type => stream_config, whole_schema => persistent_term:get({openapi_handler_schema,test_openapi})},
  {error, #{error := not_object, path := _}} = openapi_schema:process(Json, Opts),
  ok.

check_explain(_) ->
  Json = #{<<"name">> => <<"read_default">>, extra_key1 => <<"abc">>, <<"extrakey2">> => def, inputs => [#{}, #{}], cluster_ingest => #{}},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  #{name := <<"read_default">>,
    cluster_ingest := #{'$explain' := #{required := []}},
    inputs := [
      #{'$explain' := #{required := [<<"url">>]}},
      #{'$explain' := #{required := [<<"url">>]}}],
    '$explain' := #{required := [<<"name">>]}} = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true, explain => [required]}),
  % The same knowledge may be obtained by effective_schema explain as well
  #{inputs := [#{'$explain' := #{effective_schema := EffISchema}}|_], '$explain' := #{effective_schema := EffSchema}} =
    openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true, explain => [effective_schema]}),
  #{required := [<<"url">>]} = maps:with([required], EffISchema),
  #{required := [<<"name">>]} = maps:with([required], EffSchema),
  ok.

check_explain_on_error(_) ->
  Json = #{<<"name">> => <<"read_default">>, <<"position">> => true, extra_key1 => <<"abc">>, <<"extrakey2">> => def, inputs => [#{}, #{}], cluster_ingest => #{}},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  {error, #{
    error := not_integer, input := true, path := [<<"stream_config">>, 0, <<"stream_config_specific">>, position]}
  } = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true, explain => [required]}),
  ok.

one_of_integer_const(_) ->
  Json = #{<<"name">> => <<"one_of_integer_const">>, <<"inputs">> => [#{<<"apts">> => 1}, #{<<"apts">> => <<"3">>}, #{<<"apts">> => <<"video">>}]},
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  #{
    name := <<"one_of_integer_const">>,
    inputs := [#{apts := 1}, #{apts := 3}, #{apts := video}]
  } = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, apply_defaults => true, explain => [required]}),
  ok.

one_of_const_default(_) ->
  % When type is oneof(const), default should be returned as atom
  FooProp = #{k1 => #{oneOf => [#{const => <<"hello">>}, #{const => <<"world">>}], default => <<"world">>}},
  FooType = #{type => <<"object">>, properties => FooProp},
  #{k1 := world} = openapi_schema:process(
        #{},
        #{schema => FooType, apply_defaults => true}),
  ok.


drop_unidirectional_keys(_) ->
  Schema = #{
    type => <<"object">>,
    properties => #{
      pn => #{type => <<"integer">>},
      pr => #{type => <<"integer">>, readOnly => true},
      pw => #{type => <<"integer">>, writeOnly => true}
    }
  },
  % 'raw' access does not involve any read/write filtering logic
  ObjRaw = openapi_schema:process(#{pn => 1, pr => 2, pw => 3}, #{schema => Schema, extra_obj_key => drop, access_type => raw}),
  [pn, pr, pw] = lists:sort(maps:keys(ObjRaw)),
  % 'write' access drops readOnly properties
  ObjW = openapi_schema:process(#{pn => 1, pr => 2, pw => 3}, #{schema => Schema, extra_obj_key => drop, access_type => write}),
  [pn, pw] = lists:sort(maps:keys(ObjW)),
  % 'read' access drops writeOnly properties
  ObjR = openapi_schema:process(#{pn => 1, pr => 2, pw => 3}, #{schema => Schema, extra_obj_key => drop, access_type => read}),
  [pn, pr] = lists:sort(maps:keys(ObjR)),
  ok.

filter_read_only_props(_) ->
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  Json = #{
    <<"name">> => <<"stream">>,
    <<"stats">> => #{<<"id">> => <<"61893ba6-07b3-431b-b2f7-716ac1643953">>}},

  #{name := <<"stream">>} = Spec = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, access_type => write}),
  false = maps:is_key(spec,Spec),
  #{name := <<"stream">>, stats := #{id := <<"61893ba6-07b3-431b-b2f7-716ac1643953">>}} = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema}),
  ok.



fetch_type(_) ->
  #{allOf := _} = openapi_schema:type(test_openapi, stream_config),
  ok.


% The case being checked here:
%  * business logic relies on extended attributes in schema
%  * type of processed object contains allOf or discriminator
%  * openapi_schema:type is not enough because it does not expand allOf and cannot choose discriminated type without actual data
%  * `explain => [effective_schema]` option makes openapi_schema:process return the resulting schema for processed object
% This way the calling code can access object or property attributes to perform extra actions
discriminator_effective_schema(_) ->
  % risk of unterminated recursion, thus timetrap
  ct:timetrap(500),

  FooProp = #{k1 => #{type => <<"integer">>, default => 101, 'x-attr1' => <<"val1">>}},
  BarProp = #{k2 => #{type => <<"integer">>, default => 22,  'x-attr2' => true}},
  FooOwn = #{type => <<"object">>, properties => FooProp, 'x-objAttr1' => [<<"k1">>]},
  BarOwn = #{type => <<"object">>, properties => BarProp, 'x-objAttr2' => [2]},
  FooType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, FooOwn]},
  BarType = #{allOf => [#{'$ref' => <<"#/components/schemas/discr_t">>}, BarOwn]},
  Mapping = #{foo => <<"#/components/schemas/foo_t">>, bar => <<"#/components/schemas/bar_t">>},

  DSchemaForDis = fun(DisSchema) ->
    BaseProp = #{k3 => #{type => <<"integer">>}, dis => DisSchema},
    DType = #{type => <<"object">>, properties => BaseProp, 'x-objAttr1' => [<<"k3">>],
      discriminator => #{propertyName => <<"dis">>, mapping => Mapping}},
    #{components => #{schemas => #{discr_t => DType, foo_t => FooType, bar_t => BarType}}}
  end,

  % Easy case: discriminator is defined as oneOf(const) and passed explicitly
  DSchema1 = DSchemaForDis(#{oneOf => [#{const => <<"foo">>}, #{const => <<"bar">>}]}),
  #{dis := foo, k1 := 101, '$explain' := Explain1foo} = openapi_schema:process(#{dis => <<"foo">>},
    #{type => discr_t, whole_schema => DSchema1, apply_defaults => true, explain => [effective_schema]}),
  #{dis := bar, k2 := 22, '$explain' := Explain1bar} = openapi_schema:process(#{dis => <<"bar">>},
    #{type => discr_t, whole_schema => DSchema1, apply_defaults => true, explain => [effective_schema]}),

  #{effective_schema := #{properties := Eff1fooProps} = Eff1foo} = Explain1foo,
  [<<"k1">>,<<"k3">>] = lists:sort(maps:get('x-objAttr1', Eff1foo)),
  [dis, k1, k3] = lists:sort(maps:keys(Eff1fooProps)),
  #{k1 := #{'x-attr1' := <<"val1">>}} = Eff1fooProps,

  #{effective_schema := #{properties := Eff1barProps} = Eff1bar} = Explain1bar,
  [<<"k3">>] = maps:get('x-objAttr1', Eff1bar),
  [2] = maps:get('x-objAttr2', Eff1bar),
  [dis, k2, k3] = lists:sort(maps:keys(Eff1barProps)),
  #{k2 := #{'x-attr2' := true}} = Eff1barProps,
  ok.
