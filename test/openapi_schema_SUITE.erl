-module(openapi_schema_SUITE).
-compile([nowarn_export_all, export_all]).

all() ->
  [
    {group, process}
  ].

groups() ->
  [
    {process, [], [ % parallel
      read_default,
      extra_keys_error,
      extra_keys_drop,
      null_in_array,
      discriminator,
      non_object_validate,
      regexp_pattern,
      min_max_length,
      max_items_array,
      max_items_object,
      required_keys,
      required_keys_filter,
      validate_scalar_as_object,
      check_explain,
      check_explain_on_error,
      one_of_integer_const,
      filter_read_only_props
  ]}
  ].


init_per_suite(Config) ->
  SchemaPath = code:lib_dir(openapi_handler) ++ "/test/flussonic-230127.json",
  openapi_handler:load_schema(SchemaPath, test_openapi),
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

  FooType1 = FooType#{properties := FooProp#{k4 => #{type => <<"integer">>}, dis => #{type => <<"string">>, default => foo}}},
  BarType1 = BarType#{properties := BarProp#{k5 => #{type => <<"integer">>}, dis => #{type => <<"string">>, default => foo}}},
  DSchema1 = #{components => #{schemas => #{discr_t => DType, foo_t => FooType1, bar_t => BarType1}}},

  Foo5 = openapi_schema:process(#{k1 => 12, k2 => 34, k4 => 56}, #{type => discr_t, whole_schema => DSchema1}),
  [k1, k4] = lists:sort(maps:keys(Foo5)), % apply default

  ok.


non_object_validate(_) ->
  {error, #{error := not_object}} = openapi_schema:process([<<"123">>], #{schema => #{type => <<"object">>}}),
  ok.

regexp_pattern(_) ->

  {error, #{error := nomatch_pattern}} = openapi_schema:process(<<"123">>, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  % {error, #{error := not_string}} = openapi_schema:process(abc, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  abc = openapi_schema:process(abc, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  <<"abc">> =  openapi_schema:process(<<"abc">>, #{schema => #{type => <<"string">>, pattern => <<"^[a-z]+$">>}}),
  abc =  openapi_schema:process(abc, #{schema => #{type => <<"string">>}}),

  ok.


min_max_length(_) ->
  {error, #{error := too_short}} = 
    openapi_schema:process(<<"123">>, #{schema => #{type => <<"string">>, minLength => 5}}),
  {error, #{error := too_long}} = 
    openapi_schema:process(<<"123">>, #{schema => #{type => <<"string">>, maxLength => 2}}),
  ok.


max_items_array(_) ->
  {error, #{error := too_many_items}} = openapi_schema:process([1,2,3],
    #{schema => #{type => <<"array">>, maxItems => 2, items => #{type => <<"integer">>}}}),
  ok.

max_items_object(_) ->
  {error, #{error := too_many_items}} = openapi_schema:process(#{a => 1,b => 2, c => 3}, 
    #{schema => #{type => <<"object">>, maxItems => 2, additionalProperties => #{type => integer}}}),
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

filter_read_only_props(_) ->
  Schema = persistent_term:get({openapi_handler_schema,test_openapi}),
  Json = #{
    <<"name">> => <<"stream">>,
    <<"stats">> => #{<<"id">> => <<"61893ba6-07b3-431b-b2f7-716ac1643953">>}},

  #{name := <<"stream">>} = Spec = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema, access_type => write}),
  false = maps:is_key(spec,Spec),
  #{name := <<"stream">>, stats := #{id := <<"61893ba6-07b3-431b-b2f7-716ac1643953">>}} = openapi_schema:process(Json, #{type => stream_config, whole_schema => Schema}),
  ok.
