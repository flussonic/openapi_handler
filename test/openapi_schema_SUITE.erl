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
      extra_keys_request,
      extra_keys_not_request,
      null_in_array
  ]}
  ].


init_per_suite(Config) ->
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


extra_keys_request(_) ->
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


extra_keys_not_request(_) ->
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

