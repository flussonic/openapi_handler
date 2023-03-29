-module(openapi_handler_SUITE).
-compile([nowarn_export_all, export_all]).


all() ->
  [{group, routes}, {group, handling}].

groups() ->
  [
   {routes, [yaml_routes, json_routes]},
   {handling, [
     trivial,
     non_existing_api,
     not_implemented,
     path_parameters,
     json_body_parameters,
     query_string_parameters,
     multiple_file_upload,
     json_array_error,
     json_array_ok,
     undefined_in_non_nullable,
     erase_value_with_null,
     error_response,
     done_request,
     non_exist_key
   ]}
  ].

-ifdef(legacy).
start_http(Routes, ApiName) ->
  cowboy:start_http(ApiName, 1, [{port, 0}],
    [{env, [{dispatch, cowboy_router:compile([{'_', Routes}])}]}]).
-else.
start_http(Routes, ApiName) ->
  cowboy:start_clear(ApiName, [{port, 0}],
    #{env => #{dispatch => cowboy_router:compile([{'_', Routes}])}}).
-endif.


init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _} = application:ensure_all_started(lhttpc),

  PetstorePath = filename:join(code:lib_dir(openapi_handler, test),"redocly-petstore.yaml"),
  TestSchemaPath = filename:join(code:lib_dir(openapi_handler, test),"test_schema.yaml"),
  PetstoreRoutes = openapi_handler:routes(#{
    schema => PetstorePath,
    prefix => <<"/test/yml">>,
    name => petstore_server_api,
    module => fake_petstore
  }),
  TestSchemaRoutes = openapi_handler:routes(#{
    schema => TestSchemaPath,
    prefix => <<"/test/yml">>,
    name => test_schema_api,
    module => test_schema_res
  }),
  {ok, _} = application:ensure_all_started(cowboy),
  start_http(PetstoreRoutes, petstore_api_server),
  start_http(TestSchemaRoutes, test_schema_server),
  PetstorePort = ranch:get_port(petstore_api_server),
  TestSchemaPort = ranch:get_port(test_schema_server),

  PetstoreApi = openapi_client:load(#{
    schema_url => PetstorePath,
    url => <<"http://127.0.0.1:",(integer_to_binary(PetstorePort))/binary,"/test/yml">>
  }),
  TestSchemaApi = openapi_client:load(#{
    schema_url => TestSchemaPath,
    url => <<"http://127.0.0.1:",(integer_to_binary(TestSchemaPort))/binary,"/test/yml">>
  }),
  openapi_client:store(petstore_api, PetstoreApi),
  openapi_client:store(test_schema_api, TestSchemaApi),
  Config.

end_per_suite(Config) ->
  Config.

% From Cowboy documentation:
%  > Finally, each path contains matching rules for the path along with optional constraints,
%  > and gives us the handler module to be used along with its initial state.
%  >
%  > Path1 = {PathMatch, Handler, InitialState}.
%
% Here, in routes group we ensure openapi_handler:routes/1 returns a valid list
% of Cowboy matching rules in proper order (longer path first)
yaml_routes(_) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/redocly-petstore.yaml",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => petstore_yaml, prefix => <<"/test/yml">>}),
  [{<<"/test/yml/user/logout">>,_,{petstore_yaml,<<"/user/logout">>}},
   _, _, _,
   {<<"/test/yml/user/:username">>,_,{petstore_yaml,<<"/user/:username">>}},
   {<<"/test/yml/user">>,_,{petstore_yaml,<<"/user">>}}
   |_] = Routes,
  ok.

json_routes(_) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/redocly-big-openapi.json",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => rebilly_json, prefix => <<"/test/json">>}),
  [{<<"/test/json/websites/:id/webhook">>,_, {rebilly_json,<<"/websites/:id/webhook">>}},
   {<<"/test/json/websites/:id">>,_, {rebilly_json,<<"/websites/:id">>}},
   {<<"/test/json/websites">>,_, {rebilly_json,<<"/websites">>}},
   {<<"/test/json/webhooks/:id">>,_, {rebilly_json,<<"/webhooks/:id">>}},
   {<<"/test/json/webhooks">>,_, {rebilly_json,<<"/webhooks">>}}
   |_] = Routes,
  ok.




authorize(_) -> #{auth => yes_please}.
postprocess(JSON, _) -> JSON.


trivial(_) ->
  % Here we get error with code 200 because no response is described and we cannot
  % reliably tell user that server response is just an undescribed something
  {error, {200,#{say := <<"goodbye">>}}} = openapi_client:call(petstore_api,logoutUser, #{}),
  ok.

non_existing_api(_) ->
  {error, not_loaded} = openapi_client:call(non_existing_api,some_method,#{}).

not_implemented(_) ->
  {error, {501, #{error := <<"not_implemented">>}}} =
    openapi_client:call(petstore_api, createUsersWithArrayInput, #{}),
  ok.


path_parameters(_) ->
  #{id := 238457234857} = openapi_client:call(petstore_api,getUserByName,#{username => <<"Jack">>}),
  ok.


undefined_in_non_nullable(_) ->
  User = #{id := 2384572} = openapi_client:call(petstore_api,getUserByName,#{username => <<"John">>}),
  [id,username] = lists:sort(maps:keys(User)),
  ok.


erase_value_with_null(_) ->
  Args = #{username => <<"Mary">>, json_body => #{firstName => null}},
  User = #{id := 15} = openapi_client:call(petstore_api,updateUser,Args),
  [id,username] = lists:sort(maps:keys(User)),
  ok.


non_exist_key(_) ->
  Args = #{username => <<"Mary">>, json_body => #{firstName => <<"Anna">>, non_exist => some}},
  {error,{400,Res}} = openapi_client:call(petstore_api,updateUser,Args),

  #{<<"extra_keys">> := [<<"non_exist">>],
    <<"input1">> := #{<<"firstName">> := <<"Anna">>,<<"non_exist">> := <<"some">>},
    <<"name">> := <<"request_body">>,
    <<"while">> := <<"parsing_parameters">>} = jsx:decode(Res),

  ok.


json_body_parameters(_) ->
  Order0 = #{id => 1000011, petId => 7214, shipDate => <<"20221103-221700">>, status => placed, requestId => <<"testrequestid">>},
  #{quantity := 31} =
    openapi_client:call(petstore_api, placeOrder, #{json_body => Order0}),
  ok.

query_string_parameters(_) ->
  [#{name := <<"Dingo">>}] =
    openapi_client:call(petstore_api, findPetsByStatus, #{status => [pending,sold]}),
  ok.


fake_request(Name, Method, Path, Extra) ->
  StreamId = {Method, Path},
  Headers = case maps:get(body, Extra, <<>>) of
    <<_, _/binary>> -> #{<<"content-type">> => <<"application/json">>, <<"accept">> => <<"application/json">>};
    _ -> #{<<"accept">> => <<"application/json">>}
  end,
  Req1 = Extra#{method => Method, headers => Headers, streamid => StreamId, tester => self()},
  erase(body_read),
  {_ok, Req2, ApiRequest} = openapi_handler:do_init(Req1, Name, Path, ?MODULE, #{no_handle => true}),
  is_map(ApiRequest) andalso openapi_handler:do_handle(Req2, ApiRequest, ?MODULE),
  receive
    {StreamId, _, Response} ->
      ct:print("Resp ~120p", [Response]),
      Response
  after
    0 -> {error, no_response}
  end.


%% Cowboy mock. Provides Cowboy 2.9 API for tests
method(Req) -> maps:get(method, Req, <<"GET">>).
peer(Req) -> maps:get(peer, Req, {{10,62,13,5},4824}).
qs(Req) -> maps:get(qs, Req, <<>>).
parse_qs(Req) -> cow_qs:parse_qs(qs(Req)).
bindings(Req) -> maps:get(bindings, Req, #{}).
headers(Req) -> maps:get(headers, Req, #{}).
header(Name, Req) -> header(Name, Req, undefined).
header(Name, Req, Default) -> maps:get(Name, headers(Req), Default).
parse_header(<<"accept-encoding">>, Req) -> maps:get(accept_encoding, Req, undefined).

read_body(#{streamid := StreamId} = Req) ->
  case get(last_body_read) of
    StreamId -> ct:print("body_read_twice ~0p", [StreamId]), error(body_read_twice);
    _ -> put(last_body_read, StreamId)
  end,
  Body = maps:get(body, Req, <<>>),
  {ok, Body, Req}.

reply(Code, Headers, Body, #{tester := Tester, streamid := StreamId} = Req) ->
  case get(last_response_sent) of
    StreamId -> ct:print("response_sent_twice ~0p", [StreamId]), error(response_sent_twice);
    _ -> put(last_response_sent, StreamId)
  end,
  Tester ! {StreamId, self(), {response, Code, Headers, Body}},
  Req.


read_multipart_files(Req) -> {ok, maps:get(files, Req, []), Req}.



uploadFiles(#{files := Files}) ->
  [{<<"upload_name1.txt">>,<<"11\n">>},{<<"file2.txt">>,<<"22\n">>}] = Files,
  #{}.
multiple_file_upload(_) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/multiple-upload.yaml",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => mu, prefix => <<"/test/mu">>}),
  [{<<"/test/mu/uploadFiles">>, _, {mu, <<"/uploadFiles">>}}] = Routes,

  Req = #{
    files => [{<<"upload_name1.txt">>,<<"11\n">>},{<<"file2.txt">>,<<"22\n">>}]
  },
  {response, 200, _, _Res} = fake_request(mu, <<"POST">>, <<"/uploadFiles">>, Req),
  ok.

json_array_error(_) ->
  Array = <<"1,2,3">>,
  {error,{400, #{error := <<"not_array">> }}} = openapi_client:call(test_schema_api, jsonArray, #{json_body => Array}),
  ok.

json_array_ok(_) ->
  Array = [1,2,3],
  Res = openapi_client:call(test_schema_api, jsonArray, #{json_body => Array}),
  #{<<"json_res">> := <<"1">>} = jsx:decode(Res),
  ok.

putFile(#{req := Req}) ->
  File = #{size => 100},
  {done, Req#{body => jsx:encode(File)}}.

done_request(_) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/done_req.yaml",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => put, prefix => <<"/test/put">>}),
  [{<<"/test/put/putFile">>, _, {put, <<"/putFile">>}}] = Routes,
  _ = fake_request(put, <<"PUT">>, <<"/putFile">>, #{}),
  ok.


error_response(_) ->
  {error, {501,#{error := <<"not_implemented">>}}} =
    openapi_client:call(petstore_api, getInventory, #{}),
  ok.
