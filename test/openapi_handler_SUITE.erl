-module(openapi_handler_SUITE).
-compile([nowarn_export_all, export_all]).


all() ->
  [{group, routes}, {group, handling}].

groups() ->
  [
   {routes, [json_routes]},
   {handling, [
     trivial,
     non_existing_api,
     not_implemented,
     path_parameters,
     json_body_parameters,
     broken_json,
     query_string_parameters,
     multiple_file_upload,
     json_array_error,
     json_array_ok,
     undefined_in_non_nullable,
     erase_value_with_null,
     error_response,
     done_request,
     autorize_handler_args,
     non_exist_key,
     non_exist_key_drop,
     check_xml_content_responses,
     check_json_content_responses,
     check_text_content_responses,
     check_nonsense_content_responses,
     required_keys_filter,
     select_not_filters_required_keys,
     unavailable_error
   ]}
  ].

start_http(Routes, ApiName) ->
  cowboy:start_clear(ApiName, [{port, 0}],
    #{env => #{dispatch => cowboy_router:compile([{'_', Routes}])}}).


init_per_suite(Config) ->
  inets:start(),
  {ok, _} = application:ensure_all_started(cowboy),

  PetstorePath = filename:join(code:lib_dir(openapi_handler),"test/redocly-petstore.json"),
  TestSchemaPath = filename:join(code:lib_dir(openapi_handler),"test/test_schema.json"),
  PetstoreRoutes = openapi_handler:routes(#{
    schema => PetstorePath,
    prefix => <<"/test/yml">>,
    name => petstore_server_api,
    module => fake_petstore
  }),
  PetstoreWithSchemaOptsRoutes = openapi_handler:routes(#{
    schema => PetstorePath,
    prefix => <<"/test/yml">>,
    name => petstore_with_schema_opts_server_api,
    module => fake_petstore,
    schema_opts => #{extra_obj_key => drop}
  }),
  TestSchemaRoutes = openapi_handler:routes(#{
    schema => TestSchemaPath,
    prefix => <<"/test/yml">>,
    name => test_schema_api,
    module => test_schema_res
  }),
  {ok, _} = application:ensure_all_started(cowboy),
  start_http(PetstoreRoutes, petstore_api_server),
  start_http(PetstoreWithSchemaOptsRoutes, petstore_with_schema_opts_api_server),
  start_http(TestSchemaRoutes, test_schema_server),
  PetstorePort = ranch:get_port(petstore_api_server),
  PetstoreWithSchemaOptsPort = ranch:get_port(petstore_with_schema_opts_api_server),
  TestSchemaPort = ranch:get_port(test_schema_server),

  PetstoreApi = openapi_client:load(#{
    schema_url => PetstorePath,
    url => <<"http://127.0.0.1:",(integer_to_binary(PetstorePort))/binary,"/test/yml">>
  }),
  PetstoreWithSchemaOptsApi = openapi_client:load(#{
    schema_url => PetstorePath,
    url => <<"http://127.0.0.1:",(integer_to_binary(PetstoreWithSchemaOptsPort))/binary,"/test/yml">>
  }),
  TestSchemaApi = openapi_client:load(#{
    schema_url => TestSchemaPath,
    url => <<"http://127.0.0.1:",(integer_to_binary(TestSchemaPort))/binary,"/test/yml">>
  }),
  openapi_client:store(petstore_api, PetstoreApi),
  openapi_client:store(petstore_with_schema_opts_api, PetstoreWithSchemaOptsApi),
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

json_routes(_) ->
  SchemaPath = code:lib_dir(openapi_handler) ++ "/test/redocly-big-openapi.json",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => rebilly_json, prefix => <<"/test/json">>}),
  [{<<"/test/json/websites/:id/webhook">>,_, {rebilly_json,<<"/websites/:id/webhook">>}},
   {<<"/test/json/websites/:id">>,_, {rebilly_json,<<"/websites/:id">>}},
   {<<"/test/json/websites">>,_, {rebilly_json,<<"/websites">>}},
   {<<"/test/json/webhooks/:id">>,_, {rebilly_json,<<"/webhooks/:id">>}},
   {<<"/test/json/webhooks">>,_, {rebilly_json,<<"/webhooks">>}}
   |_] = Routes,
  ok.




authorize(#{'$cowboy_req' := _}) -> #{auth => yes_please};
authorize(_) -> ct:fail("There is no '$cowboy_req' in the authorize request arguments").
postprocess(JSON, _) -> JSON.
log_call(CallInfo) ->
  (whereis(openapi_handler_SUITE_log_call_server) /= undefined) andalso (openapi_handler_SUITE_log_call_server ! {log_call, ?MODULE, CallInfo}).


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
    <<"while">> := <<"parsing_parameters">>} = openapi_json:decode(Res),
  ok.

non_exist_key_drop(_) ->
  Args = #{username => <<"Mary">>, json_body => #{firstName => null, non_exist_key => some}},
  #{id := 15} = openapi_client:call(petstore_with_schema_opts_api,updateUser,Args),

  ok.


json_body_parameters(_) ->
  Order0 = #{id => 1000011, petId => 7214, shipDate => <<"20221103-221700">>, status => placed, requestId => <<"testrequestid">>},
  #{quantity := 31} =
    openapi_client:call(petstore_api, placeOrder, #{json_body => Order0}),
  ok.

broken_json(_) ->
  Port = integer_to_list(ranch:get_port(petstore_api_server)),
  JSON = "{\"key\":\"value\"]}",
  Request = {"http://127.0.0.1:"++Port++"/test/yml/store/order", [], "application/json", JSON},
  {ok, {{_, 400, _}, Headers, Body}} = httpc:request(post, Request, [{timeout, 5000}], [{body_format, binary}]),
  "application/json" = proplists:get_value("content-type", Headers),
  #{<<"error">> := <<"broken_json">>} = openapi_json:decode(Body),
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
resp_header(H, Req) -> resp_header(H, Req, undefined).
resp_header(H, Req, Default) -> maps:get(H, maps:get(resp_headers, Req, #{}), Default).

read_body(#{streamid := StreamId} = Req) ->
  case get(last_body_read) of
    StreamId -> ct:print("body_read_twice ~0p", [StreamId]), error(body_read_twice);
    _ -> put(last_body_read, StreamId)
  end,
  Body = maps:get(body, Req, <<>>),
  {ok, Body, Req}.

reply(Code, Headers, Body, #{tester := Tester, streamid := StreamId} = Req) ->
  % Ensure all headers are lowercase
  NonLowercaseHeaders = maps:filter(fun(K, _) -> string:lowercase(iolist_to_binary(K)) /= K end, Headers),
  ZeroMap = #{},
  ZeroMap = NonLowercaseHeaders,

  case get(last_response_sent) of
    StreamId -> ct:print("response_sent_twice ~0p", [StreamId]), error(response_sent_twice);
    _ -> put(last_response_sent, StreamId)
  end,
  Tester ! {StreamId, self(), {response, Code, Headers, Body}},
  Req#{resp_headers => Headers, resp_body => Body}.


read_multipart_files(Req) -> {ok, maps:get(files, Req, []), Req}.



uploadFiles(#{files := Files}) ->
  [{<<"upload_name1.txt">>,<<"11\n">>},{<<"file2.txt">>,<<"22\n">>}] = Files,
  #{}.
multiple_file_upload(_) ->
  SchemaPath = code:lib_dir(openapi_handler) ++ "/test/multiple-upload.json",
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
  #{<<"json_res">> := <<"1">>} = openapi_json:decode(Res),
  ok.

putFile(#{req := Req, '$cowboy_req' := CowboyReq}) ->
  <<"PUT">> = cowboy_req:method(CowboyReq),
  Body = <<"{\"size\":100}">>,
  Req1 = reply(200, #{<<"content-length">> => byte_size(Body), <<"content-type">> => <<"application/json">>}, Body, Req),
  {done, Req1}.

done_request(_) ->
  register(openapi_handler_SUITE_log_call_server, self()),
  SchemaPath = code:lib_dir(openapi_handler) ++ "/test/done_req.json",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => put, prefix => <<"/test/put">>}),
  [{<<"/test/put/putFile">>, _, {put, <<"/putFile">>}}] = Routes,
  _ = fake_request(put, <<"PUT">>, <<"/putFile">>, #{}),
  receive
    {log_call, ?MODULE, #{operationId := putFile} = CallInfo} ->
      #{content_type := <<"application/json">>, content_length := 12} = CallInfo
  after 1000 -> error(no_log_call)
  end,
  unregister(openapi_handler_SUITE_log_call_server),
  ok.

autorize_handler_args(_) ->
  SchemaPath = code:lib_dir(openapi_handler) ++ "/test/done_req.json",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => aha, prefix => <<"/test/arf">>}),
  [{<<"/test/arf/putFile">>, _, {aha, <<"/putFile">>}}] = Routes,

  {response, 200, _, _Res} = fake_request(aha, <<"PUT">>, <<"/putFile">>, #{}),
  ok.

error_response(_) ->
  {error, {501,#{error := <<"not_implemented">>}}} =
    openapi_client:call(petstore_api, getInventory, #{}),
  ok.


% Methods valid_raw_response_value/2 and valid_simple_response_value/2 are helpfull methods for testing handling responses with preassigned content types.
% Openapi_handler checks compatibility of content type and of 'Accept' value.
valid_raw_response_value(<<"random/nonsense">> = _ContentType, _Accept) ->
  % Content type  for /headersContentType at test_schema.yaml.
  % Error should be generated.
  {error,{500,#{error => <<"invalid_response">>}}};
valid_raw_response_value(<<"application/json">>  = _ContentType, _Accept) ->
  % Content type <<"application/json">> does not suggest binary body.
  % Error should be generated.
  {error,{500,#{error => <<"invalid_response">>}}};
valid_raw_response_value(ContentType, <<"*/*">> = _Accept) ->
  content(ContentType);
valid_raw_response_value(ContentType, <<"random/before-wildcard; */*">> = _Accept) ->
  content(ContentType);
valid_raw_response_value(ContentType, Accept) when ContentType == Accept ->
  content(ContentType);
valid_raw_response_value(_, _) ->
  {error,{500,#{error => <<"invalid_response">>}}}.


% For json simple response
valid_simple_response_value(<<"application/json">> = ContentType, _Accept) ->
  content(ContentType);
% For binary simple response
valid_simple_response_value(_ContentType, <<"application/json">> = _Accept) ->
  {error,{500,#{error => <<"invalid_response">>}}};
valid_simple_response_value(_ContentType, <<"random/nonsense">> = _Accept) ->
  {error,{500,#{error => <<"invalid_response">>}}}; % <<"random/nonsense">> is not described
valid_simple_response_value(_ContentType, <<"random/before-wildcard; */*">> = _Accept) ->
  {error,{500,#{error => <<"invalid_response">>}}};
valid_simple_response_value(ContentType, _Accept) ->
  content(ContentType).



content(ContentType) ->
  maps:get(ContentType, #{<<"text/plain">> => <<"OK">>,
    <<"application/json">> => #{<<"result">> => <<"OK">>},
    <<"application/xml">> => <<"<Message> OK </Message>">>,
    <<"random/nonsense">> => <<"Some">>
  }).


accept_type_list() ->
  [<<"text/plain">>, <<"*/*">>, <<"application/xml">>, <<"application/json">>, <<"random/nonsense">>, <<"random/before-wildcard; */*">>].


get_response(ContentType, Accept) -> do_get_response(#{content_type => ContentType, accept => Accept}).
get_response(simple, ContentType, Accept) -> do_get_response(#{response_view => simple, content_type => ContentType, accept => Accept}).

do_get_response(#{content_type := ContentType} = CallParams) ->
  RespView = maps:get(response_view, CallParams, undefined),
  register(test_schema_log_call_server, self()),
  Result = openapi_client:call(test_schema_api,headersContentType,CallParams),
  receive
    {log_call, test_schema_res, #{code := OK} = CallInfo} when OK >= 200, OK < 300, RespView /= simple ->
      % On successful answer, content should be as callback returned
      #{content_type := ContentType, content_length := CLen} = CallInfo,
      true = (CLen > 0);
    {log_call, test_schema_res, #{code := OK} = CallInfo} when OK >= 200, OK < 300, RespView == simple ->
      #{content_type := _, content_length := CLen} = CallInfo,
      true = (CLen > 0);
    {log_call, test_schema_res, CallInfo} ->
      % Some error. It should have JSON description
      #{content_type := <<"application/json">>, content_length := CLen} = CallInfo,
      true = (CLen > 0)
  after 1000 -> error(no_log_call)
  end,
  unregister(test_schema_log_call_server),
  Result.


check_xml_content_responses(_) ->
  lists:foldl(fun(Accept, _) -> 
    true = valid_raw_response_value(<<"application/xml">>, Accept) == get_response(<<"application/xml">>, Accept),
    true = valid_simple_response_value(<<"application/xml">>, Accept) == get_response(simple, <<"application/xml">>, Accept) end,
  [], accept_type_list()),
  ok.


check_json_content_responses(_) ->
  lists:foldl(fun(Accept, _) ->
    true = valid_raw_response_value(<<"application/json">>, Accept) == get_response(<<"application/json">>, Accept),
    true = valid_simple_response_value(<<"application/json">>, Accept) == get_response(simple, <<"application/json">>, Accept) end,
  [], accept_type_list()),
  ok.


check_text_content_responses(_) ->
  lists:foldl(fun(Accept, _) ->
    true = valid_raw_response_value(<<"text/plain">>, Accept) == get_response(<<"text/plain">>, Accept),
    true = valid_simple_response_value(<<"text/plain">>, Accept) == get_response(simple, <<"text/plain">>, Accept) end,
  [], accept_type_list()),
  ok.


check_nonsense_content_responses(_) ->
  lists:foldl(fun(Accept, _) ->
    true = valid_raw_response_value(<<"random/nonsense">>, Accept) == get_response(<<"random/nonsense">>, Accept),
    true = valid_simple_response_value(<<"random/nonsense">>, Accept) == get_response(simple, <<"random/nonsense">>, Accept) end,
  [], accept_type_list()),
  ok.


required_keys_filter([_| _]) -> {skip, disabled_by_34435};
required_keys_filter(_) ->
  % required properties: p1, p2(r/o=true), p3(w/o=true)
  Res1 = openapi_client:call(test_schema_api, saveRequiredFilter, #{json_body => #{
    % no required properties
  }}),
  {error, {400, #{missing_required := [<<"p1">>, <<"p3">>]}}} = Res1, % p2 filtered out

  Res2 = openapi_client:call(test_schema_api, saveRequiredFilter, #{json_body => #{
    p1 => 1, p2 => 2, p3 => 3 % all required properties provided
  }}),
  #{p1 := 1, p2 := 2, p3 := 3} = Res2,

  Res3 = openapi_client:call(test_schema_api, saveRequiredFilter, #{json_body => #{
    p1 => 1, p3 => 3
  }}),
  {error, {500, #{missing_required := [<<"p2">>]}}} = Res3, % no p2 in response
  ok.


select_not_filters_required_keys(_) ->
  % p3 is 'writeOnly', so it should be dropped in results
  #{elements := [Elem1,Elem1]} = openapi_client:call(test_schema_api, selectCollectionFields,
      #{json_body => #{p1 => 1, p2 => 2, p3 => 3, p4 => 4, p5 => 5}}),
  #{p1 := 1, p2 := 2, p4 := 4, p5 :=5} = Elem1,

  % p1, p2 are required keys, so they are returned despite not explicitly requested
  #{elements := [Elem3,Elem3]} = openapi_client:call(test_schema_api, selectCollectionFields,
      #{json_body => #{p1 => 1, p2 => 2, p3 => 3, p4 => 4, p5 => 5}, select => <<"p4">>}),
  #{p1 := 1, p2 := 2, p4 := 4} = Elem3,
  undefined = maps:get(p3, Elem3, undefined),
  undefined = maps:get(p5, Elem3, undefined),

  ok.


unavailable_error(_) ->
  {error,unavailable} = openapi_client:call(test_schema_api, selectCollectionFields, #{json_body => #{unavailable => true}}),
  ok.

