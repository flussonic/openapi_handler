-module(openapi_handler_SUITE).
-compile([nowarn_export_all, export_all]).


all() ->
  [{group, routes}, {group, handling}].

groups() ->
  [
   {routes, [yaml_routes, json_routes]},
   {handling, [
     trivial,
     path_parameters,
     json_body_parameters,
     query_string_parameters,
     multiple_file_upload,
     undefined_in_non_nullable,
     done_request
   ]}
  ].

init_per_group(_, Config) ->
  Config.

end_per_group(_, Config) ->
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


% Simple callback with no parameters
logoutUser(_) -> #{say => goodbye}.
trivial(_) ->
  {response, 200, _, RespBody} = fake_request(<<"GET">>, <<"/user/logout">>, #{}),
  #{<<"say">> := <<"goodbye">>} = jsx:decode(iolist_to_binary(RespBody)),
  ok.

% A parameter in path. Schema says id is integer, so it is converted even if handler returns it as a binary
getUserByName(#{username := <<"John">>}) -> #{username => <<"John">>, id => <<"2384572">>, pet => undefined};
getUserByName(#{username := <<"Jack">>}) -> #{username => <<"Jack">>, id => <<"238457234857">>}.

path_parameters(_) ->
  {response, 200, _, RespBody} = fake_request(<<"GET">>, <<"/user/:username">>, #{bindings => #{username => <<"Jack">>}}),
  #{<<"id">> := 238457234857} = jsx:decode(iolist_to_binary(RespBody)),
  ok.


undefined_in_non_nullable(_) ->
  {response, 200, _, Json} = fake_request(petstore_yaml, <<"GET">>, <<"/user/:username">>, #{bindings => #{username => <<"John">>}}),
  User = #{<<"id">> := 2384572} = jsx:decode(iolist_to_binary(Json)),
  [<<"id">>,<<"username">>] = lists:sort(maps:keys(User)),
  ok.


% Object in a JSON body
placeOrder(#{json_body := #{petId := 7214, status := placed} = Order}) -> Order#{quantity => 31}.
json_body_parameters(_) ->
  Order0 = #{id => 1000011, petId => 7214, shipDate => <<"20221103-221700">>, status => placed, requestId => <<"testrequestid">>},
  {response, 200, _, RespBody} = fake_request(<<"POST">>, <<"/store/order">>, #{body => jsx:encode(Order0)}),
  #{<<"quantity">> := 31} = jsx:decode(iolist_to_binary(RespBody)),
  ok.

% A parameter in a query string
findPetsByStatus(_) -> #{}.
query_string_parameters(_) ->
  %{response, 200, _, RespBody} = fake_request(<<"GET">>, <<"/pet/findByStatus">>, #{qs => <<"status=pending,sold">>}),
  %#{} = jsx:decode(iolist_to_binary(RespBody)),
  {skip, qs_array_not_implemented}.


fake_request(Method, Path, Extra) ->
  fake_request(petstore_yaml, Method, Path, Extra).
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

putFile(#{req := Req}) ->
  File = #{size => 100},
  {done, Req#{body => jsx:encode(File)}}.

done_request(_) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/done_req.yaml",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => put, prefix => <<"/test/put">>}),
  [{<<"/test/put/putFile">>, _, {put, <<"/putFile">>}}] = Routes,
  _ = fake_request(put, <<"PUT">>, <<"/putFile">>, #{}),
  ok.

