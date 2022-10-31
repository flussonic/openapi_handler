-module(openapi_handler_SUITE).
-compile([nowarn_export_all, export_all]).


all() ->
  [{group, routes}, {group, handling}].

groups() ->
  [
   {routes, [yaml_routes, json_routes]},
   {handling, [callbacks]}
  ].


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
  [{<<"/test/yml/user/logout">>,openapi_handler,{petstore_yaml,<<"/user/logout">>}},
   _, _, _,
   {<<"/test/yml/user/:username">>,openapi_handler,{petstore_yaml,<<"/user/:username">>}},
   {<<"/test/yml/user">>,openapi_handler,{petstore_yaml,<<"/user">>}}
   |_] = Routes,
  ok.

json_routes(_) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/redocly-big-openapi.json",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => rebilly_json, prefix => <<"/test/json">>}),
  [{<<"/test/json/websites/:id/webhook">>,openapi_handler, {rebilly_json,<<"/websites/:id/webhook">>}},
   {<<"/test/json/websites/:id">>,openapi_handler, {rebilly_json,<<"/websites/:id">>}},
   {<<"/test/json/websites">>,openapi_handler, {rebilly_json,<<"/websites">>}},
   {<<"/test/json/webhooks/:id">>,openapi_handler, {rebilly_json,<<"/webhooks/:id">>}},
   {<<"/test/json/webhooks">>,openapi_handler, {rebilly_json,<<"/webhooks">>}}
   |_] = Routes,
  ok.


callbacks(_) ->
  SchemaPath = code:lib_dir(openapi_handler, test) ++ "/redocly-petstore.yaml",
  Routes = openapi_handler:routes(#{schema => SchemaPath, module => ?MODULE, name => petstore_yaml, prefix => <<"/test/yml">>}),
  Req1 = #{method => <<"GET">>, headers => #{}}
  ok.

