-module(openapi_handler_legacy).

-export([init/3, handle/2, terminate/3]).

-export([method/1, peer/1, qs/1, parse_qs/1, bindings/1]).
-export([read_body/1, reply/4, headers/1, header/2, header/3, parse_header/2]).

init(_, Req, {Name, CowboyPath}) ->
  openapi_handler:do_init(Req, Name, CowboyPath, ?MODULE, #{ok => shutdown}).

handle(Req, #{} = Request) ->
  openapi_handler:do_handle(Req, Request, ?MODULE).

terminate(_,_,_) ->
  ok.


%% Cowboy compat wrapper. Provides Cowboy 2.9 API for Cowboy 1.0
method(Req) ->
  {Method, _Req1} = cowboy_req:method(Req),
  Method.

peer(Req) ->
  {Peer, _Req1} = cowboy_req:peer(Req),
  Peer.

qs(Req) ->
  {QS, _Req1} = cowboy_req:qs(Req),
  QS.

parse_qs(Req) ->
	try
		cow_qs:parse_qs(qs(Req))
	catch _:_:Stacktrace ->
		erlang:raise(exit, {request_error, qs,
			'Malformed query string; application/x-www-form-urlencoded expected.'
		}, Stacktrace)
	end.

bindings(Req) ->
  {Bindings, _Req1} = cowboy_req:bindings(Req),
  maps:from_list(Bindings).

read_body(Req) ->
  cowboy_req:body(Req).

reply(Code, Headers, Body, Req) ->
  {ok, Req1} = cowboy_req:reply(Code, maps:to_list(Headers), Body, Req),
  Req1.

headers(Req) ->
  {Headers, _Req1} = cowboy_req:headers(Req),
  maps:from_list(Headers).

header(Name, Req) ->
  {Value, _Req1} = cowboy_req:header(Name, Req),
  Value.

header(Name, Req, Default) ->
  {Value, _Req1} = cowboy_req:header(Name, Req, Default),
  Value.

parse_header(Name, Req) ->
  {ok, Value, _Req1} = cowboy_req:parse_header(Name, Req),
  Value.

