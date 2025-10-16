-module(openapi_handler_legacy).

-export([init/3, handle/2, terminate/3]).

-export([method/1, peer/1, qs/1, parse_qs/1, bindings/1]).
-export([read_body/1, reply/4, headers/1, header/2, header/3, parse_header/2]).
-export([read_multipart_files/1]).

init(_, Req, {Name, CowboyPath}) ->
  openapi_handler:do_init(Req, Name, CowboyPath, ?MODULE, #{ok => shutdown, no_handle => true}).

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



read_multipart_files(Req) ->
  % This is a copy of openapi_handler:read_multipart_files/1 with
  % cowboy_req:part/1, cowboy_req:part_body/1 and different number of cow_multipart:form_data/1
  % results.
  % Despite other methods are cowboy_req methods in this module, it is much simpler to keep such
  % method instead of multiple call mocks to read_part/1 and read_part_body/1 in tests
  do_read_multipart_files(Req, []).

do_read_multipart_files(Req0, Files) ->
  case cowboy_req:part(Req0) of
    {ok, Headers, Req1} ->
      {file, _FieldName, Filename, _CType, _TE} = cow_multipart:form_data(Headers),
      {Bin, Req2} = read_multipart_file(Req1, <<>>),
      do_read_multipart_files(Req2, [{Filename, Bin}| Files]);
    {done, Req1} ->
      {ok, Files, Req1}
end.

read_multipart_file(Req0, Bin) ->
  case cowboy_req:part_body(Req0) of
    {ok, LastBodyChunk, Req} -> {<<Bin/binary, LastBodyChunk/binary>>, Req};
    {more, BodyChunk, Req} -> read_multipart_file(Req, <<Bin/binary, BodyChunk/binary>>)
  end.
