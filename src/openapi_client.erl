-module(openapi_client).
-include_lib("kernel/include/logger.hrl").


-export([load/1, call/3, store/2]).


load(#{schema_url := URL} = State) ->
  case get_url(iolist_to_binary(URL)) of
    {ok, Bin} ->
      Schema = jsx:decode(Bin, [return_maps,{labels,atom}]),
      #{servers := [#{url := BaseURL}]} = Schema,
      BaseURI = uri_string:parse(BaseURL),

      case State of
        #{url := URL0} ->
          URI = uri_string:parse(URL0),
          URI1 = URI#{path => maps:get(path,BaseURI)},
          (maps:remove(url,State))#{schema => Schema, uri => URI1};
        #{} ->
          URI = uri_string:parse(URL),
          URI1 = (maps:with([host,port,scheme],URI))#{path => maps:get(path,BaseURI)},
          State#{schema => Schema, uri => URI1}
      end;
    undefined ->
      {error, no_schema}
  end.


get_url(<<"file://",Path/binary>>) ->
  case file:read_file(Path) of
    {ok, Bin} -> {ok, Bin};
    {error, _E} -> undefined
  end;

get_url(<<"http",_/binary>> = URL) ->
  case lhttpc:request(URL, get, [], <<>>, 30000) of
    {ok, {{200,_},_,Bin}} -> {ok, Bin};
    _ -> undefined
  end.


store(Name, #{schema := _} = SchemaState) ->
  persistent_term:put({openapi,Name}, SchemaState#{schema_name => Name}).


call(Name, OperationId, Args) when is_atom(Name) ->
  call(persistent_term:get({openapi,Name}), OperationId, Args);

call(#{schema := Schema, uri := URI}, OperationId, Args) ->
  case search_operation(OperationId, Schema) of
    undefined ->
      {error, no_such_operation};
    #{path := Path, method := Method, responses := Responses} = Op->
      BasePath = maps:get(path, URI),
      Path1 = filename:join(BasePath, string:trim(Path,both,"/")),
      URI1 = URI#{path => Path1},
      {RequestURI, RequestHeaders, RequestBody} = substitute_args(Op, URI1, Args),
      RequestURL = uri_string:recompose(RequestURI),
      ?LOG_DEBUG("> ~s ~s\n~p\n~s\n", [Method, RequestURL, RequestHeaders, case Args of 
        #{raw_body := _} -> <<"raw_file_upload">>;
        _ -> RequestBody
      end]),
      case lhttpc:request(RequestURL, Method, RequestHeaders, RequestBody, 50000) of
        {ok, {{Code,_},ResponseHeaders,Bin}} when is_map_key(Code, Responses) ->
          ?LOG_DEBUG("< ~p\n~p\n~s", [Code, ResponseHeaders,Bin]),
          ResponseContentType = case proplists:get_value("Content-Type", ResponseHeaders) of
            undefined -> undefined;
            CT -> list_to_atom(CT)
          end,
          ResponseSpec = maps:get(Code, Responses),
          ContentMap = maps:get(content, ResponseSpec, #{}),
          Response1 = case maps:get(ResponseContentType, ContentMap, undefined) of 
            #{schema := ResponseSchema} when ResponseContentType == 'application/json' ->
              JSON = jsx:decode(Bin, [return_maps]),
              Response = openapi_schema:process(JSON, #{schema => ResponseSchema, whole_schema => Schema}),
              Response;
            #{} when ResponseContentType == 'text/plain' ->
              Bin;
            undefined when Code == 204 ->
              ok
          end,
          case Code of
            200 -> Response1;
            204 -> Response1;
            _ -> {error, {Code, Response1}}
          end;
        {ok, {{404,_},_,Body}} ->
          ?LOG_INFO("~s ~s -> 404 ~p", [Method, uri_string:recompose(RequestURI), Body]),
          {error, enoent};
        {ok, {{403,_},_,_}} ->
          ?LOG_INFO("~s ~s -> 403", [Method, uri_string:recompose(RequestURI)]),
          {error, denied};
        {ok, {{Code,_},ResponseHeaders,Bin}} ->
          Response = case proplists:get_value("Content-Type", ResponseHeaders) of
            "application/json" -> try jsx:decode(Bin, [return_maps,{labels,atom}])
              catch _:_ -> Bin end;
            _ -> Bin
          end,
          {error, {Code,Response}};
        {error, E} ->
          {error, E}
      end
      % io:format("call ~p with ~p\n", [Op, Args])
  end;


call(#{} = State, OperationId, Args) ->
  case load(State) of
    #{} = State1 -> call(State1, OperationId, Args);
    {error, E} -> {error, E}
  end.




search_operation(OperationId, #{paths := Paths}) ->
  search_operation_in_paths(atom_to_binary(OperationId,latin1), maps:to_list(Paths)).

search_operation_in_paths(_OperationId, []) ->
  undefined;

search_operation_in_paths(OperationId, [{Path,Methods}|Paths]) ->
  Parameters = maps:get(parameters, Methods, []),
  case search_operation_in_methods(OperationId, maps:to_list(maps:remove(parameters,Methods))) of
    undefined ->
      search_operation_in_paths(OperationId, Paths);
    #{} = Op ->
      Parameters1 = maps:get(parameters, Op, []),
      Responses = maps:from_list([{list_to_integer(atom_to_list(K)),V} || {K,V} <- maps:to_list(maps:get(responses, Op, #{}))]),
      Op#{parameters => Parameters ++ Parameters1, path => atom_to_binary(Path,latin1), responses => Responses}
  end.


search_operation_in_methods(OperationId, [{Method,#{operationId := OpId} = Op}|_]) when OperationId == OpId ->
  Op#{method => Method};

search_operation_in_methods(OperationId, [_|Methods]) ->
  search_operation_in_methods(OperationId, Methods);

search_operation_in_methods(_, []) ->
  undefined.





substitute_args(#{parameters := Parameters}, #{} = URI, #{} = Args) ->
  Query = uri_string:dissect_query(iolist_to_binary(maps:get(query,URI,<<>>))),
  substitute_args2(Parameters, URI, Query, [], <<>>, maps:to_list(Args)).


substitute_args2(_, URI, Query, Headers, Body, []) ->
  {URI#{query => uri_string:compose_query(Query)}, Headers, Body};

substitute_args2(Parameters, URI, Query, Headers, _, [{json_body,Value}|Args]) ->
  substitute_args2(Parameters, URI, Query, Headers++[{"Content-Type","application/json"}], corejson:encode(Value), Args);

substitute_args2(Parameters, URI, Query, Headers, _, [{raw_body,Value}|Args]) ->
  substitute_args2(Parameters, URI, Query, Headers++[{"Content-Type","text/plain"}], Value, Args);

substitute_args2(Parameters, URI, Query, Headers, Body, [{originator,Value}|Args]) ->
  substitute_args2(Parameters, URI, Query, Headers++[{"X-Originator",Value}], Body, Args);

substitute_args2(Parameters, URI, Query, Headers, Body, [{Key_,Value}|Args]) ->
  Key = atom_to_binary(Key_,latin1),
  case lists2:mapfind(Key, name, Parameters) of
    undefined ->
      substitute_args2(Parameters, URI, lists:keystore(Key,1,Query,{Key,to_b(Value)}), Headers, Body, Args);
    #{in := <<"path">>} ->
      Path1 = binary:replace(maps:get(path,URI),<<"{",Key/binary,"}">>, cow_qs:urlencode(to_b(Value))),
      substitute_args2(Parameters, URI#{path => Path1}, Query, Headers, Body, Args);
    #{in := <<"query">>} ->
      substitute_args2(Parameters, URI, lists:keystore(Key,1,Query,{Key,to_b(Value)}), Headers, Body, Args);
    #{in := <<"header">>} ->
      substitute_args2(Parameters, URI, Query, Headers++[{Key,to_b(Value)}], Body, Args);
    _ ->
      ct:pal("params(~p) ~p\n", [Key, Parameters]),
      substitute_args2(Parameters, URI, Query, Headers, Body, Args)
  end.




to_b(I) when is_integer(I) -> integer_to_binary(I);
to_b(A) when is_atom(A) -> atom_to_binary(A);
to_b(X) -> iolist_to_binary(X).
