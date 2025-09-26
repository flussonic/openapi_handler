-module(openapi_client).
-include_lib("kernel/include/logger.hrl").


-export([load/1, call/3, call/4, store/2]).


load(#{schema_url := Path0} = State) ->
  Path = case iolist_to_binary(Path0) of
    <<"file://",U/binary>> -> U;
    U -> U
  end,
  Schema = openapi_handler:read_schema(Path),
  #{servers := [#{url := BaseURL}|_]} = Schema,
  BaseURI = uri_string:parse(BaseURL),
  BasePath = maps:get(path,BaseURI),

  case State of
    #{url := URL0} ->
      case uri_string:parse(iolist_to_binary(URL0)) of
        #{path := <<>>} = URI ->
          URI1 = URI#{path => BasePath},
          (maps:remove(url,State))#{schema => Schema, uri => URI1};
        #{} = URI ->
          (maps:remove(url,State))#{schema => Schema, uri => URI}
      end;
    #{} ->
      State#{schema => Schema, uri => BaseURI}
  end.



store(Name, #{schema := _} = SchemaState) ->
  persistent_term:put({openapi,Name}, SchemaState#{schema_name => Name}).

call(NameOrState, OperationId, Args) when is_atom(NameOrState); is_map(NameOrState) ->
  call(NameOrState, OperationId, Args, _Opts = []).


call(Name, OperationId, Args, Opts) when is_atom(Name) ->
  try persistent_term:get({openapi,Name}) of
    Api -> call(Api, OperationId, Args, Opts)
  catch
    _:_:_ -> {error, not_loaded}
  end;

call(#{schema := Schema, uri := URI} = State, OperationId, Args0, Opts) when is_list(Opts) ->
  Args1 = maps:merge(maps:get(default_args, State, #{}), Args0),
  % Skip useless flags from openapi_handler
  Args = maps:without([agent_ip,auth_context,raw_qs,req,collection_type,schema_name], Args1),
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
      Timeout = proplists:get_value(timeout, Opts, 50000),
      Request = case Method of
        get ->
          {RequestURL, RequestHeaders};
        _ ->
          ContentType = proplists:get_value("Content-Type", RequestHeaders, "text/plain"),
          {RequestURL, RequestHeaders, ContentType, RequestBody}
      end,
      Result = case httpc:request(Method, Request, [{timeout, Timeout}], [{body_format, binary}]) of
        {ok, {{_, Code0, _}, ResponseHeaders0, Bin0}} ->
          {ok, Code0, [{string:to_lower(K),V} || {K,V} <- ResponseHeaders0], Bin0};
        {error, E0} ->
          {error, E0}
      end,

      case Result of
        {ok, Code,ResponseHeaders,Bin} when is_map_key(Code, Responses) ->
          ?LOG_DEBUG("< ~p\n~p\n~s", [Code, ResponseHeaders,Bin]),
          check_cors_presence(ResponseHeaders),
          ResponseContentType = case proplists:get_value("content-type", ResponseHeaders) of
            undefined -> undefined;
            CT -> list_to_atom(CT)
          end,
          ResponseSpec = maps:get(Code, Responses),
          ContentMap = maps:get(content, ResponseSpec, #{}),
          Response1 = case maps:get(ResponseContentType, ContentMap, undefined) of 
            #{schema := ResponseSchema} when ResponseContentType == 'application/json' ->
              JSON = openapi_json:decode(Bin),
              Response = openapi_schema:process(JSON, #{schema => ResponseSchema, whole_schema => Schema}),
              Response;
            #{} when ResponseContentType == 'text/plain' orelse ResponseContentType == 'text/csv' ->
              Bin;
            undefined when Code == 204 ->
              ok;
            _ ->
              Bin
          end,
          case Code of
            200 -> Response1;
            204 -> Response1;
            404 -> {error, enoent};
            _ -> {error, {Code, Response1}}
          end;
        {ok, 404,ResponseHeaders,Body} ->
          ?LOG_INFO("~s ~s -> 404 ~p", [Method, uri_string:recompose(RequestURI), Body]),
          check_cors_presence(ResponseHeaders),
          {error, enoent};
        {ok, 503,ResponseHeaders,Body} ->
          ?LOG_INFO("~s ~s -> 503 ~p", [Method, uri_string:recompose(RequestURI), Body]),
          check_cors_presence(ResponseHeaders),
          {error, unavailable};
        {ok, 403,ResponseHeaders,_} ->
          ?LOG_INFO("~s ~s -> 403", [Method, uri_string:recompose(RequestURI)]),
          check_cors_presence(ResponseHeaders),
          {error, denied};
        {ok, Code,ResponseHeaders,Bin} ->
          check_cors_presence(ResponseHeaders),
          Response = case proplists:get_value("content-type", ResponseHeaders) of
            "application/json" -> try openapi_json:decode_with_atoms(Bin)
              catch _:_ -> Bin end;
            _ -> Bin
          end,
          {error, {Code,Response}};
        {error, E} ->
          {error, E}
      end
      % io:format("call ~p with ~p\n", [Op, Args])
  end;


call(#{} = State, OperationId, Args, Opts) ->
  case load(State) of
    #{} = State1 -> call(State1, OperationId, Args, Opts)
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
      OpResponses = [{K,V} || {K,V} <- maps:to_list(maps:get(responses, Op, #{})), K =/= default],
      Responses = maps:from_list([{list_to_integer(atom_to_list(K)),V} || {K,V} <- OpResponses]),
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
  substitute_args2(Parameters, URI, Query, Headers++[{"Content-Type","application/json"}], openapi_json:encode(Value), Args);

substitute_args2(Parameters, URI, Query, Headers, _, [{raw_body,Value}|Args]) ->
  substitute_args2(Parameters, URI, Query, Headers++[{"Content-Type","text/plain"}], Value, Args);

substitute_args2(Parameters, URI, Query, Headers, Body, [{originator,Value}|Args]) ->
  substitute_args2(Parameters, URI, Query, Headers++[{"X-Originator",Value}], Body, Args);

substitute_args2(Parameters, URI, Query, Headers, Body, [{authorization,Value}|Args]) ->
  substitute_args2(Parameters, URI, Query, Headers++[{"Authorization",Value}], Body, Args);

substitute_args2(Parameters, URI, Query, Headers, Body, [{accept,Value}|Args]) ->
  substitute_args2(Parameters, URI, Query, Headers++[{"Accept",Value}], Body, Args);

substitute_args2(Parameters, URI, Query, Headers, _, [{files,Files}|Args]) ->
  Headers1 = Headers ++ [{"Content-Type", "multipart/form-data; boundary=abcde12345"}],
  Body = iolist_to_binary([
    lists:map(fun({Name, Bin}) -> [
      "--abcde12345\r\n",
      "Content-Disposition: form-data; name=\"file\"; filename=\"",Name,"\"\r\n",
      "\r\n", Bin, "\r\n"
    ] end, Files),
    "--abcde12345--\r\n"
  ]),
  substitute_args2(Parameters, URI, Query, Headers1, Body, Args);

substitute_args2(Parameters, URI, Query, Headers, Body, [{Key_,Value}|Args]) ->
  Key = atom_to_binary(Key_,latin1),
  case lists_mapfind(Key, name, Parameters) of
    false ->
      substitute_args2(Parameters, URI, lists:keystore(Key,1,Query,{Key,to_b(Value)}), Headers, Body, Args);
    #{in := <<"path">>} ->
      Path1 = binary:replace(maps:get(path,URI),<<"{",Key/binary,"}">>, cow_qs:urlencode(to_b(Value))),
      substitute_args2(Parameters, URI#{path => Path1}, Query, Headers, Body, Args);
    #{in := <<"query">>} = Spec ->
      Style = maps:get(style, Spec, undefined),
      BinValue = case Value of
        [_|_] when Style == <<"form">> -> iolist_to_binary(lists:join(<<",">>, [to_b(I) || I <- Value]));
        _ -> to_b(Value)
      end,
      substitute_args2(Parameters, URI, lists:keystore(Key,1,Query,{Key,BinValue}), Headers, Body, Args);
    #{in := <<"header">>} ->
      substitute_args2(Parameters, URI, Query, Headers++[{Key,to_b(Value)}], Body, Args);
    _ ->
      substitute_args2(Parameters, URI, Query, Headers, Body, Args)
  end.


to_b(I) when is_integer(I) -> integer_to_binary(I);
to_b(A) when is_atom(A) -> atom_to_binary(A,latin1);
to_b(X) -> iolist_to_binary(X).


check_cors_presence(Headers) ->
  case proplists:get_value("access-control-allow-origin", Headers) of
    "*" ->
      true;
    _Absent ->
      error([no_cors_headers,Headers])
  end.



lists_mapfind(Value, Key, List) ->
  case [V || #{Key := Val} = V <- List, Val == Value] of
    [V1|_] -> V1;
    _ -> false
  end.
