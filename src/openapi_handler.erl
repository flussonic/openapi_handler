-module(openapi_handler).
-include_lib("kernel/include/logger.hrl").


-export([routes/1, init/3, handle/2, terminate/3]).
-export([load_schema/2]).
-export([routes_sort/1]). % for tests



routes(#{schema := SchemaPath, module := Module, name := Name, prefix := Prefix}) ->
  #{} = Schema = load_schema(SchemaPath, Name),

  #{paths := Paths} = Schema,
  Routes = lists:map(fun({Path,PathSpec}) ->
    <<"/", _/binary>> = CowboyPath = re:replace(atom_to_list(Path), "{([^\\}]+)}", ":\\1",[global,{return,binary}]),
    Parameters = maps:get(parameters, PathSpec, []),
    PathSpec1 = maps:filtermap(
      fun(Method, Operation) -> prepare_operation_fm(Method, Operation, Parameters) end,
      maps:remove(parameters,PathSpec)),

    % It is too bad to pass all this stuff through cowboy options because it starts suffering
    % from GC on big state. Either ETS, either persistent_term, either compilation of custom code
    persistent_term:put({openapi_handler_route,Name,CowboyPath}, PathSpec1#{name => Name, module => Module}),
    {<<Prefix/binary, CowboyPath/binary>>, ?MODULE, {Name,CowboyPath}}
  end, maps:to_list(Paths)),
  % Нужно отсортировать так, чтобы символ ':', это противоречит стандартной сортировке
  % поэтому отсоритируем как есть и развернем, тогда у нас ':' точно будет в ожидаемом месте
  routes_sort(Routes).


prepare_operation_fm(_M, #{operationId := OperationId_} = Operation, Parameters) ->
  Op1 = maps:remove(description,Operation),
  OperationId = binary_to_atom(OperationId_,latin1),
  Params1 = Parameters ++ maps:get(parameters, Op1, []),
  Op2 = Op1#{operationId => OperationId, parameters => Params1},
  Op3 = case Op2 of
    #{'x-collection-name' := CollectionName, 'x-collection-type' := CollectionType} ->
      Op2#{
        'x-collection-name' => binary_to_atom(CollectionName,latin1),
        'x-collection-type' := binary_to_atom(CollectionType,latin1)
      };
    #{} ->
      Op2
  end,
  Responses0 = maps:to_list(maps:get(responses, Operation, #{})),
  Responses = [{list_to_integer(atom_to_list(Code)),maps:remove(description,CodeSpec)} || 
    {Code,CodeSpec} <- Responses0,
    Code /= default],
  {true, Op3#{responses => maps:from_list(Responses)}};
prepare_operation_fm(_, #{}, _) ->
  % Skip operations with no operationId
  false.

routes_sort(Routes) ->
  lists:reverse(lists:sort(fun path_sort/2, Routes)).

path_sort({Path1,_,_},{Path2,_,_}) ->
  Path1 =< Path2.



load_schema(#{} = Schema, Name) ->
  #{components := #{schemas := Schemas}} = Schema,
  [persistent_term:put({openapi_handler_schema,Name,atom_to_binary(Type,latin1)}, TypeSchema) ||
    {Type,TypeSchema} <- maps:to_list(Schemas)],
  persistent_term:put({openapi_handler_schema,Name},Schema),
  Schema;

load_schema(SchemaPath, Name) when is_list(SchemaPath) orelse is_binary(SchemaPath) ->
  {ok, Bin} = file:read_file(SchemaPath),
  Format = case filename:extension(iolist_to_binary(SchemaPath)) of
    <<".yaml">> -> yaml;
    <<".yml">> -> yaml;
    <<".json">> -> json;
    _ -> json
  end,
  DecodedSchema = case Format of
    json ->
      jsx:decode(Bin,[return_maps,{labels,atom}]);
    yaml ->
      application:ensure_all_started(yamerl),
      [Decoded0] = yamerl:decode(Bin, [{erlang_atom_autodetection, false}, {map_node_format, map}, {str_node_as_binary, true}]),
      map_keys_to_atom(Decoded0)
  end,
  load_schema(DecodedSchema, Name).

%% yamerl lacks an option to make all map keys atom, but keep values binary. So, we need this converter.
map_keys_to_atom(#{} = Map) ->
  maps:fold(fun(K, V, Acc) -> Acc#{binary_to_atom(K, utf8) => map_keys_to_atom(V)} end, #{}, Map);
map_keys_to_atom(List) when is_list(List) ->
  [map_keys_to_atom(E) || E <- List];
map_keys_to_atom(Value) ->
  Value.






init(_, Req, {Name, CowboyPath}) ->
  #{module := Module, name := Name} = Spec = persistent_term:get({openapi_handler_route, Name, CowboyPath}),
  {Method_, Req2} = cowboy_req:method(Req),
  Method = case Method_ of
    <<"POST">> -> post;
    <<"GET">> -> get;
    <<"DELETE">> -> delete;
    <<"PUT">> -> put;
    <<"OPTIONS">> -> options;
    _ -> undefined
  end,
  Operation = maps:get(Method, Spec, undefined),
  {Originator, _} = cowboy_req:header(<<"x-originator">>, Req),
  {Authorization, _} = cowboy_req:header(<<"authorization">>, Req),
  case Operation of
    undefined when Method == options ->
      {ok, Req3} = cowboy_req:reply(200, cors_headers(), <<>>, Req2),
      {shutdown, Req3, undefined};
    undefined ->
      {ok, Req3} = cowboy_req:reply(405, 
        json_headers(),
        [jsx:encode(#{error => <<"unknown_operation">>}),"\n"], Req2),
      {shutdown, Req3, undefined};
    #{} ->
      {Args, Req3} = collect_parameters(Operation, Req2, Name),
      case Args of
        {error, E} ->
          {ok, Req4} = cowboy_req:reply(400, 
            json_headers(),
            [jsx:encode(E#{while => parsing_parameters}),"\n"], Req3),
          {shutdown, Req4, undefined};
        #{} ->
          Accept = case cowboy_req:header(<<"accept">>, Req3, <<"application/json">>) of
            {<<"text/plain",_/binary>>,_} -> text;
            {_,_} -> json
          end,
          {Ip,_} = fetch_ip_address(Req),
          Operation1 = Operation#{
            module => Module,
            args => Args,
            name => Name,
            ip => Ip,
            accept => Accept,
            originator => Originator,
            authorization => Authorization
          },
          case Module:authorize(Operation1) of
            #{} = AuthContext ->
              {ok, Req3, Operation1#{auth_context => AuthContext}};
            {error, denied} ->
              {ok, Req4} = cowboy_req:reply(403, json_headers(), [jsx:encode(#{error => authorization_failed}),"\n"], Req3),
              {shutdown, Req4, undefined}
          end
      end
  end.


collect_parameters(#{parameters := Parameters} = Spec, Req, ApiName) ->
  {Qs, Req1} = cowboy_req:qs(Req),
  {QsVals, Req2} = cowboy_req:qs_vals(Req1),
  {Bindings, Req3} = cowboy_req:bindings(Req2),
  {Headers, _} = cowboy_req:headers(Req3),
  ContentType = case proplists:get_value(<<"content-type">>, Headers) of
    <<"application/json",_/binary>> -> 'application/json';
    <<"text/json",_/binary>> -> 'application/json';
    <<"text/plain",_/binary>> -> 'text/plain';
    _ -> 'application/octet-stream'
  end,
  Args = lists:foldl(fun
    (_, {error, E}) ->
      {error, E};
    (#{in := In, name := Name, schema := Schema} = ParamSpec, Acc) ->
      Required = maps:get(required, ParamSpec, false),
      Key = case In of
        <<"header">> ->
          Name1 = binary:replace(Name, <<"X-">>, <<>>),
          Name2 = binary:replace(Name1, <<"-">>, <<"_">>, [global]),
          Name3 = cowboy_bstr:to_lower(Name2),
          binary_to_atom(Name3, latin1);
        _ ->
          binary_to_atom(Name,latin1) % It is OK here to make binary_to_atom
      end,
      Value1 = case In of
        <<"path">> -> proplists:get_value(Key, Bindings);
        <<"query">> -> proplists:get_value(Name, QsVals);
        <<"header">> -> proplists:get_value(cowboy_bstr:to_lower(Name), Headers)
      end,
      Value2 = case Schema of
        #{default := Default} when Value1 == undefined -> Default;
        _ -> Value1
      end,
      case Value2 of
        undefined when Required ->
          {error, #{missing_required => Name}};
        undefined ->
          Acc;
        _ ->
          case openapi_schema:process(Value2, #{schema => Schema}) of
            {error, ParseError} ->
              {error, ParseError#{name => Name, input => Value1}};
            Value3 ->
              Acc#{Key => Value3}
          end
      end
  end, #{raw_qs => Qs}, Parameters),

  case Args of
    {error, _} ->
      {Args, Req3};
    #{} ->
      case Spec of
        #{requestBody := #{content := #{'application/json' := #{schema := BodySchema}}}} when
          ContentType == 'application/json' ->
          {ok, TextBody, Req4} = cowboy_req:body(Req3),
          case TextBody of
            <<>> ->
              {Args, Req4};
            _ ->
              Body = jsx:decode(TextBody, [return_maps]),
              case openapi_schema:process(Body, #{schema => BodySchema, name => ApiName}) of
                {error, ParseError} ->
                  {{error, ParseError#{name => request_body, input1 => Body}}, Req4};
                Value1 ->
                  Args1 = Args#{json_body => Value1},
                  {Args1, Req4}
              end              
          end;
        #{requestBody := #{content := #{'text/plain' := #{schema := #{type := <<"string">>}}}}} when
          ContentType == 'text/plain' ->
          {ok, TextBody, Req4} = cowboy_req:body(Req3),
          Args1 = Args#{raw_body => TextBody},
          {Args1, Req4};          
        #{requestBody := #{content := #{'*/*' := #{schema := #{format := <<"binary">>}}}}} ->
          Args1 = Args#{req => Req3},
          {Args1, Req3};          
        #{} ->
          {Args, Req3}
      end
  end.
  


handle(Req, #{responses := Responses, name := Name, module := Module, ip := Ip} = Request) ->
  T1 = erlang:system_time(micro_seconds),
  {ContentType, Code, Response} = handle_request(Request),
  % T2 = erlang:system_time(micro_seconds),
  {Code2, Headers, PreparedResponse} = case Responses of
    #{Code := #{content := #{'application/json' := #{schema := Schema}}}} when ContentType == json ->
      case openapi_schema:process(Response, #{schema => Schema, name => Name}) of
        {error, Error} ->
          {500, json_headers(), [jsx:encode(Error),"\n"]};
        EncodedJson ->
          Postprocessed = Module:postprocess(EncodedJson, Request),
          {Code, json_headers(), [jsx:encode(Postprocessed),"\n"]}
      end;
    #{} when is_binary(Response) andalso ContentType == text ->
      {Code, text_headers(), Response};
    #{} ->
      {Code, json_headers(), [jsx:encode(Response),"\n"]};
    {done, Req1} ->
      {done, undefined, Req1}
  end,
  T3 = erlang:system_time(micro_seconds),

  catch Module:log_call(Request#{code => Code, time => T3-T1, ip => Ip}),
  {ok, Req2} = case Code2 of
    done -> {ok, PreparedResponse}; % HACK to bypass request here
    204 -> cowboy_req:reply(Code2, cors_headers(), [], Req);
    _ -> gzip_and_reply(Code2, Headers, PreparedResponse, Req)
  end,
  {ok, Req2, undefined}.


gzip_and_reply(Code, Headers, Body, Req) when Code >= 200 andalso Code < 300->
  {ok, AcceptEncoding,_Req2} = cowboy_req:parse_header(<<"accept-encoding">>, Req),
  AcceptGzip = if is_list(AcceptEncoding) -> lists:keymember(<<"gzip">>, 1, AcceptEncoding);
    true -> false
  end,

  Gzipping = AcceptGzip == true,
  case Gzipping of
    true ->
      Body1 = zlib:gzip(Body),
      Headers1 = [{<<"Content-Encoding">>,<<"gzip">>}|Headers],
      cowboy_req:reply(Code, Headers1, Body1, Req);
    false ->
      cowboy_req:reply(Code, Headers, Body, Req)
  end;

gzip_and_reply(Code, Headers, Body, Req) ->
  cowboy_req:reply(Code, Headers, Body, Req).


fetch_ip_address(Req) ->
  case cowboy_req:header(<<"x-real-ip">>, Req) of
    {Ip, Req1} when is_binary(Ip) -> {Ip, Req1};
    _ ->
      case cowboy_req:header(<<"cf-connecting-ip">>, Req) of
        {Ip,Req1} when is_binary(Ip) -> {Ip, Req1};
        _ ->
          {{PeerAddr,_}, Req1} = cowboy_req:peer(Req),
          Ip = list_to_binary(inet_parse:ntoa(PeerAddr)),
          {Ip, Req1}
      end
  end.


json_headers() ->
  [{<<"Content-Type">>,<<"application/json">>}] ++ cors_headers().

text_headers() ->
  [{<<"Content-Type">>,<<"text/plain">>}] ++ cors_headers().

cors_headers() ->
  [{<<"Access-Control-Allow-Origin">>, <<"*">>},
  {<<"Access-Control-Allow-Methods">>, <<"GET, PUT, DELETE, OPTIONS">>},
  {<<"Access-Control-Expose-Headers">>, <<"*">>},
  {<<"Access-Control-Allow-Headers">>, <<"*">>}].


handle_request(#{module := Module, operationId := OperationId, args := Args, auth_context := AuthContext,
  'x-collection-name' := CollectionName} = OpenAPI) ->

  #{raw_qs := Qs} = Args,
  Type = maps:get('x-collection-type', OpenAPI),
  Name = maps:get(name, OpenAPI),
  case openapi_collection:parse_qs(Qs, #{limit => 100, collection_type => Type, schema_name => Name}) of
    {error, E} ->
      {json, 400, E#{while => parsing_query}};
    #{} = RawQuery ->
      Query = maps:merge(Args, RawQuery),
      T1 = minute:now_ms(),
      try Module:OperationId(Query#{auth_context => AuthContext}) of
        {json, Code, Response} ->
          {json, Code, Response};
        #{CollectionName := FullList} when is_list(FullList) ->
          T2 = minute:now_ms(),
          R = openapi_collection:list(FullList, Query#{timing => #{load => T2-T1}, collection => CollectionName}),
          {json, 200, R}
      catch
        error:undef:ST ->
          case ST of
            [{Module,OperationId,_,_}|_] ->
              {json, 501, #{error => not_implemented}};
            _ ->
              ?LOG_ALERT(#{class => error, error => undef, stacktrace => ST}),
              Response = #{error => crashed},
              {json, 500, Response}
          end;
        C:E:ST ->
          ?LOG_ALERT(#{class => C, error => E, stacktrace => ST}),
          Response = #{error => crashed},
          {json, 500, Response}
      end
  end;

handle_request(#{module := Module, operationId := OperationId, args := Args, accept := Accept, auth_context := AuthContext, ip := Ip}) ->
  try Module:OperationId(Args#{auth_context => AuthContext, agent_ip => Ip}) of
    {error, badrequest} ->
      {json, 400, #{error => bad_request}};
    {error, enoent} ->
      {json, 404, #{error => not_found}};
    {error, unavailable} ->
      {json, 503, #{error => unavailable}};
    ok ->
      {json, 204, undefined};
    {json, Code, Response} ->
      {json, Code, Response};
    #{} = Response ->
      {json, 200, Response};
    <<_/binary>> = Response when Accept == text ->
      {text, 200, Response};
    {done, Req} ->
      {done, Req};
    Else ->
      ?LOG_ALERT(#{operationId => OperationId, invalid_response => Else, accept => Accept}),
      {json, 500, #{error => invalid_response}}
  catch
    C:E:ST ->
      ?LOG_ALERT(#{class => C, error => E, stacktrace => ST}),
      case ST of
        [{Module,OperationId,_,_}|_] when E == undef ->
          {json, 501, #{error => not_implemented}};
        _ ->
          {json, 500, #{error => crashed}}
      end
  end.



terminate(_,_,_) ->
  ok.
