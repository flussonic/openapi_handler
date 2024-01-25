-module(openapi_handler).
-include_lib("kernel/include/logger.hrl").


-export([init/2, handle/2, terminate/3]).
-export([routes/1, load_schema/2, choose_module/0]).
-export([read_schema/1]).
-export([routes_sort/1]). % for tests

% For compatibility with legacy Cowboy. Called by openapi_handler_legacy.
-export([do_init/5, do_handle/3]).


routes(#{schema := SchemaPath, module := Module, name := Name, prefix := Prefix} = Config) ->
  #{} = Schema = load_schema(SchemaPath, Name),
  SchemaOpts = get_schema_opts(Config),
  HandlerModule = choose_module(),

  #{paths := Paths} = Schema,
  Routes = lists:map(fun({Path,PathSpec}) ->
    <<"/", _/binary>> = CowboyPath = re:replace(atom_to_list(Path), "{([^\\}]+)}", ":\\1",[global,{return,binary}]),
    Parameters = maps:get(parameters, PathSpec, []),
    PathSpec1 = maps:filtermap(
      fun(Method, Operation) -> prepare_operation_fm(Method, Operation, Parameters) end,
      maps:remove(parameters,PathSpec)),

    % It is too bad to pass all this stuff through cowboy options because it starts suffering
    % from GC on big state. Either ETS, either persistent_term, either compilation of custom code
    persistent_term:put({openapi_handler_route,Name,CowboyPath}, PathSpec1#{name => Name, module => Module, schema_opts => SchemaOpts}),
    {<<Prefix/binary, CowboyPath/binary>>, HandlerModule, {Name,CowboyPath}}
  end, maps:to_list(Paths)),
  % After sorting, bindings (starting with ":") must be after constant path segments, so generic routes only work after specific ones.
  % E.g. "/api/users/admin" must be before "/api/users/:id"
  % Thus special sorting function
  routes_sort(Routes).

choose_module() ->
  try
    % Check if cowboy uses modern Req (map)
    1234 = cowboy_req:port(#{port => 1234}),
    ?MODULE
  catch
    error:{badrecord,_}:_ ->
      % cowboy uses record for Req, use legacy wrapper
      openapi_handler_legacy
  end.


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
  lists:sort(fun path_sort/2, Routes).

path_sort({Path1,_,_},{Path2,_,_}) ->
  Path1 >= Path2.



load_schema(#{} = Schema, Name) ->
  #{components := #{schemas := Schemas}} = Schema,
  [persistent_term:put({openapi_handler_schema,Name,atom_to_binary(Type,latin1)}, TypeSchema) ||
    {Type,TypeSchema} <- maps:to_list(Schemas)],
  persistent_term:put({openapi_handler_schema,Name},Schema),
  Schema;

load_schema(SchemaPath, Name) when is_list(SchemaPath) orelse is_binary(SchemaPath) ->
  DecodedSchema = read_schema(SchemaPath),
  load_schema(DecodedSchema, Name).

read_schema(SchemaPath) ->
  Bin = case file:read_file(SchemaPath) of
    {ok, Bin_} -> Bin_;
    {error, E} -> error({E,SchemaPath})
  end,
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
  DecodedSchema.


%% yamerl lacks an option to make all map keys atom, but keep values binary. So, we need this converter.
map_keys_to_atom(#{} = Map) ->
  maps:fold(fun(K, V, Acc) -> Acc#{binary_to_atom(K, utf8) => map_keys_to_atom(V)} end, #{}, Map);
map_keys_to_atom(List) when is_list(List) ->
  [map_keys_to_atom(E) || E <- List];
map_keys_to_atom(Value) ->
  Value.






init(Req, {Name, CowboyPath}) ->
  do_init(Req, Name, CowboyPath, cowboy_req, #{}).

do_init(Req, Name, CowboyPath, Mod_cowboy_req, Compat) ->
  #{module := Module, name := Name, schema_opts := SchemaOpts} = Spec = persistent_term:get({openapi_handler_route, Name, CowboyPath}),
  Method_ = Mod_cowboy_req:method(Req),
  Method = case Method_ of
    <<"POST">> -> post;
    <<"GET">> -> get;
    <<"DELETE">> -> delete;
    <<"PUT">> -> put;
    <<"OPTIONS">> -> options;
    _ -> undefined
  end,
  Operation = maps:get(Method, Spec, undefined),
  Originator = Mod_cowboy_req:header(<<"x-originator">>, Req),
  Authorization = Mod_cowboy_req:header(<<"authorization">>, Req),

  % For compatibility with legacy Cowboy
  _ok = maps:get(ok, Compat, ok),
  NoHandle = maps:get(no_handle, Compat, false),

  case Operation of
    undefined when Method == options ->
      Req3 = Mod_cowboy_req:reply(200, cors_headers(), <<>>, Req),
      {_ok, Req3, undefined};
    undefined ->
      Req3 = Mod_cowboy_req:reply(405,
        json_headers(),
        [jsx:encode(#{error => <<"unknown_operation">>}),"\n"], Req),
      {_ok, Req3, undefined};
    #{} ->
      {Args, Req3} = collect_parameters(Operation, Req, Name, SchemaOpts, Mod_cowboy_req),
      case Args of
        {error, E} ->
          Req4 = Mod_cowboy_req:reply(400,
            json_headers(),
            [jsx:encode(E#{while => parsing_parameters}),"\n"], Req3),
          {_ok, Req4, undefined};
        #{} ->
          Accept = case Mod_cowboy_req:header(<<"accept">>, Req3, <<"application/json">>) of
            <<"text/plain",_/binary>> -> text;
            <<"text/csv",_/binary>> -> csv;
            <<"*/*">> -> any;
            <<"application/json">> -> json;
            Other -> Other
          end,
          Ip = fetch_ip_address(Req, Mod_cowboy_req),
          Operation1 = Operation#{
            module => Module,
            args => Args,
            name => Name,
            ip => Ip,
            accept => Accept,
            originator => Originator,
            authorization => Authorization,
            '$cowboy_req' => Req
          },
          case Module:authorize(Operation1) of
            #{} = AuthContext when NoHandle ->
              {ok, Req3, Operation1#{auth_context => AuthContext}};
            #{} = AuthContext ->
              handle(Req, Operation1#{auth_context => AuthContext});
            {error, denied} ->
              Req4 = Mod_cowboy_req:reply(403, json_headers(), [jsx:encode(#{error => authorization_failed}),"\n"], Req3),
              {_ok, Req4, undefined}
          end
      end
  end.


collect_parameters(#{parameters := Parameters} = Spec, Req, ApiName, SchemaOpts, Mod_cowboy_req) ->
  Qs = Mod_cowboy_req:qs(Req),
  QsVals = Mod_cowboy_req:parse_qs(Req),
  Bindings = Mod_cowboy_req:bindings(Req),
  Headers = Mod_cowboy_req:headers(Req),
  ContentType = case maps:get(<<"content-type">>, Headers, undefined) of
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
        <<"path">> -> maps:get(Key, Bindings, undefined);
        <<"query">> -> proplists:get_value(Name, QsVals);
        <<"header">> -> maps:get(cowboy_bstr:to_lower(Name), Headers, undefined)
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
      {Args, Req};
    #{} ->
      case Spec of
        #{requestBody := #{content := #{'application/json' := #{schema := BodySchema}}}} when
          ContentType == 'application/json' ->
          {ok, TextBody, Req4} = Mod_cowboy_req:read_body(Req),
          case TextBody of
            <<>> ->
              {Args, Req4};
            _ ->
              Body = jsx:decode(TextBody, [return_maps]),
              case openapi_schema:process(Body, maps:merge(#{schema => BodySchema, patch => true, name => ApiName, array_convert => false, extra_obj_key => error, required_obj_keys => drop, access_type => write}, SchemaOpts)) of
                {error, ParseError_} ->
                  ParseError = maps:without([encoded], ParseError_),
                  {{error, ParseError#{name => request_body, input1 => Body}}, Req4};
                Value1 ->
                  Args1 = Args#{json_body => Value1},
                  {Args1, Req4}
              end
          end;
        #{requestBody := #{content := #{'text/plain' := #{schema := #{type := <<"string">>}}}}} when
          ContentType == 'text/plain' ->
          {ok, TextBody, Req4} = Mod_cowboy_req:read_body(Req),
          Args1 = Args#{raw_body => TextBody},
          {Args1, Req4};
        #{requestBody := #{content := #{'*/*' := #{schema := #{format := <<"binary">>}}}}} ->
          Args1 = Args#{req => Req},
          {Args1, Req};
        #{requestBody := #{content := #{'multipart/form-data' := #{schema := #{
            type := <<"object">>, properties := #{file := #{
              type := <<"array">>, items := #{
                type := <<"string">>, format := <<"binary">>}}}}}}}} ->
          {ok, Files, Req1} = read_multipart_files(Mod_cowboy_req, Req),
          Args1 = Args#{files => Files, req => Req1},
          {Args1, Req1};
        #{} ->
          {Args, Req}
      end
  end.


read_multipart_files(cowboy_req, Req) ->
  do_read_multipart_files(Req, []);
read_multipart_files(Mod_cowboy_req, Req) ->
  Mod_cowboy_req:read_multipart_files(Req).

do_read_multipart_files(Req0, Files) ->
  case cowboy_req:read_part(Req0) of
    {ok, Headers, Req1} ->
      {file, _FieldName, Filename, _CType} = cow_multipart:form_data(Headers),
      {Bin, Req2} = read_multipart_file(Req1, <<>>),
      do_read_multipart_files(Req2, [{Filename, Bin}| Files]);
    {done, Req1} ->
      {ok, Files, Req1}
end.

read_multipart_file(Req0, Bin) ->
  case cowboy_req:read_part_body(Req0) of
    {ok, LastBodyChunk, Req} -> {<<Bin/binary, LastBodyChunk/binary>>, Req};
    {more, BodyChunk, Req} -> read_multipart_file(Req, <<Bin/binary, BodyChunk/binary>>)
  end.




handle(Req, #{} = Request) ->
  do_handle(Req, #{} = Request, cowboy_req).

do_handle(Req, #{module := _, ip := _} = Request, Mod_cowboy_req) ->
  T1 = erlang:system_time(micro_seconds),
  Response = handle_request(Request),
  % T2 = erlang:system_time(micro_seconds),
  {Code2, Headers, PreparedResponse} = handle_response(Response, Request),
  T3 = erlang:system_time(micro_seconds),
  catch do_log_call(Code2, Headers, PreparedResponse, Request#{time => T3-T1}, Mod_cowboy_req),
  Req2 = case Code2 of
    done -> PreparedResponse; % HACK to bypass request here
    204 -> Mod_cowboy_req:reply(Code2, cors_headers(), [], Req);
    _ -> gzip_and_reply(Code2, Headers, PreparedResponse, Req, Mod_cowboy_req)
  end,
  {ok, Req2, undefined}.

% Add some response info and log the handled api call
do_log_call(done, _Headers, PreparedResponse, #{module := Module} = Request, Mod_cowboy_req) ->
  ContentType = catch Mod_cowboy_req:resp_header(<<"content-type">>, PreparedResponse, undefined),
  ContentLength = catch Mod_cowboy_req:resp_header(<<"content-length">>, PreparedResponse, undefined),
  % Cowboy does not store sent status code
  Module:log_call(Request#{code => undefined, content_type => ContentType, content_length => ContentLength});
do_log_call(Status, Headers, PreparedResponse, #{module := Module} = Request, _Mod_cowboy_req) ->
  ContentType = maps:get(<<"content-type">>, Headers, undefined),
  ContentLength = iolist_size(PreparedResponse),
  Module:log_call(Request#{code => Status, content_type => ContentType, content_length => ContentLength}).

% User code itself works with the request and changes its state,
% for example, when receiving a large request body
handle_response({done, Req}, _) ->
  {done, undefined, Req};

handle_response({ContentType, Code, Response}, Request) ->
  handle_response({ContentType, Code, #{}, Response}, Request);

handle_response({ContentType_, Code, Headers, Response}, #{responses := Responses, name := Name, module := Module} = Request) ->
  ContentType = check_accept_type(ContentType_, is_binary(Response)),
  case Responses of
    #{Code := #{content := #{'application/json' := #{schema := Schema}}}} when (ContentType == json orelse ContentType == <<"application/json">>) ->
      case openapi_schema:process(Response, #{schema => Schema, name => Name, required_obj_keys => drop, access_type => read}) of
        {error, Error} ->
          {500, maps:merge(Headers, json_headers()), [jsx:encode(Error),"\n"]};
        TransformedResponse ->
          Postprocessed = case erlang:function_exported(Module, postprocess, 2) of
            true -> Module:postprocess(TransformedResponse, Request);
            false -> TransformedResponse
          end,
          {Code, maps:merge(Headers, json_headers()), [jsx:encode(Postprocessed),"\n"]}
      end;
    #{} when is_binary(Response) ->
      {Code, maps:merge(Headers, text_headers(ContentType)), Response};
    #{} ->
      {Code, maps:merge(Headers, json_headers()), [jsx:encode(Response),"\n"]}
  end.


gzip_and_reply(Code, Headers, Body, Req, Mod_cowboy_req) when Code >= 200 andalso Code < 300->
  AcceptEncoding = Mod_cowboy_req:parse_header(<<"accept-encoding">>, Req),
  AcceptGzip = if is_list(AcceptEncoding) -> lists:keymember(<<"gzip">>, 1, AcceptEncoding);
    true -> false
  end,

  Gzipping = AcceptGzip == true,
  case Gzipping of
    true when is_map(Headers) ->
      Body1 = zlib:gzip(Body),
      Headers1 = Headers#{<<"content-encoding">> => <<"gzip">>},
      Mod_cowboy_req:reply(Code, Headers1, Body1, Req);
    false ->
      Mod_cowboy_req:reply(Code, Headers, Body, Req)
  end;

gzip_and_reply(Code, Headers, Body, Req, Mod_cowboy_req) ->
  Mod_cowboy_req:reply(Code, Headers, Body, Req).


fetch_ip_address(Req, Mod_cowboy_req) ->
  case Mod_cowboy_req:header(<<"x-real-ip">>, Req) of
    Ip when is_binary(Ip) -> Ip;
    _ ->
      case Mod_cowboy_req:header(<<"cf-connecting-ip">>, Req) of
        Ip when is_binary(Ip) -> Ip;
        _ ->
          {PeerAddr,_} = Mod_cowboy_req:peer(Req),
          Ip = list_to_binary(inet_parse:ntoa(PeerAddr)),
          Ip
      end
  end.


json_headers() ->
  (cors_headers())#{<<"content-type">> => <<"application/json">>}.

text_headers(text) ->
  (cors_headers())#{<<"content-type">> => <<"text/plain">>};
text_headers(csv) ->
  (cors_headers())#{<<"content-type">> => <<"text/csv">>};
text_headers(ContentType) ->
  (cors_headers())#{<<"content-type">> => ContentType}.


cors_headers() ->
  #{<<"access-control-allow-origin">> => <<"*">>,
    <<"access-control-allow-methods">> => <<"GET, PUT, DELETE, OPTIONS">>,
    <<"access-control-expose-headers">> => <<"*">>,
    <<"access-control-allow-headers">> => <<"*">>,
    <<"access-control-allow-private-network">> => <<"true">>
  }.


handle_request(#{module := Module, operationId := OperationId, args := Args, accept := Accept, auth_context := AuthContext, responses := Responses,
  'x-collection-name' := CollectionName, '$cowboy_req' := CowboyReq} = OpenAPI) ->
  #{raw_qs := Qs} = Args,
  Type = maps:get('x-collection-type', OpenAPI),
  Name = maps:get(name, OpenAPI),
  case openapi_collection:parse_qs(Qs, #{limit => 100, collection_type => Type, schema_name => Name}) of
    {error, E} ->
      {json, 400, E#{while => parsing_query}};
    #{} = RawQuery ->
      Args1 = Args#{
        auth_context => AuthContext,
        collection_type => Type,
        schema_name => Name,
        '$cowboy_req' => CowboyReq
      },
      Query = maps:merge(Args1, RawQuery),
      T1 = erlang:system_time(milli_seconds),
      try Module:OperationId(Query) of
        {json, Code, Response} ->
          {json, Code, Response};
        {error, {Code, #{} = Response}} when is_integer(Code) ->
          {json, Code, Response};
        {error, badrequest} ->
          {json, 400, #{error => bad_request}};
        {error, enoent} ->
          {json, 404, #{error => not_found}};
        {error, unavailable} ->
          {json, 503, #{error => unavailable}};
        #{CollectionName := FullList} = R0 when is_list(FullList) ->
          #{responses := #{200 := #{content := #{'application/json' := #{schema := Schema}}}}} = OpenAPI,
          T2 = erlang:system_time(milli_seconds),
          Delta = maps:without([CollectionName], R0),
          R = openapi_collection:list(FullList, Query#{timing => #{load => T2-T1}, collection => CollectionName, schema => Schema, schema_name => Name}),
          {json, 200, maps:merge(Delta, R)};
        {raw, Code, #{} = Headers, <<_/binary>> = Body} ->
          handle_raw_response(Accept, OperationId, Responses, {raw, Code, Headers, Body})
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

handle_request(#{module := Module, operationId := OperationId, args := Args, accept := Accept, auth_context := AuthContext, ip := Ip, responses := Responses, '$cowboy_req' := CowboyReq}) ->
  try Module:OperationId(Args#{auth_context => AuthContext, agent_ip => Ip, '$cowboy_req' => CowboyReq}) of
    {error, badrequest} ->
      {json, 400, #{error => bad_request}};
    {error, enoent} ->
      {json, 404, #{error => not_found}};
    {error, unavailable} ->
      {json, 503, #{error => unavailable}};
    {error, {Code, #{} = Response}} when is_integer(Code) ->
      {json, Code, Response};
    ok ->
      {json, 204, undefined};
    {json, Code, Response} ->
      {json, Code, Response};
    #{} = Response ->
      {json, 200, Response};
    <<_/binary>> = Response ->
      Accept1 = check_accept_type(Accept, true),
      case is_binary_content_required(Responses, 200, Accept1) of
        true -> {Accept1, 200, Response};
        _ ->
          ?LOG_ALERT(#{operationId => OperationId, invalid_response => Response, accept => Accept}),
          {json, 500, #{error => invalid_response}}
      end;
    {done, Req} ->
      {done, Req};
    {raw, Code, #{} = Headers, <<_/binary>> = Body} ->
      handle_raw_response(Accept, OperationId, Responses, {raw, Code, Headers, Body});
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


is_binary_content_required(Responses, Code, ContentType_) ->
  ContentType = binary_to_atom(content_type_bin(ContentType_), latin1),
  case Responses of 
    #{Code := #{content := #{ContentType := #{schema := #{type := <<"string">>}}}}} -> true;
    #{Code := #{content := #{ContentType := #{schema := #{type := string}}}}} -> true;
    #{Code := #{content := #{ContentType := #{schema := #{type := _Some}}}}} -> false;
    _ -> undefined  % No response with given ContentType is described for this Code
  end.


handle_raw_response(Accept, OperationId, Responses, {raw, Code, Headers, Body}) when is_binary(Body) ->
  ContentType = find_content_type_header(Headers),
  case (is_binary_content_required(Responses, Code, ContentType) == true) andalso
       (Accept == any orelse nomatch =/= re:run(content_type_bin(Accept), ContentType)) of
    true -> {ContentType, Code, Headers, Body};
    false ->
      ?LOG_ALERT(#{operationId => OperationId, invalid_response => {raw, Code, Headers, Body}, accept => Accept}),
      {json, 500, #{error => invalid_response}}
  end.


content_type_bin(text) -> <<"text/plain">>;
content_type_bin(csv) -> <<"text/csv">>;
content_type_bin(json) -> <<"application/json">>;
content_type_bin(Bin) when is_binary(Bin) -> Bin.


check_accept_type(any, false = _IsBodyBinary) -> json;
check_accept_type(any, true = _IsBodyBinary) -> text;
check_accept_type(Accept, _) -> Accept.


find_content_type_header(Headers) when is_map(Headers)->
  ContentTypes = [maps:get(HeaderName, Headers) || HeaderName <- maps:keys(Headers), string:equal(HeaderName, <<"content-type">>, true)],
  case ContentTypes of
    [ContentType|_] -> ContentType;
    [] -> undefined
  end.


terminate(_,_,_) ->
  ok.

get_schema_opts(#{schema_opts := #{} = SchemaOpts}) ->
  maps:map(fun
    (extra_obj_key,Flag) when Flag == drop; Flag == error -> ok;
    (K,V) -> error({bad_schema_opt,K,V})
  end, SchemaOpts),
  SchemaOpts;
get_schema_opts(#{schema_opts := _}) ->
  error(bad_schema_opts);
get_schema_opts(#{}) ->
  #{}.
