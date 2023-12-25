-module(test_schema_res).
-compile([nowarn_export_all, export_all]).

authorize(_) ->
  #{auth => yes_please}.

log_call(CallInfo) ->
  (whereis(test_schema_log_call_server) /= undefined) andalso (test_schema_log_call_server ! {log_call, ?MODULE, CallInfo}).

jsonArray(#{json_body := [1,2,3]} = Req) ->
  ct:pal("Req: ~p", [Req]),
  #{json_res => <<"1">>}.


headersContentType(#{accept := _Accept, content_type := ContentType} = Req) ->
  Content = #{
    'text/plain' => <<"OK">>,
    'application/json' => #{result => <<"OK">>},
    'application/xml' => <<"<Message> OK </Message>">>,
    'random/nonsense' => <<"Some">>
  },
  case Req of
    #{response_view := <<"simple">>} -> maps:get(ContentType, Content);
    _ -> {raw, 200, #{<<"content-type">> => atom_to_binary(ContentType)}, maps:get(ContentType, Content)} 
  end.

saveRequiredFilter(#{json_body := B}) ->
  #{} = B.


selectCollectionFields(#{json_body := #{<<"unavailable">> := true}}) ->
  {error,unavailable};

selectCollectionFields(#{json_body := B}) ->
  #{elements => [B, B]}.

