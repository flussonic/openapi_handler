-module(test_schema_res).
-compile([nowarn_export_all, export_all]).

authorize(_) ->
  #{auth => yes_please}.

jsonArray(#{json_body := [1,2,3]} = Req) ->
  ct:pal("Req: ~p", [Req]),
  #{json_res => <<"1">>}.
