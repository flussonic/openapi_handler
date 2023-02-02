-module(fake_petstore).
-compile([nowarn_export_all, export_all]).



authorize(_) ->
  #{auth => yes_please}.


% Simple callback with no parameters
logoutUser(_) ->
  #{say => goodbye}.

% A parameter in path. Schema says id is integer, so it is converted even if handler returns it as a binary
getUserByName(#{username := <<"John">>}) ->
  #{username => <<"John">>, id => <<"2384572">>, pet => undefined};

getUserByName(#{username := <<"Jack">>}) ->
  #{username => <<"Jack">>, id => <<"238457234857">>}.

updateUser(#{username := <<"Mary">>, json_body := Body}) ->
  case Body of
    #{firstName := undefined} -> #{username => <<"Mary">>, id => 15};
    _ -> {json, 400, #{error => must_erase_firstName, body => Body}}
  end.

% A parameter in a query string
findPetsByStatus(#{status := [pending,sold]}) -> 
  {json, 200, [#{name => <<"Dingo">>}]}.

% Object in a JSON body
placeOrder(#{json_body := #{petId := 7214, status := placed} = Order}) ->
  Order#{quantity => 31}.


% This responses simulates response of openapi_client:call
getInventory(#{}) ->
  {error, {501, #{error => <<"not_implemented">>}}}.
