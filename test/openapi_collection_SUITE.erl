-module(openapi_collection_SUITE).
-compile([nowarn_export_all, export_all]).

all() ->
  [{group, filter}].

groups() ->
  [
   {filter, [filter_enum_types]}
  ].



filter_enum_types(_) ->
  Dataset = [
    #{id => 1, key1 => testvalue},
    #{id => 2, key1 => <<"testvalue">>},
    #{id => 3, key1 => othervalue},
    #{id => 3, key1 => <<"othervalue">>},
    #{id => 4, key2 => testvalue}
  ],
  #{estimated_count := 2, items := [#{id := 1}, #{id := 2}]} = openapi_collection:list(Dataset, #{filter => #{key1 => [testvalue]}}),
  ok.

