-module(openapi_collection).
-include_lib("kernel/include/logger.hrl").

-export([parse_qs/2]).
-export([qs/1]).
-export([list/2]).
-export([calculate_cursors/5, unwrap_kv/1]).
-export([filter_collection/2]).



add_defaults(Query0) ->
  Sort1 = maps:get(sort, Query0, []),
  ImplicitSort = {['$position'],asc},
  Sort2 = case Sort1 of
    [] -> [ImplicitSort];
    _ -> Sort1
  end,
  Query0#{sort => Sort2}.



parse_qs(Qs, #{collection_type := TypeName, schema_name := Name} = Opts) ->
  Query1 = parse_qs_api(cow_qs:parse_qs(Qs), maps:with([limit],Opts), Opts),

  Query2 = case Query1 of
    #{filter := Filter0} ->
      case parse_filter(Filter0, Name, TypeName) of
        {error, E} ->
          {error, E};
        Filter ->
          Query1#{filter => Filter}
      end;
    #{} ->
      Query1
  end,
  Query2.



parse_filter(#{} = Filter0, Name, TypeName) ->
  Opts = #{
    type => TypeName,
    name => Name,
    query => true,
    auto_convert => true
  },
  case openapi_schema:process(Filter0, Opts) of
    #{} = Filter1 -> Filter1;
    {error, E} -> {error, E}
  end.






parse_qs_api([{<<"cursor">>, Cursor}|Qs], Query, Opts) ->
  CursorQuery = parse_qs(base64:decode(Cursor), Opts),
  parse_qs_api(Qs, Query#{cursor => CursorQuery}, Opts);

parse_qs_api([{<<"sort">>, Sort}|Qs], Query, Opts) ->
  Sortlist = [case Arg of
    <<"-",Arg0/binary>> -> {binary:split(Arg0,<<".">>,[global]), desc};
    _ -> {binary:split(Arg,<<".">>,[global]), asc}
  end || Arg <- binary:split(Sort,<<",">>,[global])],
  parse_qs_api(Qs, Query#{sort => Sortlist}, Opts);

parse_qs_api([{<<"limit">>,LimitBin}|Qs], Query, Opts) ->
  case string:to_integer(LimitBin) of
    {Limit,<<>>} ->
      parse_qs_api(Qs, Query#{limit => Limit}, Opts);
    _ ->
      parse_qs_api(Qs, Query#{parse_error => broken_limit}, Opts)
  end;

parse_qs_api([{<<"select">>,Select}|Qs], Query, Opts) ->
  SelectList = [binary:split(Arg,<<".">>,[global]) || Arg <- binary:split(Select,<<",">>,[global])],
  Wrap = fun
    Wrap([], _) ->
      true;
    Wrap([Key|List], Map) ->
      case Map of
        #{Key := #{} = V} -> Map#{Key => Wrap(List, V)};
        _ -> Map#{Key => Wrap(List, #{})}
      end
  end,
  SelectMap = lists:foldl(Wrap, #{}, SelectList),
  parse_qs_api(Qs, Query#{select => SelectMap}, Opts);

parse_qs_api([{<<"$reversed">>,<<"true">>}|Qs], Query, Opts) ->
  parse_qs_api(Qs, Query#{reversed => true}, Opts);


parse_qs_api([{Key,Value}|Qs], Query, Opts) ->
  L1 = size(Key) - 3,
  L2 = size(Key) - 4,
  L3 = size(Key) - 5,
  L4 = size(Key) - 7,
  Filter = maps:get(filter, Query, #{}),
  {Key1, Value1} = case Key of
    <<Key0:L1/binary, "_ne">> -> {Key0, #{'$ne' => Value}};
    <<Key0:L1/binary, "_gt">> -> {Key0, #{'$gt' => Value}};
    <<Key0:L1/binary, "_lt">> -> {Key0, #{'$lt' => Value}};
    <<Key0:L1/binary, "_is">> when Value == <<"null">> -> {Key0, null};
    <<Key0:L4/binary, "_is_not">> when Value == <<"null">> -> {Key0, not_null};
    <<Key0:L2/binary, "_gte">> -> {Key0, #{'$gte' => Value}};
    <<Key0:L2/binary, "_lte">> -> {Key0, #{'$lte' => Value}};
    <<Key0:L3/binary, "_like">> -> {Key0, #{'$like' => Value}};
    _ when Value == true -> {Key, [<<"true">>]};
    _ -> {Key, binary:split(Value,<<",">>,[global])}
  end,
  Key2 = binary:split(Key1, <<".">>, [global]),
  Filter1 = deep_set(Key2, Value1, Filter),
  parse_qs_api(Qs, Query#{filter => Filter1}, Opts);

parse_qs_api([], Query, _Opts) ->
  Query.


deep_set([K], V, Map) ->
  case Map of
    #{K := #{} = V1} when is_map(V) -> Map#{K => maps:merge(V1,V)};
    _ -> Map#{K => V}
  end;

deep_set([K|List], V, Map) ->
  case maps:get(K, Map, #{}) of
    #{} = Map1 ->
      Map#{K => deep_set(List, V, Map1)};
    _ ->
      Map
  end.



qs(#{} = Query) ->
  QsVals = encode_qs_api2(maps:to_list(Query)),
  Text = cow_qs:qs(QsVals),
  Text.

encode_qs_api2([{filter,Filter}|Query]) ->
  Enc = fun
    Enc({Key,V}) when is_atom(Key) -> Enc({atom_to_binary(Key,latin1),V});
    Enc({Key,#{'$ne' := V}}) -> [{<<Key/binary,"_ne">>, V}];
    Enc({Key,#{'$gt' := V}}) -> [{<<Key/binary,"_gt">>, V}];
    Enc({Key,#{'$lt' := V}}) -> [{<<Key/binary,"_lt">>, V}];
    Enc({Key,#{'$gte' := V}}) -> [{<<Key/binary,"_gte">>, V}];
    Enc({Key,#{'$lte' := V}}) -> [{<<Key/binary,"_lte">>, V}];
    Enc({Key,#{'$like' := V}}) -> [{<<Key/binary,"_like">>, V}];
    Enc({Key,null}) -> [{<<Key/binary,"_is">>, <<"null">>}];
    Enc({Key,not_null}) -> [{<<Key/binary,"_is_not">>, <<"null">>}];
    Enc({Key,V}) when is_list(V) -> [{Key, iolist_to_binary(lists:join($,,V))}];
    Enc({Key,#{}=V}) ->
      [ [{<<Key/binary,".",K2/binary>>,V2} || {K2,V2} <- Enc({K1,V1})] || {K1,V1} <- maps:to_list(V)];
    Enc(#{} = KV) ->
      [Enc(KV_) || KV_ <- maps:to_list(KV)]
  end,
  lists:flatten(Enc(Filter))++ encode_qs_api2(Query);

encode_qs_api2([{reversed,true}|Query]) ->
  [{<<"$reversed">>,<<"true">>}|encode_qs_api2(Query)];

encode_qs_api2([{limit,Limit}|Query]) ->
  LimitBin = if
    is_integer(Limit) -> integer_to_binary(Limit);
    is_binary(Limit) -> Limit
  end,
  [{<<"limit">>,LimitBin}|encode_qs_api2(Query)];

encode_qs_api2([{sort,Sort}|Query]) ->
  SortBin = lists:join($,, lists:map(fun
    ({Key,asc}) -> lists:join($., [to_b(K) || K <- Key]);
    ({Key,desc}) -> ["-"] ++ lists:join($., [to_b(K) || K <- Key])
  end, Sort)),
  [{<<"sort">>,iolist_to_binary(SortBin)}|encode_qs_api2(Query)];

encode_qs_api2([{select,Select}|Query]) ->
  Enc = fun
    Enc({Key,V}) when is_atom(Key) -> Enc({atom_to_binary(Key,latin1),V});
    Enc({Key,true}) -> [Key];
    Enc({Key,#{}=V}) ->
      [ [<<Key/binary,".",K2/binary>> || K2 <- Enc(KV)] || KV <- maps:to_list(V)];
    Enc(#{} = KV) ->
      [Enc(KV_) || KV_ <- maps:to_list(KV)]
  end,
  Select1 = lists:flatten(Enc(Select)),
  Select2 = iolist_to_binary(lists:join($,, Select1)),
  [{<<"select">>,Select2}|encode_qs_api2(Query)];

% encode_qs_api2([{cc,CountCache}|Query]) ->
%   [{<<"cc">>,integer_to_binary(CountCache)}|encode_qs_api2(Query)];

encode_qs_api2([{cursor,#{} = Cursor}|Query]) ->
  [{<<"cursor">>,base64:encode(qs(Cursor))}|encode_qs_api2(Query)];

encode_qs_api2([]) ->
  [].



to_b(Atom) when is_atom(Atom) -> atom_to_binary(Atom,latin1);
to_b(Int) when is_integer(Int) -> integer_to_binary(Int);
to_b(Bin) when is_binary(Bin) -> Bin.









% $$\       $$\             $$\     
% $$ |      \__|            $$ |    
% $$ |      $$\  $$$$$$$\ $$$$$$\   
% $$ |      $$ |$$  _____|\_$$  _|  
% $$ |      $$ |\$$$$$$\    $$ |    
% $$ |      $$ | \____$$\   $$ |$$\ 
% $$$$$$$$\ $$ |$$$$$$$  |  \$$$$  |
% \________|\__|\_______/    \____/ 






list(Collection, #{} = Query0) ->
  Query = add_defaults(Query0),

  Timing = maps:get(timing, Query, #{}),
  Cursor = maps:get(cursor, Query, #{}),

  T2 = erlang:system_time(milli_seconds),
  IndexedCollection = lists:zipwith(fun(P,S) -> S#{'$position' => P} end, lists:seq(0,length(Collection)-1), Collection),

  Reversed = maps:get(reversed, Cursor, false),
  SortedCollection = sort_collection(maps:get(sort, Query), IndexedCollection, Reversed),

  T3 = erlang:system_time(milli_seconds),

  {FilteredCollection, _} = filter_collection(maps:get(filter, Query, #{}), SortedCollection),
  {CursorFilteredCollection, HasLess} = filter_collection(maps:get(filter, Cursor, #{}), FilteredCollection),

  T4 = erlang:system_time(milli_seconds),
  {LimitedCollection, HasMore} = limit_collection(maps:get(limit, Query, undefined), CursorFilteredCollection),
  T5 = erlang:system_time(milli_seconds),
  SelectedCollection = select_fields(Query, LimitedCollection),
  ReversedCollection = case Reversed of
    true -> lists:reverse(SelectedCollection);
    false -> SelectedCollection
  end,
  ClearedCollection = [maps:remove('$position',S) || S <- ReversedCollection],
  ResultSet = ClearedCollection,
  T6 = erlang:system_time(milli_seconds),

  % D = fun(List) ->
  %   [maps:get(name,N) || N <- List]
  % end,
  % ct:pal("Filter ~p\n~p ->\n~p\n~p\n\n\n"
  %   "sorted\n~p\n\n"
  %   "has_more: ~p\n~p", [Query, D(IndexedCollection), D(FilteredCollection), D(CursorFilteredCollection),
  %   D(SortedCollection),
  %   HasMore, D(LimitedCollection)]),

  TotalCount = length(FilteredCollection),
  Cursors = calculate_cursors(maps:with([filter,select,sort,limit],Query), ReversedCollection, Reversed, HasMore, HasLess),
  CollectionName = maps:get(collection, Query, items),
  Cursors#{
    CollectionName => ResultSet,
    estimated_count => TotalCount,
    timing => Timing#{
      filter => T4-T3,
      sort => T3-T2,
      limit => T5-T4,
      select => T6-T5
    }
  }.







% $$$$$$$$\ $$\ $$\   $$\                         
% $$  _____|\__|$$ |  $$ |                        
% $$ |      $$\ $$ |$$$$$$\    $$$$$$\   $$$$$$\  
% $$$$$\    $$ |$$ |\_$$  _|  $$  __$$\ $$  __$$\ 
% $$  __|   $$ |$$ |  $$ |    $$$$$$$$ |$$ |  \__|
% $$ |      $$ |$$ |  $$ |$$\ $$   ____|$$ |      
% $$ |      $$ |$$ |  \$$$$  |\$$$$$$$\ $$ |      
% \__|      \__|\__|   \____/  \_______|\__|      




filter_collection(#{} = Filter, Collection) ->
  KV = unwrap_kv(Filter),
  {Collection1, HasLess} = filter3(Collection, KV),
  {Collection1, HasLess}.


filter3([Item|Collection], KV) ->
  Collection1 = [S || S <- Collection, filter_match(S,KV)],
  case filter_match(Item, KV) of
    true -> {[Item|Collection1], false};
    false -> {Collection1, true}
  end;

filter3([], _) ->
  {[], false}.


filter_match(Item, KV) ->
  lists:all(fun(KV1) -> filter2(KV1, Item) end, KV).


filter2({Key,Values0}, Item) when is_list(Values0) ->
  % make binaries if enum
  Values = lists:flatmap(fun
    (Bool) when is_boolean(Bool) -> [Bool];
    (V) when is_atom(V) -> [V, atom_to_binary(V)];
    (V) -> [V]
  end, Values0),
  lists:member(getkey(Key,Item),Values);

filter2({Key,not_null}, Item) ->
  getkey(Key,Item) =/= undefined;

filter2({Key,null}, Item) ->
  getkey(Key,Item) == undefined;

filter2({Key,#{'$ne' := Value}}, Item) ->
  getkey(Key,Item) =/= Value;

filter2({Key,#{'$gt' := Value}}, Item) ->
  V = getkey(Key,Item),
  V > Value andalso V =/= undefined;

filter2({Key,#{'$lt' := Value}}, Item) ->
  V = getkey(Key,Item),
  V < Value andalso V =/= undefined;

filter2({Key,#{'$gte' := Value}}, Item) ->
  V = getkey(Key,Item),
  V >= Value andalso V =/= undefined;

filter2({Key,#{'$lte' := Value}}, Item) ->
  V = getkey(Key,Item),
  V =< Value andalso V =/= undefined;

filter2({Key,#{'$like' := Value}}, Item) ->
  case to_b2(getkey(Key,Item)) of
    <<Binary/binary>> when is_binary(Value) ->
      case binary:match(Binary, [Value], []) of
        nomatch -> false;
        _ -> true
      end;
    _ -> false
  end.


to_b2(undefined) -> undefined;
to_b2(null) -> null;
to_b2(<<Bin/binary>>) -> Bin;
to_b2(Atom) when is_atom(Atom) -> atom_to_binary(Atom,latin1);
to_b2(V) -> V.

%
% One more trick here:
% "stats.bitrate_gt=187" is converted to  stats => #{bitrate => #{'$gt' => 187}}
%
% We unwind it into  [stats, bitrate] => #{'$gt' => 187}  and iterate over the list
%
% TODO: protocol for accessing lists
%
% One more case:
% stats.last_dts_ago_gt=0&stats.last_dts_ago_lt=1000  is transformed to  stats => #{last_dts_ago => #{'$gt' => 0,'lt' => 1000}}
%

unwrap_kv(#{} = Filter) ->
  unwrap_kv2(maps:to_list(Filter), []).


unwrap_kv2([], _Prefix) ->
  [];

unwrap_kv2([{Key, #{'$ne' := V} = Value}|List], Prefix) ->
  [{Prefix ++ [Key], #{'$ne' => V}} | unwrap_kv2([{Key, maps:remove('$ne',Value)}|List], Prefix)];

unwrap_kv2([{Key, #{'$gt' := V} = Value}|List], Prefix) ->
  [{Prefix ++ [Key], #{'$gt' => V}} | unwrap_kv2([{Key, maps:remove('$gt',Value)}|List], Prefix)];

unwrap_kv2([{Key, #{'$gte' := V} = Value}|List], Prefix) ->
  [{Prefix ++ [Key], #{'$gte' => V}} | unwrap_kv2([{Key, maps:remove('$gte',Value)}|List], Prefix)];

unwrap_kv2([{Key, #{'$lt' := V} = Value}|List], Prefix) ->
  [{Prefix ++ [Key], #{'$lt' => V}} | unwrap_kv2([{Key, maps:remove('$lt',Value)}|List], Prefix)];

unwrap_kv2([{Key, #{'$lte' := V} = Value}|List], Prefix) ->
  [{Prefix ++ [Key], #{'$lte' => V}} | unwrap_kv2([{Key, maps:remove('$lte',Value)}|List], Prefix)];

unwrap_kv2([{Key, #{'$like' := V} = Value}|List], Prefix) ->
  [{Prefix ++ [Key], #{'$like' => V}} | unwrap_kv2([{Key, maps:remove('$like',Value)}|List], Prefix)];

unwrap_kv2([{_,#{} = Value}|List], Prefix) when Value == #{} ->
  unwrap_kv2(List, Prefix);

unwrap_kv2([{Key,#{} = Value}|List], Prefix) ->
  unwrap_kv2(maps:to_list(Value), Prefix ++ [Key]) ++ unwrap_kv2(List, Prefix);

unwrap_kv2([{Key,Value}|List], Prefix) ->
  [{Prefix++[Key], Value}|unwrap_kv2(List, Prefix)].




getkey([K],S) ->
  getkey(K, S);

getkey([K|List],S) ->
  getkey(List, maps_get(K,S));

getkey(K,S) ->
  maps_get(K,S).

maps_get(K,S) ->
  V1 = case S of
    #{K := V} ->
      V;
    #{} when is_binary(K) ->
      maps:get(binary_to_existing_atom(K,latin1),S, undefined);
    _ ->
      undefined
  end,
  case V1 of
    null -> undefined;
    <<"null">> -> undefined;
    <<"undefined">> -> undefined;
    V1 -> V1
  end.





setkey([K],S, V) ->
  setkey(K, S, V);

setkey([K|List],S,V) ->
  maps_set(K,S,setkey(List, maps_get(K,S), V));

setkey(K,S,V) ->
  maps_set(K,S,V).

maps_set(K,S,V) ->
  case S of
    #{K := _} ->
      S#{K => V};
    #{} when is_binary(K) ->
      maps:put(binary_to_existing_atom(K,latin1),V,S);
    #{} ->
      S#{K => V};
    undefined ->
      #{K => V}
  end.




%  $$$$$$\                       $$\     
% $$  __$$\                      $$ |    
% $$ /  \__| $$$$$$\   $$$$$$\ $$$$$$\   
% \$$$$$$\  $$  __$$\ $$  __$$\\_$$  _|  
%  \____$$\ $$ /  $$ |$$ |  \__| $$ |    
% $$\   $$ |$$ |  $$ |$$ |       $$ |$$\ 
% \$$$$$$  |\$$$$$$  |$$ |       \$$$$  |
%  \______/  \______/ \__|        \____/ 


sort_collection(Key, Collection, Reversed) ->
  Collection1 = lists:sort(fun(S1,S2) ->
    Value1 = get_comparator(Key, S1, S2),
    Value2 = get_comparator(Key, S2, S1),
    case Reversed of
      false -> Value1 =< Value2;
      true -> Value1 > Value2
    end
  end, Collection),
  Collection1.


get_comparator([], _, _) ->
  [];

get_comparator([{Key, asc}|List], S1, S2) ->
  [get_comparator_key(Key,S1)| get_comparator(List, S1, S2)];

get_comparator([{Key, desc}|List], S1, S2) ->
  [get_comparator_key(Key,S2)| get_comparator(List, S1, S2)].


% Explicit type sort
get_comparator_key(Key, S) ->
  case getkey(Key, S) of
    undefined -> {0,undefined};
    Int when is_integer(Int) -> {1,Int};
    Bin when is_binary(Bin) -> {2,Bin};
    Value -> {3,Value}
  end.

% $$\       $$\               $$\   $$\     
% $$ |      \__|              \__|  $$ |    
% $$ |      $$\ $$$$$$\$$$$\  $$\ $$$$$$\   
% $$ |      $$ |$$  _$$  _$$\ $$ |\_$$  _|  
% $$ |      $$ |$$ / $$ / $$ |$$ |  $$ |    
% $$ |      $$ |$$ | $$ | $$ |$$ |  $$ |$$\ 
% $$$$$$$$\ $$ |$$ | $$ | $$ |$$ |  \$$$$  |
% \________|\__|\__| \__| \__|\__|   \____/ 





limit_collection(undefined, Collection) ->
  {Collection, false};


limit_collection(Count, Collection) when is_integer(Count) andalso Count > 0 ->
  List1 = lists:sublist(Collection, Count+1),
  HasMore = length(List1) > Count,
  {lists:sublist(List1, Count), HasMore}.







select_fields(#{select := Fields} = Query, Streams) ->
  Streams1 = case Query of
    #{schema := Schema, schema_name := SchemaName, collection := CollectionName} ->
      #{CollectionName := Streams1_} = openapi_schema:process(
        #{CollectionName => Streams}, #{schema => Schema, name => SchemaName, required_obj_keys => drop, access_type => read, explain => [required]}),
        Streams1_;
    #{} ->
      Streams
  end,
  [selector(Fields#{'$position' => true}, S) || S <- Streams1];


select_fields(_, Streams) ->
  Streams.


selector(true, S) ->
  S;

selector(Fields, S) ->
  RequiredKeys = [binary_to_atom(Key, latin1) || Key <- maps:get(required, maps:get('$explain', S, #{}), [])],
  RequiredFields = maps:with(RequiredKeys, S),
  S1 = lists:flatmap(fun({K,Nested}) ->
    case S of
      #{K := V} ->
        [{K,selector(Nested,V)}];
      #{} ->
        K1 = binary_to_existing_atom(K,latin1),
        case S of
          #{K1 := V} -> [{K1,selector(Nested,V)}];
          _ -> [{K,undefined}]
        end
    end      
  end, maps:to_list(Fields)),
  delete_explain(maps:merge(maps:from_list(S1), RequiredFields)).




calculate_next_cursor(#{sort := Sort}, [_|_] = List) ->
  Last = lists:last(List),
  Filter1 = lists:foldl(fun
    ({Sort1,asc}, F) ->
      case getkey(Sort1,Last) of
        undefined -> F;
        Val -> setkey(Sort1, F, #{'$gt' => to_b(Val)})
      end;
    ({Sort1,desc}, F) ->
      case getkey(Sort1,Last) of
        undefined -> F;
        Val -> setkey(Sort1, F, #{'$lt' => to_b(Val)})
      end
  end, #{}, Sort),
  #{filter => Filter1};

calculate_next_cursor(_, []) ->
  undefined.




calculate_prev_cursor(#{sort := [_|_] = Sort}, [First|_]) ->
  Filter1 = lists:foldl(fun
    ({Sort1,asc}, F) ->
      case getkey(Sort1,First) of
        undefined -> F;
        Val -> setkey(Sort1, F, #{'$lt' => to_b(Val)})
      end;
    ({Sort1,desc}, F) ->
      case getkey(Sort1,First) of
        undefined -> F;
        Val -> setkey(Sort1, F, #{'$gt' => to_b(Val)})
      end
  end, #{}, Sort),
  #{filter => Filter1, reversed => true};

calculate_prev_cursor(_, []) ->
  undefined.





calculate_cursors(Query, List, Reversed, HasMore, HasLess) ->
  Next64 = case calculate_next_cursor(Query, List) of
    _ when not Reversed and not HasMore -> undefined;
    _ when Reversed and not HasLess -> undefined;
    undefined -> undefined;
    Next -> base64:encode(qs(Next))
  end,
  Prev64 = case calculate_prev_cursor(Query, List) of
    _ when not Reversed and not HasLess -> undefined;
    _ when Reversed and not HasMore -> undefined;
    undefined -> undefined;
    Prev -> base64:encode(qs(Prev))
  end,
  #{
    next => Next64,
    prev => Prev64
  }.




delete_explain(Elem) when is_list(Elem) ->
  [delete_explain(ElemItem) || ElemItem <- Elem];
delete_explain(Elem) when is_map(Elem) ->
  maps:fold(fun(Key, Value, Acc) -> Acc#{Key => delete_explain(Value)} end, #{}, maps:without(['$explain'], Elem));
delete_explain(Elem) ->
  Elem.


