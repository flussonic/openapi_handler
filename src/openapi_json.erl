-module(openapi_json).

-export([encode/1]).
-export([decode/1, decode_with_atoms/1]).

-if(?OTP_RELEASE >= 27).

decode(Bin) ->
  try
    json:decode(Bin)
  catch
    _:_:_ -> {error, broken_json}
  end.

json_push_atom(Key, Value, Acc) ->
  [{binary_to_atom(Key), Value} | Acc].

decode_with_atoms(Bin) ->
  {Object, ok, <<>>} = json:decode(Bin, ok, #{object_push => fun json_push_atom/3}),
  Object.

encode(Source) ->
  Encode = fun
    (undefined, Encoder) -> json:encode_atom(null, Encoder);
    (Other, Encoder) -> json:encode_value(Other, Encoder)
  end,
  try
    iolist_to_binary(json:encode(Source, Encode))
  catch
    _:_:_ -> iolist_to_binary(json:encode(#{error => error_in_json_encoder, input => iolist_to_binary(io_lib:format("~p",[Source]))}))
  end.

-else. %% ?OTP_RELEASE < 27

decode(Bin) ->
  try jsx:decode(Bin,[return_maps])
  catch
    _:_:_ -> {error, broken_json}
  end.

decode_with_atoms(Bin) ->
  jsx:decode(Bin,[return_maps,{labels,atom}]).

encode(Source) ->
  try jsx:encode(Source)
  catch
    _:_:_ -> jsx:encode(#{error => error_in_json_encoder, input => iolist_to_binary(io_lib:format("~p",[Source]))})
  end.

-endif. %% ?OTP_RELEASE
