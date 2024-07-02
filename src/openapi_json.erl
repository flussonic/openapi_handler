-module(openapi_json).

-export([encode/1]).
-export([decode/1, decode_with_atoms/1]).

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

