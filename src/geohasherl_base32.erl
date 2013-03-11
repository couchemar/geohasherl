-module(geohasherl_base32).

-export([decode/1, encode/1, encode_binary/1]).

-define(Base32_symbols,
        <<"0123456789bcdefghjkmnpqrstuvwxyz">>).

decode(<<>>) -> [];
decode(<<S, Rest/binary>>) ->
    binary:list_to_bin([decode_symbol(S) | decode(Rest)]).
decode_symbol(B32_symbol) ->
    Symbols = ?Base32_symbols,
    {Position, _} = binary:match(Symbols, [<<B32_symbol>>], []),
    Position.

encode(Dec) when Dec >= size(?Base32_symbols) ->
    Bin = encode(Dec, 5),
    encode_binary(Bin);
encode(Dec) when Dec < size(?Base32_symbols) ->
    Bin = binary:at(?Base32_symbols, Dec),
    binary:list_to_bin([Bin]).
encode(Dec, Precision) ->
    << <<X:8>> || <<X:5>> <= <<Dec:(5*Precision)>> >>.

encode_binary(Bin) ->
    List = binary:bin_to_list(Bin),
    binary:list_to_bin(lists:map(fun(L) -> encode(L) end, List)).

