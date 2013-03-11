-module(geohasherl).

-export([to_hash/2, to_hash/3, to_coordinates/1]).

-define(Precision(X), X*5).

to_hash(Latitude, Longitude) ->
    encode(Latitude, Longitude, ?Precision(12)).

to_hash(Latitude, Longitude, Precision) ->
    encode(Latitude, Longitude, ?Precision(Precision)).

to_coordinates(GeoHash) ->
    Bin = geohasherl_base32:decode(GeoHash),
    decode_binary(Bin).

encode(Latitude, Longitude, Precision) ->
    LatPre = Precision div 2,
    LongPreAdd = Precision rem 2,
    LongPre = LatPre + LongPreAdd,
    EncodedLat = encode_latitude(Latitude, LatPre),
    EncodedLong = encode_longitude(Longitude, LongPre),
    %% TODO: Сделать что-нибудь с этой магией. Например вынести в функцию.
    if LongPreAdd == 0 -> Lat = EncodedLat;
       true -> Lat = EncodedLat ++ [none]
    end,
    Flist = lists:foldl(fun({X,Y}, L) -> L ++ [X,Y] end, [],
                       lists:zip(EncodedLong, Lat)),
    if LongPreAdd == 0 -> List = Flist;
       true -> List = Flist -- [none]
    end,
    IntList = to_intlist(List),
    BinList = binary:list_to_bin(IntList),
    geohasherl_base32:encode_binary(BinList).

encode_latitude(Latitude, Precision) ->
    encode_coord(Latitude, Precision, -90.0, 90.0).

encode_longitude(Longitude, Precision) ->
    encode_coord(Longitude, Precision, -180.0, 180.0).

encode_coord(_, 0, _, _, Acc) ->
    Acc;
encode_coord(Val, Precision, Low, Top, Acc) ->
    Mid = (Low + Top) / 2,
    case Val >  Mid of
        true ->
            NewTop = Top,
            NewLow = Mid,
            Bit = 1;
        false ->
            NewTop = Mid,
            NewLow = Low,
            Bit = 0
    end,
    encode_coord(Val, Precision - 1, NewLow, NewTop, Acc ++ [Bit]).
encode_coord(Val, Precision, Low, Top) ->
    encode_coord(Val, Precision, Low, Top, []).
to_intlist([], Acc) ->
    Acc;
to_intlist(List, Acc) ->
    {L, Tail} = lists:split(5, List),
    BinL = binary:list_to_bin(L),
    Int = to_int(BinL),
    to_intlist(Tail, Acc ++ [Int]).
to_intlist(List) ->
    to_intlist(List, []).

to_int(Bin) ->
    B = << <<X:1>> || <<X>> <= Bin >>,
    <<I:5/integer>> = B,
    I.

decode_binary(Bin) ->
    BinCoords = to_binary_coords(Bin),
    BinCoordsRaw = get_coords_bits(BinCoords),
    {BinCoordsList, Tail} = lists:splitwith(fun(X) -> erlang:is_tuple(X) end,
                            BinCoordsRaw),
    {EvenBits, OddBits} = lists:unzip(BinCoordsList),
    LongitudeBits = EvenBits ++ Tail,
    LatitudeBits = OddBits,
    Latitude = decode_latitude_bits(LatitudeBits),
    Longitude = decode_longitude_bits(LongitudeBits),
    {Latitude, Longitude}.

decode_latitude_bits(Bin) ->
    decode_bits(Bin, -90.0, 90).

decode_longitude_bits(Bin) ->
    decode_bits(Bin, -180, 180).

decode_bits(Bin, Low, Top) ->
    decode_bits(Bin, Low, Top, {0, 90}).
decode_bits([], _, _, Acc) ->
    Acc;
decode_bits([B|Rest], Low, Top, Acc) ->
    Mid = (Low + Top) / 2,
    case B of
        1 ->
            NewTop = Top,
            NewLow = Mid;
        0 ->
            NewTop = Mid,
            NewLow = Low
    end,
    {_, Err} = Acc,
    NewVal = NewLow + (NewTop - NewLow) / 2,
    NewErr = Err / 2,
    decode_bits(Rest, NewLow, NewTop, {NewVal, NewErr}).

to_binary_coords(<<>>) ->
    [];
to_binary_coords(<< B:8, Rest/binary >>) ->
    binary:list_to_bin([get_binary_list(B) | to_binary_coords(Rest)]).

get_binary_list(B) ->
    << <<X>> || <<X:1>> <= <<B:5>> >>.

get_coords_bits(<<>>) ->
    [];
get_coords_bits(<< A >>) ->
    [A];
get_coords_bits(<< E, O, Rest/binary >>) ->
    [{E, O} | get_coords_bits(Rest)].
