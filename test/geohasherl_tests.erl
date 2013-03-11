-module(geohasherl_tests).
-include_lib("eunit/include/eunit.hrl").


to_hash_test() ->
    ?assertEqual(<<"ezs427zzzzzz">>, geohasherl:to_hash(42.60498046875, -5.60302734375)),
    ?assertEqual(<<"u4pruydqqvj8">>, geohasherl:to_hash(57.64911, 10.40744)),
    ?assertEqual(<<"ezs42">>, geohasherl:to_hash(42.60498046875, -5.60302734375, 5)).

to_coordinates_test_() ->
    [?_assertEqual({42.60498046875, -5.60302734375},
		   to_coord(<<"ezs42">>)),
     ?_assertEqual({57.64911063015461, 10.407439693808556},
		   to_coord(<<"u4pruydqqvj">>))
    ].

to_coord(Bin) ->
    {{Lat, _}, {Lng, _}} = geohasherl:to_coordinates(Bin),
    {Lat, Lng}.
