-module(geohasherl_base32_tests).
-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [test_decode_binary_symbol(),
     test_decode_binary_word()
    ].

encode_test_() ->
    [test_encode(),
     test_encode_word(),
     test_encode_binary()].

test_decode_binary_symbol() ->
    [?_assertEqual(<<0>>, geohasherl_base32:decode(<<"0">>)),
     ?_assertEqual(<<1>>, geohasherl_base32:decode(<<"1">>)),
     ?_assertEqual(<<2>>, geohasherl_base32:decode(<<"2">>)),
     ?_assertEqual(<<3>>, geohasherl_base32:decode(<<"3">>)),
     ?_assertEqual(<<4>>, geohasherl_base32:decode(<<"4">>)),
     ?_assertEqual(<<5>>, geohasherl_base32:decode(<<"5">>)),
     ?_assertEqual(<<6>>, geohasherl_base32:decode(<<"6">>)),
     ?_assertEqual(<<7>>, geohasherl_base32:decode(<<"7">>)),
     ?_assertEqual(<<8>>, geohasherl_base32:decode(<<"8">>)),
     ?_assertEqual(<<9>>, geohasherl_base32:decode(<<"9">>)),
     ?_assertEqual(<<10>>, geohasherl_base32:decode(<<"b">>)),
     ?_assertEqual(<<11>>, geohasherl_base32:decode(<<"c">>)),
     ?_assertEqual(<<12>>, geohasherl_base32:decode(<<"d">>)),
     ?_assertEqual(<<13>>, geohasherl_base32:decode(<<"e">>)),
     ?_assertEqual(<<14>>, geohasherl_base32:decode(<<"f">>)),
     ?_assertEqual(<<15>>, geohasherl_base32:decode(<<"g">>)),
     ?_assertEqual(<<16>>, geohasherl_base32:decode(<<"h">>)),
     ?_assertEqual(<<17>>, geohasherl_base32:decode(<<"j">>)),
     ?_assertEqual(<<18>>, geohasherl_base32:decode(<<"k">>)),
     ?_assertEqual(<<19>>, geohasherl_base32:decode(<<"m">>)),
     ?_assertEqual(<<20>>, geohasherl_base32:decode(<<"n">>)),
     ?_assertEqual(<<21>>, geohasherl_base32:decode(<<"p">>)),
     ?_assertEqual(<<22>>, geohasherl_base32:decode(<<"q">>)),
     ?_assertEqual(<<23>>, geohasherl_base32:decode(<<"r">>)),
     ?_assertEqual(<<24>>, geohasherl_base32:decode(<<"s">>)),
     ?_assertEqual(<<25>>, geohasherl_base32:decode(<<"t">>)),
     ?_assertEqual(<<26>>, geohasherl_base32:decode(<<"u">>)),
     ?_assertEqual(<<27>>, geohasherl_base32:decode(<<"v">>)),
     ?_assertEqual(<<28>>, geohasherl_base32:decode(<<"w">>)),
     ?_assertEqual(<<29>>, geohasherl_base32:decode(<<"x">>)),
     ?_assertEqual(<<30>>, geohasherl_base32:decode(<<"y">>)),
     ?_assertEqual(<<31>>, geohasherl_base32:decode(<<"z">>))].


test_decode_binary_word() ->
    [?_assertEqual(<<13,31,24,4,2>>, geohasherl_base32:decode(<<"ezs42">>))
    ].

test_encode() ->
    [?_assertEqual(<<"0">>, geohasherl_base32:encode(0)),
     ?_assertEqual(<<"1">>, geohasherl_base32:encode(1)),
     ?_assertEqual(<<"2">>, geohasherl_base32:encode(2)),
     ?_assertEqual(<<"3">>, geohasherl_base32:encode(3)),
     ?_assertEqual(<<"4">>, geohasherl_base32:encode(4)),
     ?_assertEqual(<<"5">>, geohasherl_base32:encode(5)),
     ?_assertEqual(<<"6">>, geohasherl_base32:encode(6)),
     ?_assertEqual(<<"7">>, geohasherl_base32:encode(7)),
     ?_assertEqual(<<"8">>, geohasherl_base32:encode(8)),
     ?_assertEqual(<<"9">>, geohasherl_base32:encode(9)),
     ?_assertEqual(<<"b">>, geohasherl_base32:encode(10)),
     ?_assertEqual(<<"c">>, geohasherl_base32:encode(11)),
     ?_assertEqual(<<"d">>, geohasherl_base32:encode(12)),
     ?_assertEqual(<<"e">>, geohasherl_base32:encode(13)),
     ?_assertEqual(<<"f">>, geohasherl_base32:encode(14)),
     ?_assertEqual(<<"g">>, geohasherl_base32:encode(15)),
     ?_assertEqual(<<"h">>, geohasherl_base32:encode(16)),
     ?_assertEqual(<<"j">>, geohasherl_base32:encode(17)),
     ?_assertEqual(<<"k">>, geohasherl_base32:encode(18)),
     ?_assertEqual(<<"m">>, geohasherl_base32:encode(19)),
     ?_assertEqual(<<"n">>, geohasherl_base32:encode(20)),
     ?_assertEqual(<<"p">>, geohasherl_base32:encode(21)),
     ?_assertEqual(<<"q">>, geohasherl_base32:encode(22)),
     ?_assertEqual(<<"r">>, geohasherl_base32:encode(23)),
     ?_assertEqual(<<"s">>, geohasherl_base32:encode(24)),
     ?_assertEqual(<<"t">>, geohasherl_base32:encode(25)),
     ?_assertEqual(<<"u">>, geohasherl_base32:encode(26)),
     ?_assertEqual(<<"v">>, geohasherl_base32:encode(27)),
     ?_assertEqual(<<"w">>, geohasherl_base32:encode(28)),
     ?_assertEqual(<<"x">>, geohasherl_base32:encode(29)),
     ?_assertEqual(<<"y">>, geohasherl_base32:encode(30)),
     ?_assertEqual(<<"z">>, geohasherl_base32:encode(31))].


test_encode_word() ->
    [?_assertEqual(<<"ezs42">>, geohasherl_base32:encode(14672002))
    ].

test_encode_binary() ->
    [?_assertEqual(<<"ezs42">>, geohasherl_base32:encode_binary(<<13,31,24,4,2>>))
    ].
