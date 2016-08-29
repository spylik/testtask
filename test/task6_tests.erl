-module(task6_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TM, task6).

% --------------------------------- fixtures ----------------------------------

test6_test_() ->
    {setup,
        fun disable_output/0, % disable output for ci
        {inparallel,
            [
                {<<"validate/1 must work">>,
                    fun() ->
                            ?assertEqual(false, ?TM:validate([])),
                            ?assertEqual(false, ?TM:validate([{<<"PROD_COVER_GTIN">>, <<"test">>}])),
                            ?assertEqual(false, ?TM:validate([{<<"PROD_NAME">>, <<"test">>}])),
                            ?assertEqual(false, ?TM:validate([{<<"PROD_NAME">>, <<"test">>}, {<<"SOME_VALUE">>, <<"test2">>}])),
                            ?assertEqual(false, ?TM:validate([{<<"SOME_VALUE">>, <<"test2">>}, {<<"PROD_NAME">>, <<"test">>}])),
                            ?assertEqual({<<"test2">>,<<"test1">>}, ?TM:validate([{<<"PROD_COVER_GTIN">>, <<"test2">>}, {<<"PROD_NAME">>, <<"test1">>}])),
                            ?assertEqual({<<"test2">>,<<"test1">>}, ?TM:validate([{<<"PROD_NAME">>, <<"test1">>}, {<<"PROD_COVER_GTIN">>, <<"test2">>}])),
                            ?assertEqual({<<"test2">>,<<"test1">>}, ?TM:validate([{<<"PROD_NAME">>, <<"test1">>}, {<<"PROD_COVER_GTIN">>, <<"test2">>}, {<<"SOMEVAL">>, <<"test3">>}])),
                            ?assertEqual({<<"test2">>,<<"test1">>}, ?TM:validate([{<<"SOMEVAL">>, <<"test3">>}, {<<"PROD_NAME">>, <<"test1">>}, {<<"PROD_COVER_GTIN">>, <<"test2">>}]))
                    end
                },
                {<<"extract must work">>,
                    fun() ->
                            Filename = "test/mockdata/Test.xml",
                            {ok, Bin} = file:read_file(Filename),
                            Data = ?TM:extract(Bin),
                            ?assertEqual(true, is_list(Data)),
                            ?assertEqual({<<"PROD_COVER_GTIN">>, <<"4600209001493">>}, lists:keyfind(<<"PROD_COVER_GTIN">>,1,Data)),
                            ?assertEqual({<<"GS1_MEMBER_GLN">>, <<"4600209999998">>}, lists:keyfind(<<"GS1_MEMBER_GLN">>,1,Data))
                    end
                }
            ]
        }
    }.


disable_output() ->
    error_logger:tty(false).
