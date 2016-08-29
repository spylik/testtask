-module(task6_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TM, task6).
-define(TESTAPP, task6).

% --------------------------------- fixtures ----------------------------------
% tests for cover standart otp behaviour
otp_test_() ->
    {setup,
        fun disable_output/0, % setup
        {inorder,
            [  

                {<<"Application able to start via application:ensure_all_started()">>,
                    fun() ->
                        application:ensure_all_started(?TESTAPP),
                        ?assertEqual(
                            {ok,[]},
                            application:ensure_all_started(?TESTAPP)
                        ),
                        App = application:which_applications(),
                        ?assert(is_tuple(lists:keyfind(?TESTAPP,1,App)))
                    end},
                {<<"Application able to stop via application:stop()">>,
                    fun() ->
                        application:stop(?TESTAPP),
                        App = application:which_applications(),
                        ?assertEqual(false, is_tuple(lists:keyfind(?TESTAPP,1,App)))
                end}
            ]
        }
    }.


test6_simple_test_() ->
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

test6_complex_test_() ->
    {setup,
        fun start_app/0,
        fun cleanup/1,
        {inparallel,
            [
                {<<"Request to webserver must do not crash application and cvs must exists">>,
                    fun() -> 
                        os:cmd("curl -X POST -H \"Content-Type: application/soap+xml\" -d @test/mockdata/Test.xml http://localhost:8080"),
                        App = application:which_applications(),
                        ?assert(is_tuple(lists:keyfind(?TESTAPP,1,App))),
                        ?assert(filelib:is_regular("task6.csv"))
                    end
                }
            ]
        }
    }.

start_app() -> application:ensure_all_started(?TM).

cleanup(_) -> application:stop(?TM).

disable_output() ->
    error_logger:tty(false).
