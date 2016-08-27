-module(task6_fw_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TESTSERVER, task6_fw).
-define(TESTFILE, "/tmp/task6_fw_test.csv").

% tests for cover standart gen_server behaviour

otp_test_() ->
    {setup,
        fun cleanup/0, % setup
        {inorder,
            [
                {<<"before gen_server start, testfile must do not present">>,
                    fun() ->
                        ?assertEqual(
                            false,
                            filelib:is_regular(?TESTFILE)
                        )
                end},

                {<<"gen_server able to start via ?TESTSERVER:start_link(?TESTFILE) and register">>,
                    fun() ->
                        ?TESTSERVER:start_link(?TESTFILE),
                        ?assertEqual(
                            true, 
                            is_pid(whereis(?TESTSERVER))
                        )
                end},
                {<<"gen_server able to stop via ?TESTSERVER:stop()">>,
                    fun() ->
                        ?assertEqual(ok, ?TESTSERVER:stop()),
                        ?assertEqual(
                            false,
                            is_pid(whereis(?TESTSERVER))
                        )
                end},
                {<<"gen_server start and stop in one test">>,
                    fun() ->
                        ?TESTSERVER:start_link(?TESTFILE),
                        ?assertEqual(
                            true, 
                            is_pid(whereis(?TESTSERVER))
                        ),
                        ?assertEqual(ok, ?TESTSERVER:stop()),
                        ?assertEqual(
                            false,  
                            is_pid(whereis(?TESTSERVER))
                        )
                end}
            ]
        }
    }.

% tests which require started gen_server process
genserver_started_test_() ->
    {setup,
        fun setup_start/0,
        {inorder,
             [ 
                {<<"When gen_server started is must be register as ?TESTSERVER">>,
                    fun() ->
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        )
                    end},
                {<<"Unknown gen_calls messages must do not crash gen_server">>,
                    fun() ->
                        _ = gen_server:call(?TESTSERVER, {unknown, message}),
                        timer:sleep(1), % for async cast
                        ?assertEqual(
                            true,   
                            is_pid(whereis(?TESTSERVER))
                        )
                    end},

                {<<"Unknown gen_cast messages must do not crash gen_server">>,
                    fun() ->
                        gen_server:cast(?TESTSERVER, {unknown, message}),
                        timer:sleep(1), % for async cast
                        ?assertEqual(
                            true,   
                            is_pid(whereis(?TESTSERVER))
                        )
                    end},

                {<<"Unknown gen_info messages must do not crash gen_server">>,
                    fun() ->
                        ?TESTSERVER ! {unknown, message},
                        timer:sleep(1), % for async cast
                        ?assertEqual(
                            true,
                            is_pid(whereis(?TESTSERVER))
                        )
                    end},
                {<<"When gen_server started, file must present">>,
                    fun() ->
                        ?assertEqual(
                            true,
                            filelib:is_regular(?TESTFILE)
                        )
                    end},
                {<<"Must accept newreq cast with full parameters and save it to ?TESTFILE">>,
                    fun() ->
                        GTIN = integer_to_list(rand:uniform(90)),
                        NAME = integer_to_list(rand:uniform(90)),
                        DESCR = integer_to_list(rand:uniform(90)),
                        BRAND = integer_to_list(rand:uniform(90)),
                        ?debugVal(DESCR),
                        ?debugVal(BRAND),
                        ?assertEqual(0,filelib:file_size(?TESTFILE)),
                        gen_server:cast(?TESTSERVER, {newreq, GTIN, NAME ,[{"PROD_DESC",DESCR},{"BRAND_OWNER_NAME",BRAND}]}),
                        timer:sleep(5),
                        ?assert(filelib:file_size(?TESTFILE)>0),
                        Pattern = lists:concat([GTIN,",",NAME,",",DESCR,",",BRAND,"\n"]),
                        ?debugVal(is_in_file(Pattern))
                    end}

            ]
        }
    }.

setup_start() -> cleanup(), ?TESTSERVER:start_link(?TESTFILE).

is_in_file(Pattern) ->
    {ok, Device} = file:open(?TESTFILE, [read]),
    F = fun Loop(P) when P =:= Pattern -> file:close(Device),true;
            Loop(eof) -> error_logger:warning_msg("going to close"),file:close(Device),false;
            Loop(P) -> error_logger:warning_msg("data1 is ~p~ndata2 is ~p~n,",[Pattern,P]),Loop(io:get_line(Device, ""))
        end,
    F("").
    

cleanup() ->
    file:delete(?TESTFILE),
    error_logger:tty(false).


