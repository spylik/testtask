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
                {<<"When gen_server started, file must present and size must be 3 (cuz we have BOM)">>,
                    fun() ->
                        ?assertEqual(3,filelib:file_size(?TESTFILE)) % we have BOM, so it must be 3
                    end},
                {<<"Complex test: must accept newreq cast with full parameters and save it to ?TESTFILE">>,
                    fun() ->
                        COMA = <<",">>,NL = <<"\n">>,
                        GTIN1 = integer_to_binary(rand:uniform(90)),
                        NAME1 = integer_to_binary(rand:uniform(90)),
                        DESCR1 = integer_to_binary(rand:uniform(90)),
                        BRAND1 = integer_to_binary(rand:uniform(90)),
                        gen_server:cast(?TESTSERVER, {newreq, GTIN1, NAME1 ,[{<<"PROD_DESC">>,DESCR1},{<<"BRAND_OWNER_NAME">>,BRAND1}]}),
                        timer:sleep(20),
                        ?assert(filelib:file_size(?TESTFILE)>3),
                        BOM = unicode:encoding_to_bom(utf8),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([BOM,GTIN1,COMA,NAME1,COMA,DESCR1,COMA,BRAND1,NL])))),
                        GTIN2 = integer_to_binary(rand:uniform(90)),
                        NAME2 = integer_to_binary(rand:uniform(90)),
                        DESCR2 = integer_to_binary(rand:uniform(90)),
                        BRAND2 = integer_to_binary(rand:uniform(90)),
                        gen_server:cast(?TESTSERVER, {newreq, GTIN2, NAME2 ,[{<<"PROD_DESC">>,DESCR2},{<<"BRAND_OWNER_NAME">>,BRAND2}]}),
                        timer:sleep(20),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([BOM,GTIN1,COMA,NAME1,COMA,DESCR1,COMA,BRAND1,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN2,COMA,NAME2,COMA,DESCR2,COMA,BRAND2,NL])))),
                        ?assertEqual(ok, ?TESTSERVER:stop()),
                        ?assertEqual(
                            false,  
                            is_pid(whereis(?TESTSERVER))
                        ),
                        ?TESTSERVER:start_link(?TESTFILE),
                        ?assertEqual(
                            true, 
                            is_pid(whereis(?TESTSERVER))
                        ),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([BOM,GTIN1,COMA,NAME1,COMA,DESCR1,COMA,BRAND1,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN2,COMA,NAME2,COMA,DESCR2,COMA,BRAND2,NL])))),
                        GTIN3 = integer_to_binary(rand:uniform(90)),
                        NAME3 = integer_to_binary(rand:uniform(90)),
                        DESCR3 = integer_to_binary(rand:uniform(90)),
                        BRAND3 = integer_to_binary(rand:uniform(90)),
                        gen_server:cast(?TESTSERVER, {newreq, GTIN3, NAME3 ,[{<<"PROD_DESC">>,DESCR3},{<<"BRAND_OWNER_NAME">>,BRAND3}]}),
                        timer:sleep(20),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([BOM,GTIN1,COMA,NAME1,COMA,DESCR1,COMA,BRAND1,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN2,COMA,NAME2,COMA,DESCR2,COMA,BRAND2,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN3,COMA,NAME3,COMA,DESCR3,COMA,BRAND3,NL])))),
                        GTIN4 = integer_to_binary(rand:uniform(90)),
                        NAME4 = integer_to_binary(rand:uniform(90)),
                        DESCR4 = integer_to_binary(rand:uniform(90)),
                        gen_server:cast(?TESTSERVER, {newreq, GTIN4, NAME4 ,[{<<"PROD_DESC">>,DESCR4}]}),
                        timer:sleep(20),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([BOM,GTIN1,COMA,NAME1,COMA,DESCR1,COMA,BRAND1,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN2,COMA,NAME2,COMA,DESCR2,COMA,BRAND2,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN3,COMA,NAME3,COMA,DESCR3,COMA,BRAND3,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN4,COMA,NAME4,COMA,DESCR4,COMA,NL])))),
                        GTIN5 = integer_to_binary(rand:uniform(90)),
                        NAME5 = integer_to_binary(rand:uniform(90)),
                        gen_server:cast(?TESTSERVER, {newreq, GTIN5, NAME5 ,[{<<"PRODX_DESCX">>,"test"}]}),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([BOM,GTIN1,COMA,NAME1,COMA,DESCR1,COMA,BRAND1,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN2,COMA,NAME2,COMA,DESCR2,COMA,BRAND2,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN3,COMA,NAME3,COMA,DESCR3,COMA,BRAND3,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN4,COMA,NAME4,COMA,DESCR4,COMA,NL])))),
                        ?assert(is_in_file(binary_to_list(erlang:iolist_to_binary([GTIN5,COMA,NAME5,COMA,COMA,NL]))))
                    end}

            ]
        }
    }.

setup_start() -> cleanup(), ?TESTSERVER:start_link(?TESTFILE).

is_in_file(Pattern) ->
    {ok, Device} = file:open(?TESTFILE, [read]),
    F = fun Loop(P) when P =:= Pattern -> file:close(Device),true;
            Loop(eof) -> file:close(Device),false;
            Loop(_P) -> Loop(io:get_line(Device, ""))
        end,
    F("").
    

cleanup() ->
    _ = file:delete(?TESTFILE),
    error_logger:tty(false).


