-module(task4_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TM, task4).

% --------------------------------- fixtures ----------------------------------

test4_test_() ->
    {setup,
        fun disable_output/0, % disable output for ci
        {inparallel,
            [
                {<<"For default function we should return 906609">>,
                    fun() ->
                        ?assertEqual(906609, ?TM:default())
                    end
                },
                {<<"genmin/1 must work">>,
                    fun() ->
                        ?assertEqual(10, ?TM:genmin(2)),
                        ?assertEqual(100, ?TM:genmin(3))
                    end
                },
                {<<"genmax/1 must work">>,
                    fun() ->
                        ?assertEqual(99, ?TM:genmax(2)),
                        ?assertEqual(999, ?TM:genmax(3))
                    end
                },
                {<<"is_palindrome/1 must work">>,
                    fun() ->
                        ?assertEqual(true, ?TM:is_palindrome("teet")),
                        ?assertEqual(false, ?TM:is_palindrome("tete")),
                        ?assertEqual(true, ?TM:is_palindrome("9009")),
                        ?assertEqual(false, ?TM:is_palindrome("9019"))
                    end
                },
                {<<"large_pol/1 must work">>,
                    fun() ->
                        ?assertEqual(9009, ?TM:large_pol(2))
                    end
                }
            ]
        }
    }.


disable_output() ->
    error_logger:tty(false).
