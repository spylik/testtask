-module(task2_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TM, task2).

% --------------------------------- fixtures ----------------------------------

task2_test_() ->
    {setup,
        fun disable_output/0, % disable output for ci
        {inparallel,
            [
                {<<"For default function we should return 4613732">>,
                    fun() ->
                        ?assertEqual(4613732, ?TM:default())
                    end
                },
                {<<"sum_even_fib/1 must work">>,
                    fun() ->
                        ?assertEqual(10, ?TM:sum_even_fib(9)),
                        ?assertEqual(0, ?TM:sum_even_fib(-9)),
                        ?assertEqual(0, ?TM:sum_even_fib(0)),
                        ?assertEqual(10, ?TM:sum_even_fib(33))
                    end
                }
            ]
        }
    }.


disable_output() ->
    error_logger:tty(false).
