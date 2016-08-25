-module(task5_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TM, task5).

% --------------------------------- fixtures ----------------------------------

test4_test_() ->
    {setup,
        fun disable_output/0, % disable output for ci
        {inparallel,
            [
                {<<"For default function we should return 232792560">>,
                    fun() ->
                        ?assertEqual(232792560, ?TM:default())
                    end
                },
                {<<"euqlid/2 must work">>,
                    fun() ->
                        ?assertEqual(2520, ?TM:euqlid(1,10))
                    end
                }
            ]
        }
    }.


disable_output() ->
    error_logger:tty(false).
