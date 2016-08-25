-module(task3_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TM, task3).

% --------------------------------- fixtures ----------------------------------

task3_test_() ->
    {setup,
        fun disable_output/0, % disable output for ci
        {inparallel,
            [
                {<<"For default function we should return 6857">>,
                    fun() ->
                        ?assertEqual(6857, ?TM:default())
                    end
                },
                {<<"max_factor/1 must work">>,
                    fun() ->
                        ?assertEqual(3, ?TM:max_factor(12)),
                        ?assertEqual(29, ?TM:max_factor(13195))
                    end
                }
            ]
        }
    }.


disable_output() ->
    error_logger:tty(false).
