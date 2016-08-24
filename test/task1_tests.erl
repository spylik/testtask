-module(task1_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TM, task1).

% --------------------------------- fixtures ----------------------------------

test1_test_() ->
    {setup,
        fun disable_output/0, % disable output for ci
        {inparallel,
            [
                {<<"For default function (range [1,999], dividers 3,5) we should return 233168">>,
                    fun() ->
                        ?assertEqual(233168, ?TM:default())
                    end
                },
                {<<"is_dividend/3 must work">>,
                    fun() ->
                        ?assertEqual(false, ?TM:is_dividend(false, 6, [])),
                        ?assertError(badarith, ?TM:is_dividend(false, 6, [0])),
                        ?assertEqual(true, ?TM:is_dividend(false, 6, [2])),
                        ?assertEqual(true, ?TM:is_dividend(false, 6, [2,0])),
                        ?assertEqual(true, ?TM:is_dividend(false, -6, [2])),
                        ?assertEqual(true, ?TM:is_dividend(false, 6, [-2])),
                        ?assertEqual(true, ?TM:is_dividend(false, 6, [3])),
                        ?assertEqual(false, ?TM:is_dividend(false, 6, [5])),
                        ?assertEqual(true, ?TM:is_dividend(false, 9, [2,3]))
                    end
                },
                {<<"md/3 must work">>,
                    fun() ->
                        ?assertEqual(23, ?TM:md([3,5], 1, 9)),
                        ?assertError(badarith, ?TM:md([3,5,0], 1, 9)),
                        ?assertEqual(23, ?TM:md([3,5], 0, 9)),
                        ?assertEqual(23, ?TM:md([3,5], -2, 9)),
                        ?assertError(function_clause, ?TM:md([3,5], -2, -9)),
                        ?assertEqual(-23, ?TM:md([3,5], -9, -2))
                    end
                }
            ]
        }
    }.


disable_output() ->
    error_logger:tty(false).
