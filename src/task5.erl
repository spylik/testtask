%% --------------------------------------------------------------------------------
%% File:    task5.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc Task5:
%% 2520 is the smallest number that can be divided by each of the numbers from 
%% 1 to 10 without any remainder.
%%
%% What is the smallest positive number that is evenly divisible by all of 
%% the numbers from 1 to 20?
%% --------------------------------------------------------------------------------

-module(task5).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-export([default/0]).

-compile(export_all).

% @doc hardcoded values and specs for test task.
% By default we going to find smallest positive number that divisable by range 1,20.
-spec default() -> Result when
    Result :: 232792560. % we can hardcode output type cuz we using hardcoded parameter

default() -> euqlid(1,20).


% @doc Find minimum number with dividers in range Min..Max 
-spec euqlid(Min,Max) -> Result when
    Min :: pos_integer(),
    Max :: pos_integer(),
    Result :: pos_integer().

euqlid(Min,Max) -> 
    euqlid(lists:seq(Min,Max)).

euqlid([H|[]]) -> H;
euqlid([A,B|T]) -> 
    F = fun Loop(X,0) -> X;
            Loop(X,Y) -> Loop(Y, X rem Y)
    end,
    euqlid([(A*B) div F(A,B)|T]).
