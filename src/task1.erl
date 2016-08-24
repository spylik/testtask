%% --------------------------------------------------------------------------------
%% File:    task1.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc Task1:
%% If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
%% Find the sum of all the multiples of 3 or 5 below 1000.
%% --------------------------------------------------------------------------------

-module(task1).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-export([default/0]).

-type dividers() :: pos_integer() | neg_integer().

% @doc hardcoded values and specs for test task.
% By default we multiples dividers 3 and 5 for numbers bellow 1000
-spec default() -> Result when
    Result :: 233168. % we can hardcode spec cuz we using hardcoded parameters

default() -> md([3,5], 1, 999).

% @doc calculating sum of numbers in range dividable by list of dividers.
-spec md(Dividers, Min, Max) -> Result when
    Dividers :: [dividers()],
    Min :: integer(),
    Max :: integer(),
    Result :: integer().

md(Dividers, Min, Max) ->
    lists:sum(
        [Element || Element <- lists:seq(Min,Max), is_dividend(false, Element, Dividers)]
    ).

% @doc check is number is dividable by any divider from list.
% use tail recursion with pattern matching and immidiatly return true when find first 
% divider.
-spec is_dividend(Flag, Number, Dividers) -> Result when
    Flag :: 0 | term(),
    Number :: integer(),
    Dividers :: [dividers()],
    Result :: boolean().

is_dividend(0, _, _) -> true;
is_dividend(_, _, []) -> false;
is_dividend(_, Number, [H|T]) ->
    is_dividend(Number rem H, Number, T).
