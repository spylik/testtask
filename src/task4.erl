%% --------------------------------------------------------------------------------
%% File:    task4.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc Task4:
%% A palindromic number reads the same both ways. 
%% The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
%%
%% Find the largest palindrome made from the product of two 3-digit numbers.
%% --------------------------------------------------------------------------------

-module(task4).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-export([default/0]).

% @doc hardcoded values and specs for test task.
% By default we going to find largest polindrome produced by multiply 3 digits numbers.
-spec default() -> Result when
    Result :: 906609. % we can hardcode output type cuz we using hardcoded parameter

default() -> large_pol(3).

% @doc find the largest polindrome produced by multiplies X-digit numbers
large_pol(X) -> 
    Range = lists:seq(genmin(X), genmax(X)), % generating lists once
    lists:max([A*B || A <- Range, B <- Range, is_palindrome(integer_to_list(A*B))]).

% @doc check is number is polindrome
-spec is_palindrome(L) -> Result when
    L :: nonempty_list(),
    Result :: false | nonempty_list().

is_palindrome(L) -> L =:= lists:reverse(L).

% @doc generate minimum X digit number
-spec genmin(X) -> Result when
    X :: pos_integer(),
    Result :: pos_integer().

genmin(X) -> list_to_integer("1" ++ lists:duplicate(X-1,48)).

% @doc generate maximum X digit number
-spec genmax(X) -> Result when
    X :: pos_integer(),
    Result :: pos_integer().

genmax(X) -> list_to_integer("9" ++ lists:duplicate(X-1,57)).
