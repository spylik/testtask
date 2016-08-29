%% --------------------------------------------------------------------------------
%% File:    task3.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc Task3:
%% The prime factors of 13195 are 5, 7, 13 and 29.
%% What is the largest prime factor of the number 600851475143 ?
%% --------------------------------------------------------------------------------

-module(task3).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-export([default/0]).

% @doc hardcoded values and specs for test task.
% By default we going to find largest prime factor for 600851475143.
-spec default() -> Result when
    Result :: 6857. % we can hardcode output type cuz we using hardcoded parameter

default() -> max_factor(600851475143).

% @doc find max prime factor for number
-spec max_factor(Number) -> Result when
    Number :: pos_integer(),
    Result :: pos_integer().

max_factor(Number) -> max_factor(Number,2,2).

% @doc main recursion for max_factor/1
-spec max_factor(Number, CurrentFactor, MaxFactor) -> Result when
    Number :: pos_integer(),
    CurrentFactor :: pos_integer(),
    MaxFactor :: pos_integer(),
    Result :: pos_integer().
    
max_factor(1,_,MF) -> MF;
max_factor(Number,CF,MF) when Number rem CF =:= 0 ->
    max_factor(Number div CF, CF, max(CF,MF));
max_factor(Number,CF,MF) ->
    max_factor(Number, CF+1, MF).
