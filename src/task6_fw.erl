%% --------------------------------------------------------------------------------
%% File:    task6_fw.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc Task6:
%% Implement an Erlang/OTP application that has a POST /capture method which 
%% receives input requests like the attached file Test.xml. If PROD_COVER_GTIN, 
%% PROD_NAME can be captured from a request then the request is accepted, the 
%% values of PROD_COVER_GTIN, PROD_NAME and additional values of PROD_DESC, 
%% BRAND_OWNER_NAME are written to external CSV file with 4 columns:
%% GTIN,NAME,DESC,COMPANY.
%% 
%% NOTE: if PROD_DESC, BRAND_OWNER_NAME are missed in an input request then empty 
%% values will be saved to the CSV.
%%
%% This is gen_server for save data to file.
%% --------------------------------------------------------------------------------

-module(task6_fw).

-include("deps/teaser/include/utils.hrl").

% gen server is here
-behaviour(gen_server).

% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% public api 
-export([
        start_link/1,
        stop/0
    ]).

% we will use ?MODULE as servername
-define(SERVER, ?MODULE).

-record(state, {ioDevice}).

% ----------------------------- gen_server part --------------------------------

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename], []).

stop() ->
    gen_server:stop(?SERVER).

init([Filename]) ->
%    Filename = "task6.csv",
    {ok, IoDevice} = file:open(Filename,[append]),
    {ok, #state{ioDevice = IoDevice}}.

%--------------handle_call----------------

% handle_call for all other thigs
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
%-----------end of handle_call-------------


%--------------handle_cast-----------------

handle_cast({newreq, GTIN, NAME, FullData}, State) ->
    DESC = case lists:keyfind("PROD_DESC",1,FullData) of
        {"PROD_DESC", Desc} -> Desc;
        false -> ""
    end,
    COMPANY = case lists:keyfind("BRAND_OWNER_NAME",1,FullData) of 
        {"BRAND_OWNER_NAME", Company} -> Company;
        false -> ""
    end,
    io:fwrite(State#state.ioDevice, "~p,~p,~p,~p~n",[GTIN, NAME, DESC, COMPANY]),
    {noreply, State};

% handle_cast for all other thigs
handle_cast(_Msg, State) ->
    {noreply, State}.
%-----------end of handle_cast-------------


%--------------handle_info-----------------

%% handle_info for all other thigs
handle_info(_Msg, State) ->
    {noreply, State}.
%-----------end of handle_info-------------


terminate(Reason, State) ->
    file:close(State#state.ioDevice),
    {noreply, Reason, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ============================= end of gen_server part =========================
