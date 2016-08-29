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
-type state() :: #state{}.

% ----------------------------- gen_server part --------------------------------

% @doc start api
-spec start_link(Filename) -> Result when
    Filename :: file:filename(),
    Result :: {ok,Pid} | ignore | {error,Error},
    Pid :: pid(),
    Error :: {already_started,Pid} | term().

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename], []).

% @doc stop api. Defaul is sync call gen_server:stop
-spec stop() -> ok.

stop() ->
    gen_server:stop(?SERVER).

% @doc gen_server init. We going to open file and keep ioDevice in state
-spec init([Filename]) -> Result when
    Filename :: file:filename(),
    Result :: {ok, state()}.

init([Filename]) ->
    IoDevice = case filelib:is_regular(Filename) of
        false ->
            {ok, IoDev} = file:open(Filename,[write,binary]),
            ok = file:write(IoDev,unicode:encoding_to_bom(utf8)),
            IoDev;
        true -> 
            {ok, IoDev} = file:open(Filename,[append]),
            IoDev
    end,
    ok = io:setopts(IoDevice,[{encoding,utf8}]),

    {ok, #state{ioDevice = IoDevice}}.

%--------------handle_call----------------

% @doc callbacks for gen_server handle_call.
-spec handle_call(Message, From, State) -> Result when
    Message :: term(),
    From :: {pid(), Tag},
    Tag :: term(),
    State :: term(),
    Result :: {reply, ok, State}.

% handle_info for all other thigs
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
%-----------end of handle_call-------------


%--------------handle_cast-----------------

% @doc callbacks for gen_server handle_cast.
-spec handle_cast(Message, State) -> Result when
    Message         :: NewReq,
    NewReq          :: {'newreq', binary(), binary(), [{binary(),binary()}]},
    State           :: term(),
    Result          :: {noreply, State}.

handle_cast({newreq, GTIN, NAME, FullData}, State) ->
    DESC = case lists:keyfind(<<"PROD_DESC">>,1,FullData) of
        {<<"PROD_DESC">>, Desc} -> Desc;
        false -> <<>>
    end,
    COMPANY = case lists:keyfind(<<"BRAND_OWNER_NAME">>,1,FullData) of 
        {<<"BRAND_OWNER_NAME">>, Company} -> Company;
        false -> <<>>
    end,
    io:fwrite(State#state.ioDevice, "~ts,~ts,~ts,~ts~n",[GTIN,NAME,DESC,COMPANY]),
    {noreply, State};

% handle_cast for all other thigs
handle_cast(_Msg, State) ->
    {noreply, State}.
%-----------end of handle_cast-------------


%--------------handle_info-----------------

% @doc callbacks for gen_server handle_info.
-spec handle_info(Message, State) -> Result when
    Message :: term(),
    State   :: term(),
    Result  :: {noreply, State}.

% handle_info for all other thigs
handle_info(_Msg, State) ->
    {noreply, State}.
%-----------end of handle_info-------------

-spec terminate(Reason, State) -> term() when
    Reason :: 'normal' | 'shutdown' | {'shutdown',term()} | term(),
    State :: term().

terminate(Reason, State) ->
    _ = file:close(State#state.ioDevice),
    {noreply, Reason, State}.

-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term(),
    State :: term(),
    Extra :: term(),
    Result :: {ok, NewState},
    NewState :: term().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ============================= end of gen_server part =========================
