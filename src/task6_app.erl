%% --------------------------------------------------------------------------------
%% File:    task6_app.erl
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
%% --------------------------------------------------------------------------------

-module(task6_app).
-behaviour(application).

-export([
        start/2,
        stop/1
    ]).

% @doc start application and start cowboy listerner
-spec start(Type, Args) -> Result when
    Type :: application:start_type(),
    Args :: term(),
    Result :: {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", task6, []}
		]}
	]),

	{ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),

    task6_sup:start_link().

% @doc stop application (we stopping cowboy listener here)
-spec stop(State) -> Result when
    State :: term(),
    Result :: ok.

stop(_State) ->
    _ = cowboy:stop_listener(http),
    ok.
