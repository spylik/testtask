-module(task6_sup).

% supervisor is here
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> Result when
    Result :: supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> Result when
    Result :: {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.

init([]) ->
    RestartStrategy = {one_for_one,10,10}, 

    FileWriter = {
        task6_fw,
        {task6_fw, start_link, ["task6.csv"]},
        permanent,
        5000,
        worker,
        [task6_fw]
    },

    Childrens = [FileWriter],
    {ok, {RestartStrategy, Childrens}}.
