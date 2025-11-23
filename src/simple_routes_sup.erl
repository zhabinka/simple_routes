%%%-------------------------------------------------------------------
%% @doc simple_routes top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_routes_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, DbConfig} = application:get_env(simple_routes, db),

    % https://github.com/wgnet/epgsql_pool
    Pool_size = proplists:get_value(pool_size, DbConfig),
    Params = #{host => proplists:get_value(host, DbConfig),
               port => proplists:get_value(port, DbConfig),
               username => proplists:get_value(username, DbConfig),
               password => proplists:get_value(password, DbConfig),
               database => proplists:get_value(database, DbConfig)},

    PoolChild = #{id => epgsql_pool,
                  start => {epgsql_pool, start, [db_pool, Pool_size, Pool_size, Params]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [epgsql_pool]},

    {ok, { {one_for_all, 0, 1}, [PoolChild]} }.

%%====================================================================
%% Internal functions
%%====================================================================
