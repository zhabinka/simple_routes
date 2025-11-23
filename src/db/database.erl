-module(database).
-export([init/0, execute/1, execute/2, query/1, query/2]).

init() ->
  {ok, _} = application:ensure_all_started(epgsql),
  {ok, DbConfig} = application:get_env(simple_routes, db),

  % https://github.com/wgnet/epgsql_pool
  Pool_size = proplists:get_value(pool_size, DbConfig),
  Params = #{host => proplists:get_value(host, DbConfig),
             port => proplists:get_value(port, DbConfig),
             username => proplists:get_value(username, DbConfig),
             password => proplists:get_value(password, DbConfig),
             database => proplists:get_value(database, DbConfig)},

  epgsql_pool:start(db_pool, Pool_size, Pool_size, Params).

execute(SQL) -> execute(SQL, []).
execute(SQL, Params) -> epgsql_pool:query(db_pool, SQL, Params).

query(SQL) -> query(SQL, []).
query(SQL, Params) ->
  case execute(SQL, Params) of
    {ok, _Columns, Rows} -> Rows;
    {ok, _Count} -> [];
    Error -> Error
  end.
