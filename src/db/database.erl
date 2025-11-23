-module(database).
-export([init/0, execute/1, execute/2, query/1, query/2]).

init() ->
  {ok, _} = application:ensure_all_started(epgsql),
  ok.

get_connection() ->
  {ok, DbConfig} = application:get_env(simple_routes, db),
  Host = proplists:get_value(host, DbConfig),
  Port = proplists:get_value(port, DbConfig),
  Username = proplists:get_value(username, DbConfig),
  Password = proplists:get_value(password, DbConfig),
  Database = proplists:get_value(database, DbConfig),

  epgsql:connect(Host,
                 Username,
                 Password,
                 [{database, Database},{port, Port}]).

execute(SQL) ->
  execute(SQL, []).

execute(SQL, Params) ->
  {ok, Conn} = get_connection(),
  try
    epgsql:equery(Conn, SQL, Params),
  catch
    _:Error -> {error, Error}
  after
    epgsql:close(Conn),
  end.

query(SQL) ->
  query(SQL, []).

query(SQL, Params) ->
  case execute(SQL, Params) of
    {ok, _Columns, Rows} -> {ok, Rows};
    {ok, _Count} -> {ok, []};
    Error -> Error
  end.

