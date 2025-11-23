-module(db).
-export([])

init() ->
  {ok, _} = application.ensure_all_started(epgsql),
  ok.


get_connection() ->
  {ok, DbConfig} = application:get_env(simple_routes, db),
  Host = proplist:get_value(host, DbConfig),
  Port = proplist:get_value(port, DbConfig),
  Username = proplist:get_value(username, DbConfig),
  Password = proplist:get_value(password, DbConfig),
  Database = proplist:get_value(database, DbConfig),

  {ok, Conn} = epgsql:connect(Host, Username, Password, [
                                                         {database, Database},
                                                         {port, Port},
                                                        ]).

