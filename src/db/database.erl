-module(database).
-export([execute/1, execute/2, query/1, query/2]).

execute(SQL) -> execute(SQL, []).
execute(SQL, Params) -> epgsql_pool:query(db_pool, SQL, Params).

query(SQL) -> query(SQL, []).
query(SQL, Params) ->
  case execute(SQL, Params) of
    {ok, _Columns, Rows} -> Rows;
    {ok, _Count} -> [];
    Error -> Error
  end.
