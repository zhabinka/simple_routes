-module(migrations).
-export([generate/1, list/0, migrate/0, rollback/0]).
-define(DIR, "priv/migrations").

generate(Name) ->
    FileName = string:join([timestamp(), "_", Name, ".sql"], ""),
    FilePath = filename:join(?DIR, FileName),
    Template = io_lib:format(
                 "-- ~s~n"
                 "-- :up~n"
                 "CREATE TABLE users (id SERIAL PRIMARY KEY, name VARCHAR(255));~n"
                 "CREATE INDEX users_name_idx ON users(name);~n~n"

                 "-- :down~n"
                 "DROP INDEX users_name_idx;~n"
                 "DROP TABLE users;~n",
                 [FilePath]),

    file:write_file(FilePath, Template).

migrate() -> exec(up).
rollback() -> exec(down).

% migrate(Migration) -> exec(Migration, up).
% rollback(Migration) -> exec(Migration, down).

list() ->
    {ok, Files} = file:list_dir(?DIR),
    lists:map(fun(Filename) ->
        {ok, Queries} = eql:compile(filename:join([?DIR, Filename])),
        {Filename, Queries}
    end, lists:sort(Files)).

%%
%% Private
%%

exec(Mode) ->
    {ok, Migrations} = file:list_dir(?DIR),
    lists:foreach(fun(Migration) -> exec(Migration, Mode) end, lists:sort(Migrations)).

exec(Migration, Mode) ->
    [Filename, _] = string:split(Migration, ".", trailing),
    {Version, Name} = string:to_integer(Filename),
    Params = [Version, string:trim(Name, leading, "_")],

    MigrationsUdpateQueries = [{up, "INSERT INTO migrations (version, name) VALUES ($1, $2) ON CONFLICT DO NOTHING"},
                               {down, "DELETE FROM migrations WHERE version = $1 AND name = $2"}],

    case database:execute(proplists:get_value(Mode, MigrationsUdpateQueries), Params) of
        {ok, 0} ->
            skip;
        {ok, _} ->
            {ok, Queries} = eql:compile(filename:join([?DIR, Migration])),
            ModeQueries = proplists:get_value(Mode, Queries),
            lists:foreach(
              fun(Query) -> database:execute(Query) end,
              string:split(ModeQueries, ";")
             );
        {error, {error, error, <<"42P01">>, _, _, _}} ->
            init(),
            exec(Migration, Mode)
    end.

init() ->
    {ok, _, _} = database:execute(
        "CREATE TABLE migrations ("
        "version BIGINT PRIMARY KEY,"
        "name VARCHAR(255),"
        "inserted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)").

timestamp() ->
    {{Y,M,D},{H,MM,S}} = calendar:universal_time(),
    io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B", [Y,M,D,H,MM,S]).
