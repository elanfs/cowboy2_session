-module(cowboy2_session_app).
-behaviour(application).

-export([start/2
        ,stop/1]).


start(_StartType, _StartArgs) ->
  check_db(),
  cowboy2_session_sup:start_link().

stop(_State) ->
  ok.


check_db() ->
  check_or_create_schema(node()),

  CheckTables = [cowboy2_session, cowboy2_session_secret],
  check_or_create_tables(CheckTables).

check_or_create_schema(Nodes) ->
  % check if schema created
  case mnesia:table_info(schema, disc_copies) of
    [] ->
      io:format("fresh db, creating schema on node: ~p~n", [Nodes]),
      cowboy2_session_svr:create_schema();
    _ -> ok
  end.

check_or_create_tables(Names) ->
  Tables = mnesia:system_info(tables),
  Results = lists:foldr(fun(Name, Acc) ->
    Res = lists:member(Name, Tables),
    [Res | Acc]
  end, [], Names),

  case not lists:member(false, Results) of
    false ->
      io:format("tables not found, creating...~n"),
      cowboy2_session_svr:create_db();
    true ->
      io:format("found existing tables, loading: ~p~n", [Tables]),
      case mnesia:wait_for_tables(Tables, 10000) of
        ok ->
          io:format("done loading tables: ~p~n", [Tables]);
        {error, Reason} ->
          io:format("error loading tables: ~p~n", [Reason]);
        {timeout, TimeoutTables} ->
          io:format("timeout loading tables: ~p~n", [TimeoutTables])
      end
  end.
