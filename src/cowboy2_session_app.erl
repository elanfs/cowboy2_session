-module(cowboy2_session_app).
-behaviour(application).

-export([start/2
        ,stop/1]).

-include_lib("kernel/include/logger.hrl").

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
      ?LOG_INFO("fresh db, creating schema on node: ~p~n", [Nodes]),
      cowboy2_session_svr:create_schema();
    _ -> ok
  end.

check_or_create_tables(AppTables) ->
  Tables = mnesia:system_info(tables),
  Results = lists:foldr(fun(Name, Acc) ->
    Res = lists:member(Name, Tables),
    [Res | Acc]
  end, [], AppTables),

  case not lists:member(false, Results) of
    false ->
      ?LOG_INFO("tables not found, creating..."),
      cowboy2_session_svr:create_db();
    true ->
      ?LOG_INFO("found existing tables, loading: ~p~n", [AppTables]),
      case mnesia:wait_for_tables(AppTables, 10000) of
        ok ->
          ?LOG_INFO("done loading tables: ~p~n", [AppTables]);
        {error, Reason} ->
          ?LOG_ERROR("error loading tables: ~p~n", [Reason]);
        {timeout, TimeoutTables} ->
          ?LOG_WARNING("timeout loading tables: ~p~n", [TimeoutTables])
      end
  end.
