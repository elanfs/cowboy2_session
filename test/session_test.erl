-module(session_test).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  application:set_env(mnesia, dir, "_build/test"),
  cowboy2_session_svr:delete_schema(),
  cowboy2_session_svr:create_schema(),
  cowboy2_session_svr:create_db(),
  cowboy2_session:start(),
  cowboy2_session:set(1, foo, bar, 2000),
  cowboy2_session:set(1, foo2, bar2, 2000),
  ?assertEqual({ok, bar}, cowboy2_session:get(1, foo)),
  ?assertEqual({ok, bar2}, cowboy2_session:get(1, foo2)),
  cowboy2_session:delete(1),
  ?assertEqual(0, length(cowboy2_session:keys())).
