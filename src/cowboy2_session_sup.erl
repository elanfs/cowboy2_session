-module(cowboy2_session_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
  RestartStrategy = {one_for_one, 1, 5},
  Procs = [
    {cowboy2_session_svr,
        {cowboy2_session_svr, start_link, []},
        permanent,
        5000,
        worker,
        [cowboy2_session_svr]
    }
  ],
  {ok, {RestartStrategy, Procs}}.

