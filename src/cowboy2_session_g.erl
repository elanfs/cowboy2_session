% common functions
-module(cowboy2_session_g).

-export([unixtime/0]).

unixtime() ->
  {Mega, Sec, Micro} = os:timestamp(),
  erlang:trunc((Mega * 1000000 + Sec)*1000 + Micro*0.001).