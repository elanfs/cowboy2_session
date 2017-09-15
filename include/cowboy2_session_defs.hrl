% db table

-define(S_SESS_KEY, "my$3Cr3tKeY").
-define(S_TTL_ONE_DAY, 86400000). % 1 day in ms
-define(S_SESS_DEFAULT_TTL, ?S_TTL_ONE_DAY * 30). % ttl in millis
-define(S_DEFAULT_PURGE_TIMER, 10000). % check every 10 sec

-record(cowboy2_session, {
  id :: binary(), % session id
  kv_map :: map(),
  expire_in  :: Millis::integer()
}).

