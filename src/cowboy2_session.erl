-module(cowboy2_session).
-behaviour(cowboy_middleware).

%-- app
-export([start/0]).

%-- api
-export([set/3, set/4, get/2, mget/2, get_keys/1, delete/1, flush/0, keys/0]).
-export([expire/1, touch/1]). % TODO proposed api

%-- local db function
-export([check_db/0, create_schema/0, delete_schema/0, create_db/0, delete_db/0]).

%-- cowboy middleware callback
-export([execute/2]).

-include("cowboy2_session_defs.hrl").

start() ->
  application:ensure_all_started(?MODULE).

% expire in milliseconds 1 second = 1000 ms
-spec set(Id::binary(), any(), any()) -> {ok, Key::any()} | {error, any()}.
set(Id, Key, Val) -> cowboy2_session_svr:set(Id, Key, Val, ?S_SESS_DEFAULT_TTL).

-spec set(Id::binary(), any(), any(), Expire::non_neg_integer()) -> {ok, Key::any()} | {error, any()}.
set(Id, Key, Val, Expire) -> cowboy2_session_svr:set(Id, Key, Val, Expire).

-spec get(Id::binary(), Key::any()) -> not_found | {ok, any()} | {error, any()}.
get(Id, Key) -> cowboy2_session_svr:get(Id, Key).

% get multiple keys
mget(Id, Keys) -> cowboy2_session_svr:mget(Id, Keys).

get_keys(Id) -> cowboy2_session_svr:get_keys(Id).

delete(Id) -> cowboy2_session_svr:delete(Id).

flush() -> cowboy2_session_svr:flush().

keys() -> 
  mnesia:dirty_all_keys(cowboy2_session).

expire(_Key) -> ok.
touch(_Key) -> ok.


check_db() ->
  % check if schema created
  case mnesia:table_info(schema, disc_copies) of
    [] ->
      % io:format("fresh db, creating schema on node: ~p~n", [node()]),
      ?MODULE:create_schema();
    _ -> ok
  end,
  Tables = mnesia:system_info(tables),
  case lists:member(?MODULE, Tables) of
    false ->
      % io:format("no tables found, creating...~n"),
      ?MODULE:create_db();
    true ->
      % io:format("found tables, loading: ~p~n", [Tables]),
      mnesia:wait_for_tables(Tables, 10000),
      ok
  end.

create_schema() ->
  mnesia:stop(),
  mnesia:create_schema([node()]),
  mnesia:start().

delete_schema() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:start().

create_db() ->
  mnesia:create_table(?MODULE, [
    {attributes, record_info(fields, ?MODULE)}
    ,{disc_copies, [node()]}
  ]).
  
delete_db() -> mnesia:delete_table(?MODULE).
  

%--- overload cowboy middleware callback
%---
execute(Req0, #{ handler_opts := Opts } = Env) ->
	case cowboy_req:path(Req0) of
    <<"/public">> -> {ok, Req0, Env}; % skip session for static files
    <<"/public",_/binary>> -> {ok, Req0, Env}; % skip session for static files
    _ ->
      {SessId, Req1} = get_session(Req0),

      % io:format("Env: ~p~n", [Env]),
      
      Env2 = Env#{ handler_opts := [{esessionid, SessId} | Opts] },

      {ok, Req1, Env2}

  end.

%--- private

%-spec get_session(Req::any()) -> {session_id(), Req::any()}.
get_session(Req) ->
    MatchCookies = [{esessionid, [], <<>>}],
    case cowboy_req:match_cookies(MatchCookies, Req) of
      #{esessionid := <<>>} ->
          % io:format("empty cookie~n"),
          new_session(Req);
    
        #{esessionid := Esession} ->
          SessId = decrypt(?S_SESS_KEY, Esession),
          % io:format("existing session: ~p~n", [SessId]),
          % check if session valid
          case cowboy2_session:get_keys(SessId) of
            not_found -> new_session(Req);
            _Keys -> {SessId, Req}
          end;
        
        _ ->
          % io:format("new session ~n"),
          new_session(Req)
    end.


new_session(Req) ->
  Id = gen_session_id(),
  EncryptedId = encrypt(?S_SESS_KEY, Id),
  % io:format("new_session: ~p:~p~n", [Id, EncryptedId]),
  LastSeen = cowboy2_session_g:unixtime(),
  cowboy2_session:set(Id, last_seen, LastSeen, ?S_SESS_DEFAULT_TTL),
	Req1 = cowboy_req:set_resp_cookie(
    <<"esessionid">>, EncryptedId,
    Req, #{
       path => <<"/">>
      ,max_age => ?S_SESS_DEFAULT_TTL div 1000    
    }),
  {Id, Req1}.



gen_session_id() ->
  NumBytes = 4,
  list_to_binary(integer_to_list(
    crypto:bytes_to_integer(crypto:strong_rand_bytes(NumBytes)))).


encrypt(Key, Plain) ->
  S0 = crypto:stream_init(rc4, Key),
  {_S1, Cipher} = crypto:stream_encrypt(S0, Plain),
  base64:encode(Cipher).

decrypt(Key, Base64Cipher) ->
  S0 = crypto:stream_init(rc4, Key),
  {_S1, Plain} = crypto:stream_decrypt(S0, base64:decode(Base64Cipher)),
  Plain.
