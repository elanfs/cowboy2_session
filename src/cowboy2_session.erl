-module(cowboy2_session).

%-- app
-export([start/0]).

%-- api
-export([set/3, set/4, get/2, mget/2, get_keys/1, delete/1, flush/0, keys/0]).
-export([expire/1, touch/1]). % TODO proposed api

-export([new_session/1, get_session/1]).

% -behaviour(cowboy_middleware).
%-- cowboy middleware callback
-export([execute/2]).

% -type session_kv() :: #{
%     id => binary()
%   , kv_map => map()
%   , expire_in => integer()
% }.
-exported_types([session_id/0, session_key/0]).
-type session_id() :: binary().
-type session_key() :: any().

ttl_one_day() -> 86400000.
ttl_default() -> ttl_one_day() * 30.

start() ->
  application:ensure_all_started(?MODULE).

% expire in milliseconds 1 second = 1000 ms
-spec set(session_id(), session_key(), any()) -> {ok, session_key()} | {error, any()}.
set(Id, Key, Val) -> cowboy2_session_svr:set(Id, Key, Val, ttl_default()).

-spec set(session_id(), any(), any(), Expire::non_neg_integer()) -> {ok, Key::any()} | {error, any()}.
set(Id, Key, Val, Expire) -> cowboy2_session_svr:set(Id, Key, Val, Expire).

-spec get(session_id(), Key::any()) -> not_found | {ok, any()} | {error, any()}.
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
new_session(Req) ->
  Id = cowboy2_session_g:gen_session_id(),
  Secret = cowboy2_session_g:get_session_secret(),
  EncryptedId = cowboy2_session_g:encrypt(Secret, Id),
  % io:format("new_session: ~p:~p~n", [Id, EncryptedId]),
  LastSeen = cowboy2_session_g:unixtime(),
  cowboy2_session:set(Id, last_seen, LastSeen, ttl_default()),
	Req1 = cowboy_req:set_resp_cookie(
    <<"esessionid">>, EncryptedId,
    Req, #{
       path => <<"/">>
      ,max_age => ttl_default() div 1000
    }),
  {Id, Req1}.

%-spec get_session(Req::any()) -> {session_id(), Req::any()}.
get_session(Req) ->
  MatchCookies = [{esessionid, [], <<>>}],
  case cowboy_req:match_cookies(MatchCookies, Req) of
    #{esessionid := <<>>} ->
        % io:format("empty cookie~n"),
        ?MODULE:new_session(Req);

      #{esessionid := Esession} ->
        Secret = cowboy2_session_g:get_session_secret(),
        SessId = cowboy2_session_g:decrypt(Secret, Esession),
        % io:format("existing session: ~p~n", [SessId]),
        % check if session valid
        case cowboy2_session:get_keys(SessId) of
          not_found -> ?MODULE:new_session(Req);
          _Keys -> {SessId, Req}
        end;

      _ ->
        % io:format("new session ~n"),
        ?MODULE:new_session(Req)
    end.
