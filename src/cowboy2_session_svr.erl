-module(cowboy2_session_svr).

%- public API
-export([set/4, get/2, mget/2, get_keys/1, delete/1, flush/0]).

% usual
-export([start/0, start_link/0, stop/0]).
% inherited
-export([init/1, handle_call/3, handle_cast/2
  ,handle_info/2,terminate/2, code_change/3]).

%-- local db function
-export([check_db/0, create_schema/0, delete_schema/0, create_db/0, delete_db/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(cowboy2_session, {
  id :: binary(), % session id
  kv_map :: map(),
  expire_in  :: Millis::integer()
}).

-record(cowboy2_session_secret, {
  id :: non_neg_integer(),
  value :: binary()
}).

 % ms
ttl_purge_default() -> 10000.

%-- Expire in millis
set(Id, Key, Val, Expire) ->
  gen_server:call(?MODULE, {set, Id, Key, Val, Expire}).

get(Id, Key) ->
  gen_server:call(?MODULE, {get, Id, Key}).

mget(Id, Keys) ->
  lists:foldl( fun (Key, Acc) ->
      case cowboy2_session:get(Id, Key) of
        {ok, Val} -> Acc ++ [Val];
        _ -> Acc ++ [not_found]
      end
  end
  ,[]
  ,Keys).

get_keys(Id) ->
  gen_server:call(?MODULE, {get_keys, Id}).

delete(Id) -> gen_server:call(?MODULE, {delete, Id}).

flush() -> mnesia:clear_table(cowboy2_session).

start() ->
  mnesia:start(),
  ?MODULE:check_db(),
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).


start_link() ->
  ?MODULE:check_db(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, terminate).


init([]) ->
  process_flag(trap_exit, true),
  % io:format("~p starting~n", [?MODULE]),
  {ok, #{}, 0}.

handle_call({set, Id, Key, Val, Expire}, _From, State) ->
  case mnesia:transaction(
    update_key(Id,Key, Val, cowboy2_session_g:unixtime() + Expire)
  ) of
    {atomic, ok} -> {reply, {ok, Key}, State};
    Err -> {reply, {error, Err}, State}
  end;

handle_call({get, Id, Key}, _From, State) ->
  case mnesia:transaction(read_key(Id,Key)) of
    {atomic, not_found} -> {reply, not_found, State};
    {atomic, Res} -> {reply, {ok, Res}, State};
    _ -> {reply, error, State}
  end;

handle_call({get_keys, Id}, _From, State) ->
  case mnesia:dirty_read(cowboy2_session, Id) of
    [] ->
      {reply, not_found, State};
    [Rec] ->
      {reply, maps:keys(Rec#cowboy2_session.kv_map), State}
  end;

handle_call({delete, Id}, _From, State) ->
  Res = mnesia:transaction(fun() ->
    mnesia:delete({cowboy2_session, Id}) end),

  {reply, Res, State};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  % io:format("unhandled_call: ~p~n", [Msg]),
  {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
  % io:format("handle_cast: ~p~n", [Msg]),
  {noreply, State}.


handle_info(timeout, State) ->
  generate_encryption_key(),
  % schedule purge event
  erlang:send_after(ttl_purge_default(), self(), purge),
  {noreply, State};

handle_info(purge, State) ->
  CurrTime = cowboy2_session_g:unixtime(),
  T = fun() ->
    Q = qlc:q([
      X || X <- mnesia:table(cowboy2_session)
      ,X#cowboy2_session.expire_in < CurrTime
    ]),
    case qlc:e(Q) of
      [] -> 0;
      Objs ->
        % io:format("delete many objects: ~p~n", [Objs]),
        lists:foldl(fun(Rec, Acc) ->
            mnesia:delete_object(Rec),
            Acc+1
        end,0, Objs)
    end
  end,
  _Ret = mnesia:transaction(T),
  %io:format("purged: ~p~n", [Ret]),
  erlang:send_after(ttl_purge_default(), self(), purge),
  {noreply, State};

handle_info(_Msg, State) ->
  % io:format("handle_info: ~p~n", [Msg]),
  {noreply, State}.


code_change(_OldVsn, N, _Extra) ->
  % io:format("code_change ~p,~p,~p", [OldVsn, N, Extra]),
  {ok, N}.


terminate(_Reason, _State) ->
  io:format("~p stopping~n", [?MODULE]),
  ok.

%%--------------
%% Mnesia helpers
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
  mnesia:create_table(cowboy2_session, [
    {attributes, record_info(fields, cowboy2_session)}
    ,{disc_copies, [node()]}
  ]),
  mnesia:create_table(cowboy2_session_secret, [
    {attributes, record_info(fields, cowboy2_session_secret)}
    ,{disc_copies, [node()]}
  ]).


delete_db() -> mnesia:delete_table(?MODULE).


%-- Private
generate_encryption_key() ->
  case mnesia:dirty_read(cowboy2_session_secret, 0) of
    [] ->
      Secret = base64:encode(crypto:strong_rand_bytes(64)),
      mnesia:dirty_write(#cowboy2_session_secret{
        id = 0, value = Secret
      })
    ; _ ->
      ok
  end.

read_key(Id, Key) ->
  fun() ->
    case mnesia:read(cowboy2_session, Id, write) of
      [] -> not_found;

      [Rec] -> maps:get(Key, Rec#cowboy2_session.kv_map, not_found);

      Other -> Other
    end
  end.

update_key(Id, Key, Val, Expire) ->
  fun() ->
    case mnesia:read(cowboy2_session, Id, write) of
      [] ->
        mnesia:write(#cowboy2_session{
          id = Id,
          kv_map = maps:put(Key,Val, #{}),
          expire_in = Expire
        });

      [Rec] ->
        KVMap2 = maps:put(Key,Val, Rec#cowboy2_session.kv_map),
        mnesia:write(Rec#cowboy2_session{ kv_map = KVMap2 });

      Other -> Other
    end
  end.
