[![CircleCI](https://circleci.com/gh/elanfs/cowboy2_session/tree/master.svg?style=svg)](https://circleci.com/gh/elanfs/cowboy2_session/tree/master)

cowboy2_session
=====

A Session Store middleware for Cowboy2 middleware

## Features

1. Works with Cowboy2
1. Stores session data in mnesia disc_copies table

## Try Out

```
make shell
```

## Usage

Session Id will be passed into Opts in the cowboy handler `init(Req, Opts)` callback

```erlang
init(Req, [{esessionid, Id}] = Opts) ->
  io:format("session_id : ~p~n", [Id]),
  {ok, Req, Opts}.
```

Storing Data, Expiry is milliseconds

```
cowboy2_session:set(<<"1111">>, Key, Value, Expiry)
```

Retreiving Data

```
cowboy2_session:get(<<"1111">>, Key, Value)
```

Retrieving keys related to Session Id

```
cowboy2_session:get_keys(<<"1111">>, Key, Value)
```

## Getting Started

### Register middleware

To auto start cowboy2_session, add `cowboy2_session` into your app.src

or start manually `cowboy2_session:start`

Add to Cowboy2 as middleware

example

```erlang
cowboy:start_clear(http, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  ,middlewares => [cowboy_router, cowboy2_session, cowboy_handler]
}).
```

## Getting SessionId

```erlang
init(Req0, Opts) ->
  {SessionId, Req1} = case proplists:get_value(esessionid, Opts) of
    undefined -> cowboy2_session:new_session(Req0)
    ;ExistingSessionId -> {ExistingSessionId, Req0}
  end,

  cowboy2_session:set(SessionId, foo, bar),
  Req2 = cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain">>
  }, <<"Hello world!">>, Req1),
  {ok, Req2, Opts}.
```
