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

