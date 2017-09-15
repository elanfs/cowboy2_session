cowboy2_session
=====

A (work in progress) Session Store for Cowboy2 middleware, (last tested with Cowboy2 rc2)

## Features

1. works with Cowboy2
1. stores session data in mnesia disc_copies table

## Try Out

```
make shell
```

## Usage

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
{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  ,middlewares => [cowboy_router, cowboy2_session, cowboy_handler]
}).
```

