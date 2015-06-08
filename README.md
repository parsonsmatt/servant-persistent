# servant-persistent

[Servant](https://haskell-servant.github.io/) is an awesome Haskell library for writing web APIs. It uses the type system in a way that can only be described as magic to generate type safe routes as well as clients.

[Persistent](http://www.yesodweb.com/book/persistent) is another awesome Haskell library for querying databases. It manages migrations, your schema, and querying to make data transactions mostly painless.

For some reason, no one had created an example on how to use these guys together. I put together this minimal example to show an example implementation, along with some resource management and basic error handling.

I wrote a [blog post](http://www.parsonsmatt.org/programming/2015/06/07/servant-persistent.html) that goes into a bit more detail, as well as having some exercises to work on.

## Requirements:

You will need PostgreSQL installed and listening on port 5432. The default configuration uses a database name `perservant` with username/password test:test.

## The API:

- `/users` returns a list of all users in the database
- `/users/:name` returns the first user whose name is `:name`, and returns 404 if the user doesn't show up.

## src/Main.hs

`main` starts off by pulling some settings from the environment, creating a connection pool, running the migrations, and finally running the app.

## src/Api.hs

This source contains the actual API definition.

## src/Config.hs

Contains the `runDb`, `makePool`, and `Config` definitions.

## src/Models.hs

Contains fairly typical Persistent schema definitions.
