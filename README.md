# Haskell + Stack + Docker

A pattern for running a Haskell app with docker for dev and prod.

Stack has docker integration where stack wraps docker in a DSL so developers don't need to 'deal with docker themselves'.

```yaml
docker:
  enable: true
```

I dislike hiding docker details behind an app level DSL. I much prefer this pattern where docker + high level script stuff is separate and wraps the stack and haskell level application stuff and each can easily be changed independently.

## This is from a servant + persistent boilerplate

https://github.com/parsonsmatt/servant-persistent

It takes about 30 minutes to build the first time, but everything is cached after that.

## How To

### Run within the dev container

```bash
auto/dev-environment
```

### Normal dev workflow

Usually you want to:

```bash
auto/dev-environment bash
```

then:

```bash
support/dev/run # or stack test
```

as needed. This avoids starting the docker container and running db migrations with each app run.

### To Run Without Docker

Requires [Haskell Stack](https://www.haskellstack.org/) on your host.

```bash
auto/up-dev-dependencies

support/dev/run

auto/down-dev-dependencies
```

### Build the container with just the executable for production (and usually release to a registry)

```bash
auto/release
```

## Running in CI

Due to the time + memory it takes to setup stack/ghc and compile all the dependencies it isn't desirable to do this on CI agents.

Instead this is done separately and a base image is created with everything baked in. It is recommended to push a new version of the base container when there are significant stackage version or library changes.

```bash
auto/release-ci-base
```

### Testing the prod container locally

It can be useful to test with the cut down prod container.

```bash
auto/run-prod-local
```

### Deploying To Prod

Requires a docker registry to deploy to (via `auto/release`) and some way to deploy a docker container with config.

### Others

```bash
auto/build
auto/ghci
auto/test
```

## Why?

### Why use a custom snapshot?

This allows extra dependencies to be baked into the ci base image. Otherwise they are built locally into `.stack-work`

### Why not use the official fpco stack-build containers?

For some reason the official stack docker images are very large. 8GB vs 1.3GB for official haskell, which include stack. (Although looks like there is a new [stack-build-small](https://hub.docker.com/r/fpco/stack-build-small) at 2.67GB)

Also, the stack image is based off of (old) Ubuntu, while official haskell is based on (latest) debian. Debian is a much better fit for a docker container OS and it's simpler if the build and prod OS are the same.
