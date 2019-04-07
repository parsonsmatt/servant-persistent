FROM haskell@sha256:b8349afbd4be548903521e2e3a987d530999c4e8d3c3fc1c81401a57631eeefb

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    libpq-dev \
 && rm -rf /var/lib/apt/lists/*

RUN stack --resolver ghc-8.2.2 setup \
 && rm /root/.stack/programs/x86_64-linux/ghc-8.2.2.tar.xz \
 && rm -rf /root/.stack/programs/x86_64-linux/ghc-8.2.2.temp/

WORKDIR /temp

ADD custom-snapshot.yaml /temp/custom-snapshot.yaml
ADD stack.yaml /temp/stack.yaml

ADD servant-persistent.cabal /temp/servant-persistent.cabal

RUN stack build --only-dependencies

RUN stack test --only-dependencies
