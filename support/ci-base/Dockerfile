FROM fpco/stack-build:lts-11.22

RUN stack --resolver ghc-8.2.2 setup \
 && rm /root/.stack/programs/x86_64-linux/ghc-8.2.2.tar.xz \
 && rm -rf /root/.stack/programs/x86_64-linux/ghc-8.2.2.temp/

WORKDIR /temp

ADD custom-snapshot.yaml /temp/custom-snapshot.yaml
ADD stack.yaml /temp/stack.yaml

ADD servant-persistent.cabal /temp/servant-persistent.cabal

RUN stack build --only-dependencies

RUN stack test --only-dependencies
