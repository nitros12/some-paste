FROM haskell:8
WORKDIR /code
RUN stack update
RUN apt-get update && apt-get install --yes --force-yes zip make tar build-essential xz-utils libpq-dev postgresql
COPY ./some-paste.cabal /code/some-paste.cabal
COPY ./stack.yaml /code/stack.yaml
RUN stack install --only-dependencies --install-ghc
COPY . /code
RUN stack install
