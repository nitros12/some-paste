FROM haskell:8

VOLUME /root/.local/bin

RUN export PATH=$(stack path --local-bin):/root/.local/bin/:$PATH

RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "libpq-dev"]

RUN mkdir -p /opt/server
WORKDIR /opt/server
COPY stack.yaml .
COPY *.cabal ./
RUN stack build --dependencies-only

COPY . /opt/server

RUN ["stack", "install"]