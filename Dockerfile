FROM ubuntu:24.04 AS builder

RUN apt update && \
  DEBIAN_FRONTEND=noninteractive apt install -y wget gnupg2 geoipupdate git make

RUN wget -qO- https://binaries2.erlang-solutions.com/GPG-KEY-pmanager.asc > /etc/apt/keyrings/erlang.asc && \
  echo 'deb [signed-by=/etc/apt/keyrings/erlang.asc] http://binaries2.erlang-solutions.com/ubuntu/ noble-esl-erlang-27 contrib' > /etc/apt/sources.list.d/erlang.list

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends --no-install-suggests esl-erlang=1:27.3.4-1 && apt-get clean


WORKDIR /openapi_handler
COPY Makefile rebar* ./
COPY src ./src
COPY test ./test
