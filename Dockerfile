FROM ubuntu:20.04 AS builder

RUN apt update && \
  DEBIAN_FRONTEND=noninteractive apt install -y wget gnupg2 geoipupdate git make

RUN wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb && \
  dpkg -i erlang-solutions_2.0_all.deb && \
  rm -f erlang-solutions_2.0_all.deb

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends --no-install-suggests esl-erlang=1:24.2.1-1 && apt-get clean


WORKDIR /openapi_handler
COPY Makefile rebar* ./
COPY src ./src
COPY test ./test
