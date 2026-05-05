.PHONY: all ci-test clean

all:
	./rebar3 compile

ci-test:
	REBAR_CONFIG=rebar.config_ ./rebar3 as dev ct --logdir test-logs --readable true

dialyzer:
	REBAR_CONFIG=rebar.config_ ./rebar3 as dev dialyzer

clean:
	rm -rf _build rebar.lock
