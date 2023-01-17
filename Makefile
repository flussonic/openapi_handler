.PHONY: all ci-test ci-test-legacy clean

all:
	./rebar3 compile

ci-test:
	./rebar3 as dev ct --logdir test-logs --readable true
ci-test-legacy:
	./rebar3 as dev_legacy ct --logdir test-logs --readable true

clean:
	rm -rf _build rebar.lock
