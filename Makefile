all:
	rebar3 compile

ct:
	rebar3 ct --logdir _build/ct
