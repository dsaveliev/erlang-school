ERL_RUN_ARGS:=-sname nodeA +pc unicode -pa ebin -config dchat -boot start_sasl -s dchat_app start

compile:
	rebar compile skip_deps=true

compile-all:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

clean-all:
	rebar clean
	rm -f erl_crash.dump

test:
	rebar eunit skip_deps=true

run:
	./start.sh nodeA@localhost 8080

background:
	ERL_LIBS=deps erl -detached $(ERL_RUN_ARGS)

d:
	dialyzer --src -I include src

etags:
	etags src/*
