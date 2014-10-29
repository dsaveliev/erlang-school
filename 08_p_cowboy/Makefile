ERL_RUN_ARGS:=+pc unicode -pa ebin -config try_cowboy -boot start_sasl -s try_cowboy_app start

compile:
	rebar compile skip_deps=true

get-deps:
	rebar get-deps

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

run:
	ERL_LIBS=deps erl $(ERL_RUN_ARGS)
