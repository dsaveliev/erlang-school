ERL_RUN_ARGS:=-name project_db -pa ebin  -boot start_sasl -s kvs_app start

compile: get-deps
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean
	rm -f erl_crash.dump

test:	compile
	rebar eunit skip_deps=true

run:
	ERL_LIBS=deps:apps erl $(ERL_RUN_ARGS)

background:
	ERL_LIBS=deps:apps erl -detached $(ERL_RUN_ARGS)

d:
	dialyzer --src -I include 

d_build:
	dialyzer --build_plt --apps erts kernel stdlib

etag:
	etag src/*

