ERL_RUN_ARGS:=-name project_db -pa ebin  -boot start_sasl -s kvs_app start

compile: 
	rebar compile

initialize:
	mkdir priv
	dialyzer --build_plt --apps erts kernel stdlib inets

get-deps:
	rebar get-deps

etag:
	etag src/*

clean:
	rebar clean
	rm -f erl_crash.dump

d:
	dialyzer --src -I include src

test:	compile
	rebar eunit skip_deps=true

run:
	ERL_LIBS=deps:apps erl $(ERL_RUN_ARGS)

background:
	ERL_LIBS=deps:apps erl -detached $(ERL_RUN_ARGS)


