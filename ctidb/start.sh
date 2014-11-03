#!/bin/bash

export ERL_LIBS=deps

erl -detached \
    -name ctidb_server@127.0.0.1 \
    +pc unicode \
    -pa ebin \
    -config ctidb \
    -boot start_sasl \
    -s ctidb_app start
