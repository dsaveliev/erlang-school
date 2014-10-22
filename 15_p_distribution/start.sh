#!/bin/sh

NODE_NAME=$1
PORT=$2

if [ "" = "$NODE_NAME" -o "" = "$PORT" ]; then
    echo "USAGE: $0 <node_name> <port>"
    exit 1
fi

COOKIE=jCIG1Sm6nhVrQ21i0Bj0

erl \
    +pc unicode \
    -pa ebin -pa deps/*/ebin \
    -boot start_sasl \
    -config dchat \
    -name $NODE_NAME \
    -setcookie $COOKIE \
    -s dchat_app start $PORT
