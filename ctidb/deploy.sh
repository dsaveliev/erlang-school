#!/bin/bash

PROJ_DIR=~/tmp/ctidb
DEST_DIR=~/tmp/ctidb_deploy

mkdir -p $DEST_DIR
rm -rf $DEST_DIR/*
mkdir -p $DEST_DIR/ebin
mkdir -p $DEST_DIR/priv

function copy {
    mkdir -p $DEST_DIR/deps/$1/ebin
    cp $PROJ_DIR/deps/$1/ebin/* $DEST_DIR/deps/$1/ebin
}
copy cowboy
copy cowlib
copy ranch
copy goldrush
copy lager

cp $PROJ_DIR/ebin/* $DEST_DIR/ebin/
cp $PROJ_DIR/ctidb.config $DEST_DIR/
cp $PROJ_DIR/start.sh $DEST_DIR/

cd $DEST_DIR/..
tar czf ctidb.tar.gz ctidb_deploy
