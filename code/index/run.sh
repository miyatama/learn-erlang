#!/bin/bash

rm -fr ./bigindex
mkdir ./bigindex
mkdir ./erl.supported
# copy ./code/*.erl to ./erl.supported/
cp ../*.erl ./erl.supported/
erl -boot start_clean -s indexer cold_start
