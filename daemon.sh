#!/bin/sh

# Startup script to keep it running on low memory VPS

while :; do
    sbcl --disable-ldb \
        --lose-on-corruption \
        --dynamic-space-size 200 \
        --eval '(progn (ql:quickload :pseudo))'
done
