#!/bin/sh
exec erl -pa ebin edit deps/*/ebin apps/*/ebin -boot start_sasl \
    -sname erms_dev2 \
    -s erms \
    -config 2app
