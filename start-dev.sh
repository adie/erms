#!/bin/sh
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname erms_dev \
    -s erms \
    -s reloader \
    -config app
