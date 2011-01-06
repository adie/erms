#!/bin/sh
exec erl -pa ebin edit deps/*/ebin apps/*/ebin -boot start_sasl \
    -sname erms_dev2 \
    -s erms_admin_app \
    -config 2app
