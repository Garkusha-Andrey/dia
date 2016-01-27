#!/bin/bash
exec erl -pa ebin -eval "application:start(inets), routing:init(), routing:test_update(2), routing:test_update(4), init:stop()"
