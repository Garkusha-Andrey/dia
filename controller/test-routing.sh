#!/bin/bash
exec erl -pa ebin -eval "application:start(inets), routing:init(), routing:update(), init:stop()"
