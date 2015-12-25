#!/bin/bash
exec erl -pa ebin -eval "application:start(inets), application:start(controller), init:stop()"
