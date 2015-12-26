#!/bin/bash
exec erl -pa ebin -eval "application:start(inets), application:start(routing_app), init:stop()"
