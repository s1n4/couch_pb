.PHONY: all

include ../Makefile

.DEFAULT_GOAL := run-tests
APP_ROOT = $(CURDIR)/..
INCLUDE_DIR = $(APP_ROOT)/include
EBIN = $(APP_ROOT)/ebin
DEPS_DIR = $(APP_ROOT)/deps
ERL = erl -pa $(APP_ROOT) $(EBIN) $(DEPS_DIR)/*/ebin
PROTOBUFF_FILE = $(APP_ROOT)/src/couch.proto

run-tests: compile
	$(ERL) -noshell -noinput -eval "eunit:test({application, couch_pb}), init:stop()."
