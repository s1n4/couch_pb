.PHONY: run-tests

include ../Makefile

.DEFAULT_GOAL := run-tests
APP_ROOT = $(CURDIR)/..
INCLUDE_DIR = $(APP_ROOT)/include
EBIN = $(APP_ROOT)/ebin
DEPS_DIR = $(APP_ROOT)/deps
ERL = erl -pa $(APP_ROOT) $(EBIN) $(DEPS_DIR)/*/ebin
PROTOBUFF_FILE = $(APP_ROOT)/src/couch.proto

define run_eunit
	@$(ERL) -noshell -noinput -eval '\
		case eunit:test({application, couch_pb}) of \
			error -> init:stop(1); \
			ok -> init:stop(0) \
		end.'
endef

run-tests:
	@$(compile_app) > /dev/null
	$(run_eunit)
