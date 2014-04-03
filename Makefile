.PHONY: all deps compile clean distclean shell test

INCLUDE_DIR = $(CURDIR)/include
EBIN = $(CURDIR)/ebin
DEPS_DIR = $(CURDIR)/deps
PROTOBUFF_TARBALL = https://github.com/basho/erlang_protobuffs/archive/0.8.1p3.tar.gz
PROTOBUFF_DEP = $(DEPS_DIR)
DEPS = $(PROTOBUFF_DEP)/erlang_protobuffs-0.8.1p3.tar.gz
ERL = erl -pa $(EBIN) $(DEPS_DIR)/*/ebin
PROTOBUFF_FILE = $(CURDIR)/src/couch.proto
TEST_DIR = $(CURDIR)/test

all: deps compile

deps: $(DEPS_DIR) $(DEPS)

$(DEPS_DIR):
	mkdir -p $(DEPS_DIR)

$(INCLUDE_DIR):
	mkdir -p $(INCLUDE_DIR)

$(PROTOBUFF_DEP)/%.tar.gz:
	wget $(PROTOBUFF_TARBALL) -O $@
	tar xzf $@ -C $(DEPS_DIR)
	-cd $(DEPS_DIR)/$* && $(MAKE)

define compile_app
	$(ERL) -eval ' \
		protobuffs_compile:scan_file("$(PROTOBUFF_FILE)", \
		[{output_include_dir, "$(INCLUDE_DIR)"}, {output_ebin_dir, "$(EBIN)"}]).' \
		-make
endef

compile:
	$(compile_app)

clean:
	rm -rf *.beam ebin/*.beam erl_crash.dump $(INCLUDE_DIR)/couch_pb.hrl

distclean: clean
	rm -rf $(DEPS_DIR)

shell:
	$(ERL)

test: clean deps
	@cd $(TEST_DIR); $(MAKE)
