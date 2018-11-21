PROJECT = cowboy2_session
REBAR3_BUILD = _build/default
PLUGIN_EBINS = $(shell echo $(REBAR3_BUILD)/plugins/*/ebin/ | awk '{ for (i = 1; i <= NF; i++) { print "-pa " $$i } }')
DEP_EBINS    = $(shell echo $(REBAR3_BUILD)/lib/*/ebin/ | awk '{ for (i = 1; i <= NF; i++) { print "-pa " $$i } }')

EBIN_DIR = $(REBAR3_BUILD)/lib/$(PROJECT)/ebin

RELEASE = $(PROJECT)_release
VER = 1

WERL = erl
PREFIX = ./
REL_BIN = $(REBAR3_BUILD)/rel/$(RELEASE)/bin/$(RELEASE)

compile:
	$(PREFIX)rebar3 compile

test:
	${PREFIX}rebar3 eunit

shell: compile
	$(WERL) -args_file rel/vm.args -config rel/sys.config $(PLUGIN_EBINS) $(DEP_EBINS) -s $(PROJECT) start

clean:
	$(PREFIX)rebar3 clean
	$(shell rm -rf _build/default/rel/*)

clean-all: clean
	$(PREFIX)rebar3 clean --all

.PHONY: compile fast shell test
