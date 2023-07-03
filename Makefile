PROJECT := cowboy2_session

REL_BIN := _build/default/rel/$(PROJECT)/bin/$(PROJECT)

compile:
	./rebar3 compile

test:
	./rebar3 eunit

shell: compile clean-db
	./rebar3 shell --setcookie testcookie --name test@127.0.0.1

rel: ## Creates an otp release (dev)
	./rebar3 release

start: clean rel ## Start otp app (dev)
	$(REL_BIN) start

console: rel ## Start otp app console (dev)
	$(REL_BIN) console

clean:
	./rebar3 clean
	$(shell rm -rf _build/default/rel/*)

clean-db:
	rm -rf data/db

clean-all: clean clean-db
	./rebar3 clean --all

.PHONY: compile rel fast shell test
