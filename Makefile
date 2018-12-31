
compile:
	./rebar3 compile

test:
	./rebar3 eunit

shell: compile clean-db
	./rebar3 shell --setcookie testcookie --name test@127.0.0.1

clean:
	./rebar3 clean
	$(shell rm -rf _build/default/rel/*)

clean-db:
	rm -rf data/db

clean-all: clean clean-db
	./rebar3 clean --all

.PHONY: compile fast shell test
