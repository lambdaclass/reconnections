.PHONY: dev test

dev:
	./rebar3 as dev compile && ./rebar3 as dev shell

test:
	rebar3 ct --cover

container_test:
	./rebar3 as dev ct

release:
	./rebar3 release tar