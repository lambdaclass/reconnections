.PHONY: dev test

dev:
	./rebar3 as dev compile && ./rebar3 as dev shell

test:
	blockade -d .blockade daemon > .blockade.out 2>&1 & echo $$! > .blockade.pid && \
	docker inspect --type=image redis:alpine > /dev/null || docker pull redis:alpine && \
	docker inspect --type=image postgres > /dev/null || docker pull postgres && \
	docker inspect --type=image cassandra > /dev/null || docker pull cassandra && \
	docker run --rm -it -v ${shell pwd}:/reconnections -v ~/.cache:/root/.cache -w /reconnections erlang rebar3 ct ; \
	kill -9 $$(cat .blockade.pid)

container_test:
	./rebar3 as dev ct

release:
	./rebar3 release tar
