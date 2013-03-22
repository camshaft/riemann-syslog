
REBAR = ./rebar
APP = riemann_syslog

RELEASE_TAG=release
ORIGINAL_DIR=$(APP)_orig
GIT_HASH=$(shell git log --pretty=format:'%h' -n 1)
PREV_HASH=$(shell git rev-list $(RELEASE_TAG) --pretty=format:'%h' | sed -n 2p)
# PREV_RELEASE=$(shell )

default: compile

all: deps compile dev/sync

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

generate: compile
	$(REBAR) generate
	chmod u+x rel/$(APP)/bin/$(APP)

dev:
	mkdir dev

dev/sync: dev
	git clone https://github.com/rustyio/sync.git dev/sync
	cd dev/sync && make && cd -

devstart: dev/sync
	@dev/start

distclean: clean 
	$(REBAR) delete-deps

console:
	rel/$(APP)/bin/$(APP) console -pa ../../ebin

rebuild: clean compile generate

start:
	./rel/$(ORIGINAL_DIR)/bin/$(APP) start

release: clean generate git_hash_rename
	ln -s `pwd`/rel/$(APP)_$(GIT_HASH) `pwd`/rel/$(ORIGINAL_DIR)
	@git tag -d $(RELEASE_TAG)
	@git tag $(RELEASE_TAG)

upgrade: clean generate git_hash_rename
	$(REBAR) generate-appups previous_release="$(APP)_$(PREV_HASH)"
	$(REBAR) generate-upgrade previous_release="$(APP)_$(PREV_HASH)"
	./rel/$(ORIGINAL_DIR)/bin/$(APP) upgrade `pwd`/rel/$(APP)_$(GIT_HASH)
	@git tag -d $(RELEASE_TAG)
	@git tag $(RELEASE_TAG)

git_hash_rename:
	mv rel/$(APP) rel/$(APP)_$(GIT_HASH)

test:
	$(REBAR) skip_deps=true eunit

docs: deps
	$(REBAR) skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c apps/$(APP)/ebin


.PHONY: all deps test
