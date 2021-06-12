SITE_FOLDER=_site

all: clean test

.PHONY: rebuild
rebuild:
	nix build .#website

.PHONY: build
build:
	nix run . -- build

.PHONY: test
test: build
	nix run . -- check

.PHONY: clean-full
clean-full: clean
	$(STACK) clean
	rm -f $(shell stack path --local-bin)/$(SITE_NAME)

.PHONY: clean
clean:
	nix run . -- clean

.PHONY: server
server: build
	nix run . -- server

.PHONY: watch
watch: build
	nix run . -- watch

.PHONY: develop
develop:
	make develop

.PHONY: watch-external
watch-external: build
	$(SITE_EXE) watch --host '0.0.0.0' --port '8822'

.PHONY: deploy
deploy: clean secrets build test
	@echo "$(shell git rev-parse --verify HEAD)" > $(SITE_FOLDER)/head.txt
	@echo "todo"
