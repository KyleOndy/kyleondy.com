STACK=stack --install-ghc ${STACK_ARGS}
SITE_NAME=hakyll-kyleondy
SITE_EXE=$(STACK) exec $(SITE_NAME) -- ${SITE_ARGS}
PROVIDER_FOLDER=provider
SITE_FOLDER=_site
GIT_BRANCH=$(shell git rev-parse --abbrev-ref HEAD)

all: clean test

.PHONY: rebuild
rebuild:
	$(STACK) install
	$(SITE_EXE) rebuild

.PHONY: build
build:
	$(STACK) install
	$(SITE_EXE) build

.PHONY: test
test: build
	$(SITE_EXE) check

.PHONY: clean-full
clean-full: clean
	$(STACK) clean
	rm -f $(shell stack path --local-bin)/$(SITE_NAME)

.PHONY: clean
clean:
	$(SITE_EXE) clean

.PHONY: server
server: build
	$(SITE_EXE) server

.PHONY: watch
watch: build
	$(SITE_EXE) watch

.PHONY: watch-external
watch-external: build
	$(SITE_EXE) watch --host '0.0.0.0' --port '8822'

.PHONY: secrets
secrets:
	rm -rf provider/secrets
	git clone --depth=1 git@gitlab.com:kyleondy/kyleondy.com.secret provider/secrets

.PHONY: deploy
deploy: clean secrets build test
	@echo "$(shell git rev-parse --verify HEAD)" > $(SITE_FOLDER)/head.txt
	s3_website push --dry-run
