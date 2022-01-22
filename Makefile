SITE_FOLDER=_site
GIT_REV:=$(shell git rev-parse --verify HEAD)
GH_PAGES_BRANCH=gh-pages
GH_PAGES_DIR=gh-pages

all: clean test

.PHONY: build_nix
build_nix:
	nix build .#website

.PHONY: build
build:
	nix run . -- build

.PHONY: test
test: build
	nix run . -- check

.PHONY: clean
clean:
	nix run . -- clean
	rm -f result
	@# this is hacky, just to get things working. I need to think about all the
	@# edge cases
	git worktree prune
	git worktree list --porcelain | rg $(GH_PAGES_BRANCH) | rg "^worktree " | cut -d' ' -f2 | xargs --no-run-if-empty -tI@ -- git worktree remove --force @
	rm -rf $(GH_PAGES_DIR)

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

# hacks be below
.PHONY: gh-pages
gh-pages: clean build
	git fetch
	git worktree add ./$(GH_PAGES_DIR) $(GH_PAGES_BRANCH)
	cd $(GH_PAGES_DIR) && git ls-files | xargs -tI@ -- rm -r @
	cd $(GH_PAGES_DIR) && fd --type=directory | xargs --no-run-if-empty -I@ -- rm -rf @
	cp -ar $(SITE_FOLDER)/. $(GH_PAGES_DIR)
	cd $(GH_PAGES_DIR) && git add --all && git commit -m "Built from $(GIT_REV)"
