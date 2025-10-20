SITE_FOLDER=_site

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

.PHONY: deploy
deploy: clean build
	rsync -avz --delete _site/ svc.deploy@tiger:/var/www/kyleondy.com/
