SITE_EXE=stack exec site-kyleondy
PROVIDER_FOLDER=provider

.PHONY: build
build:
	stack install
	$(SITE_EXE) -- build

.PHONY: check
check: build
	$(SITE_EXE) -- check -i

.PHONY: check-full
check-full: build
	$(SITE_EXE) -- check

.PHONY: clean
clean:
	$(SITE_EXE) -- clean

.PHONY: server
server: build
	$(SITE_EXE) -- server

.PHONY: package
pachage: check
	tar -zcvf kyleondy.com.tar.gz _site/

.PHONY: watch
watch: build
	$(SITE_EXE) -- watch
