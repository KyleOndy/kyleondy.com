SITE_EXE=stack exec site-kyleondy
PROVIDER_FOLDER=provider
BUILD_FOLDER=_build

.PHONY: build
build:
	stack install
	# need to automate building pdfs and such
	touch $(BUILD_FOLDER)/files/Kyle.Ondy-Resume.pdf
	touch $(BUILD_FOLDER)/files/Kyle.Ondy-Resume.pdf.sig
	# automate above
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
	rm -rf $(BUILD_FOLDER)

.PHONY: server
server: build
	$(SITE_EXE) -- server

.PHONY: package
pachage: check
	tar -zcvf kyleondy.com.tar.gz _site/

.PHONY: watch
watch: build
	$(SITE_EXE) -- watch
