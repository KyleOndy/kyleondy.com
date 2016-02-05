SITE_EXE=stack exec site-kyleondy
PROVIDER_FOLDER=provider
SECRET_FOLDER=provider.secret
BUILD_FOLDER=_build

.PHONY: build
build: prep
	stack install
	# need to automate building pdfs and such
	touch $(BUILD_FOLDER)/files/Kyle.Ondy-Resume.pdf
	touch $(BUILD_FOLDER)/files/Kyle.Ondy-Resume.pdf.sig
	# automate above
	$(SITE_EXE) -- build

.PHONY: check
check: prep build
	$(SITE_EXE) -- check

.PHONY: clean
clean:
	$(SITE_EXE) -- clean
	rm -rf $(BUILD_FOLDER)

.PHONY: server
server: prep build
	$(SITE_EXE) -- server

.PHONY: watch
watch: prep build
	$(SITE_EXE) -- watch

.PHONY: prep
prep:
	# need a way to get a one way sync from $(PROVIDER_FOLDER) to $(BUILD_FOLDER) so stack can rebuild
	mkdir -p $(BUILD_FOLDER)
	cp -a $(PROVIDER_FOLDER)/* $(BUILD_FOLDER)
	if [ -d "$(SECRET_FOLDER)" ]; then cp -a $(SECRET_FOLDER)/* $(BUILD_FOLDER); fi
