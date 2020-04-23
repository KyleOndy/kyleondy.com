# I like process substitution way to much to deal with `sh` as the default make
# shell. I am in no way worried about portability as any environment this runs
# within I control.
SHELL=/usr/bin/env bash

# get all files within each subdirectory
INPUT_DIR:=provider
OUTPUT_DIR:=_site
# can not pass in $(INPUT_DIR) to shell commands due to make taking the
# environment that make itself was started with. Due to this, there is some
# duplication of the string `provider/`. :/
NOTES_SOURCE:=$(shell find provider/notes -type f)
POSTS_SOURCE:=$(shell find provider/posts -type f)
PAGES_SOURCE:=$(shell find provider/pages -type f)

OUTPUT_HTML = $(NOTES_SOURCE:provider/%.markdown=_site/%/index.html) \
              $(POSTS_SOURCE:provider/%.markdown=_site/%/index.html) \
              $(PAGES_SOURCE:provider/%.markdown=_site/%/index.html)


all: build

debug/notes: ; $(info $(NOTES_SOURCE))
debug/posts: ; $(info $(POSTS_SOURCE))
debug/pages: ; $(info $(PAGES_SOURCE))

# this is the entry point
build: $(OUTPUT_HTML)

$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/%.markdown
	@mkdir -p $(dir $@)
	bin/wrap_html <(bin/convert_to_html $<) > $@
	#tidy -quiet -modify -indent --output-html --indent=auto $@

#build/%/metadata.json: provider/notes/%.markdown #$(NOTES_SOURCE)
#	bin/get_metadata $< > $@

build/index.html:

# todo: replace this with a pure bash implementation
serve: build
	docker run --rm -it -p 8080:80 -v $(CURDIR)/_site:/usr/share/nginx/html:ro nginx:stable

clean:
	rm -rf $(OUTPUT_DIR)
