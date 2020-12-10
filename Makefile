# Assumptions
#
#
#   - The home page
#   - All posts (sorted by date)
#   - All notes (sorted by name)
#   - Static pages, all under root. About, contact, pgp, etc
#   - Posts and notes, same template, differnt path
#

# Issues
#
# The following is a list of know issues I have encourned. Some of these may
# have easy fixes, I still need to sit down and read the entire Make manual.
#
# - Only changes to content trigger a rebuild. Changes to templates or the
#   files in bin/ will not trigger a rebuild. I tried adding these directories
#   directry to the recipie's prerequistes, but then these non-content files
#   need to be handled in the site generation logic.

SHELL = bash
.SHELLFLAGS = -o pipefail -c
#.DELETE_ON_ERROR:


# get all files within each subdirectory
INPUT_DIR:=provider
TMP_DIR:=_tmp
OUTPUT_DIR:=_site

# can not pass in $(INPUT_DIR) to shell commands due to make taking the
# environment that make itself was started with. Due to this, there is some
# duplication of the string `provider/`. :/

# Content
NOTES_SOURCE:= $(shell find provider/notes  -type f)
POSTS_SOURCE:= $(shell find provider/posts  -type f)
PAGES_SOURCE:= $(shell find provider/pages  -type f)
STATIC_SOURCE:=$(shell find provider/static -type f)

OUTPUT_HTML =   $(NOTES_SOURCE:$(INPUT_DIR)/%.md=_site/%/index.html) \
                $(POSTS_SOURCE:$(INPUT_DIR)/%.md=_site/%/index.html) \
                $(PAGES_SOURCE:$(INPUT_DIR)/pages/%.md=_site/%/index.html) \
                $(OUTPUT_DIR)/index.html
OUTPUT_STATIC = $(STATIC_SOURCE:$(INPUT_DIR)/static/%=_site/%)

TIDY=tidy -indent -quiet

all: build

build: $(OUTPUT_HTML) \
       $(OUTPUT_STATIC) \
       $(OUTPUT_DIR)/notes/index.html \
       $(OUTPUT_DIR)/posts/index.html \
       $(OUTPUT_DIR)/tags/index.html \
       $(OUTPUT_DIR)/css/site.css # todo: embed in pages?

$(OUTPUT_DIR)/index.html: $(INPUT_DIR)/index.html ## build root index.html
	@mkdir -p $(dir $@)
	bin/convert_to_html $< | bin/wrap_html_fragment | $(TIDY) > $@

$(OUTPUT_DIR)/posts/%/index.html: $(INPUT_DIR)/posts/%.md
	@mkdir -p $(dir $@)
	bin/convert_to_html $< | bin/wrap_html_fragment | $(TIDY) > $@

$(OUTPUT_DIR)/notes/%/index.html: $(INPUT_DIR)/notes/%.md
	@mkdir -p $(dir $@)
	bin/convert_to_html $< | bin/wrap_html_fragment | $(TIDY) > $@

$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/pages/%.md
	@mkdir -p $(dir $@)
	bin/convert_to_html $< | bin/wrap_html_fragment | $(TIDY) > $@

$(OUTPUT_DIR)/notes/index.html: $(NOTES_SOURCE)
	@mkdir -p $(dir $@)
	# todo:m this can be made better
	bin/make_notes_index | bin/wrap_html_fragment | $(TIDY) > $@

$(OUTPUT_DIR)/posts/index.html: $(NOTES_SOURCE)
	@mkdir -p $(dir $@)
	bin/make_posts_index > $@

$(OUTPUT_DIR)/tags/index.html: $(NOTES_SOURCE) $(POSTS_SOURCE)
	@mkdir -p $(dir $@)
	echo "todo" > $@

$(OUTPUT_DIR)/%: $(INPUT_DIR)/static/%
	@mkdir -p $(dir $@)
	cp $< $@

$(OUTPUT_DIR)/css/site.css: $(INPUT_DIR)/sass/site.scss
	@mkdir -p $(dir $@)
	sass $< | yuicompressor --type css > $@

# -----




#
#$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/%.md $(TEMPLATES_SOURCE) $(BIN_SOURCE)
#	@mkdir -p $(dir $@)
#	bin/wrap_html <(bin/convert_to_html $<) > $@
#
## this generates the top level pages. Why couldn't I combine this with the
## above rule?
#$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/pages/%.md $(TEMPLATES_SOURCE) $(BIN_SOURCE)
#	@mkdir -p $(dir $@)
#	bin/wrap_html <(bin/convert_to_html $<) > $@
#
#$(OUTPUT_DIR)/%: $(INPUT_DIR)/static/%
#	@mkdir -p $(dir $@)
#	cp $< $@
#
#$(OUTPUT_DIR)/notes/index.html: $(TEMPLATES_SOURCE) $(BIN_SOURCE)
#	@mkdir -p $(dir $@)
#	bin/wrap_html <(echo "todo: index of notes") > $@
#
##$(OUTPUT_DIR)/posts/index.html: $(TMP_DIR)/recent_posts.txt
##	@mkdir -p $(dir $@)
##	bin/wrap_html <(echo "todo: archive of posts") > $@
#
##$(TMP_DIR)/%.metadata.json: $(INPUT_DIR)/%.metadata.json
##	@mkdir -p $(dir $@)
##	bin/get_metadata $< > $@
#
#$(TMP_DIR)/recent_posts.txt: $(TMP_DIR) $(BIN_SOURCE)
#$(TMP_DIR)/recent_posts.txt: $(POSTS_SOURCE)
#	mkdir -p $(TMP_DIR) # todo: abstract this?
#	@echo $@ is dependent on $?
#	@echo
#	@#echo "finding everything" #> $@.$<
#	bin/parse_post_meta $? > $@
#
#$(OUTPUT_DIR)/tags/index.html: $(TEMPLATES_SOURCE) $(BIN_SOURCE)
#	@mkdir -p $(dir $@)
#	bin/wrap_html <(echo "todo: index of all tags") > $@
#
#INDEX_DEPS := $(INPUT_DIR)/index.html
#INDEX_DEPS += $(TEMPLATES_SOURCE)
#INDEX_DEPS += $(TMP_DIR)/recent_posts.txt
#$(OUTPUT_DIR)/index.html: $(INDEX_DEPS) $(BIN_SOURCE)
#	@mkdir -p $(dir $@)
#	bin/wrap_html $< >$@

# todo: replace this with a pure bash implementation
serve:
	docker run --rm -it -p 80:80 -v $(CURDIR)/_site:/usr/share/nginx/html:ro nginx:stable

clean:
	# if we just `rm -rf $(OUTPUT_DIR)` then `make server` breaks
	@mkdir -p $(OUTPUT_DIR)
	find $(OUTPUT_DIR) -mindepth 1 -delete

debug/notes: ; $(info $(NOTES_SOURCE))
debug/posts: ; $(info $(POSTS_SOURCE))
debug/pages: ; $(info $(PAGES_SOURCE))

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

