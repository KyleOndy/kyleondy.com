# I like process substitution way to much to deal with `sh` as the default make
# shell. I am in no way worried about portability as any environment this runs
# within I control.
SHELL=/usr/bin/env bash

# get all files within each subdirectory
INPUT_DIR:=provider
TMP_DIR:=_tmp
OUTPUT_DIR:=_site
# can not pass in $(INPUT_DIR) to shell commands due to make taking the
# environment that make itself was started with. Due to this, there is some
# duplication of the string `provider/`. :/
NOTES_SOURCE:=$(shell find provider/notes -type f)
POSTS_SOURCE:=$(shell find provider/posts -type f)
PAGES_SOURCE:=$(shell find provider/pages -type f)
TEMPLATES_SOURCE:=$(shell find provider/templates -type f)
BIN_SOURCE:=$(shell find bin/ -type f)

#                from here        replace this           with that
OUTPUT_HTML = $(NOTES_SOURCE:$(INPUT_DIR)/%.markdown=_site/%/index.html) \
              $(POSTS_SOURCE:$(INPUT_DIR)/%.markdown=_site/%/index.html) \
              $(PAGES_SOURCE:$(INPUT_DIR)/pages/%.markdown=_site/%/index.html) \
              _site/index.html

#METADATA_FILES = $(NOTES_SOURCE:$(INPUT_DIR)/%.markdown=$(TMP_DIR)/%.metadata.json) \
#                 $(POSTS_SOURCE:$(INPUT_DIR)/%.markdown=$(TMP_DIR)/%.metadata.json) \
#                 $(PAGES_SOURCE:$(INPUT_DIR)/%.markdown=$(TMP_DIR)/%.metadata.json)

#STATIC_FILES:=$(shell find provider/static -type f)
#OUTPUT_STATIC = $(STATIC_FILES:$(INPUT_DIR)/static/%=_site/%)

all: build

debug/notes: ; $(info $(NOTES_SOURCE))
debug/posts: ; $(info $(POSTS_SOURCE))
debug/pages: ; $(info $(PAGES_SOURCE))

$(TMP_DIR):
	mkdir -p $(TMP_DIR)

# this is the entry point
build: $(OUTPUT_HTML) \
       $(OUTPUT_STATIC) \
       #$(OUTPUT_DIR)/notes/index.html \
       #$(OUTPUT_DIR)/posts/index.html \
       #$(OUTPUT_DIR)/tags/index.html

$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/%.markdown $(TEMPLATES_SOURCE) $(BIN_SOURCE)
	@mkdir -p $(dir $@)
	bin/wrap_html <(bin/convert_to_html $<) > $@

# this generates the top level pages. Why couldn't I combine this with the
# above rule?
$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/pages/%.markdown $(TEMPLATES_SOURCE) $(BIN_SOURCE)
	@mkdir -p $(dir $@)
	bin/wrap_html <(bin/convert_to_html $<) > $@

$(OUTPUT_DIR)/%: $(INPUT_DIR)/static/%
	@mkdir -p $(dir $@)
	cp $< $@

$(OUTPUT_DIR)/notes/index.html: $(TEMPLATES_SOURCE) $(BIN_SOURCE)
	@mkdir -p $(dir $@)
	bin/wrap_html <(echo "todo: index of notes") > $@

#$(OUTPUT_DIR)/posts/index.html: $(TMP_DIR)/recent_posts.txt
#	@mkdir -p $(dir $@)
#	bin/wrap_html <(echo "todo: archive of posts") > $@

#$(TMP_DIR)/%.metadata.json: $(INPUT_DIR)/%.metadata.json
#	@mkdir -p $(dir $@)
#	bin/get_metadata $< > $@

$(TMP_DIR)/recent_posts.txt: $(TMP_DIR) $(BIN_SOURCE)
$(TMP_DIR)/recent_posts.txt: $(POSTS_SOURCE)
	mkdir -p $(TMP_DIR) # todo: abstract this?
	@echo $@ is dependent on $?
	@echo
	@#echo "finding everything" #> $@.$<
	bin/parse_post_meta $? > $@

$(OUTPUT_DIR)/tags/index.html: $(TEMPLATES_SOURCE) $(BIN_SOURCE)
	@mkdir -p $(dir $@)
	bin/wrap_html <(echo "todo: index of all tags") > $@

INDEX_DEPS := $(INPUT_DIR)/index.html
INDEX_DEPS += $(TEMPLATES_SOURCE)
INDEX_DEPS += $(TMP_DIR)/recent_posts.txt
$(OUTPUT_DIR)/index.html: $(INDEX_DEPS) $(BIN_SOURCE)
	@mkdir -p $(dir $@)
	bin/wrap_html $< >$@

# todo: replace this with a pure bash implementation
serve:
	docker run --rm -it -p 80:80 -v $(CURDIR)/_site:/usr/share/nginx/html:ro nginx:stable

clean:
	rm -rf $(TMP_DIR)
	# if we just `rm -rf $(OUTPUT_DIR)` then `make server` breaks
	@mkdir -p $(OUTPUT_DIR)
	find $(OUTPUT_DIR) -mindepth 1 -delete

.DELETE_ON_ERROR:
