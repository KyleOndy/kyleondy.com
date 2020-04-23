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

OUTPUT_HTML = $(NOTES_SOURCE:$(INPUT_DIR)/%.markdown=_site/%/index.html) \
              $(POSTS_SOURCE:$(INPUT_DIR)/%.markdown=_site/%/index.html) \
              $(PAGES_SOURCE:$(INPUT_DIR)/pages/%.markdown=_site/%/index.html) \
              _site/index.html

STATIC_FILES:=$(shell find provider/static -type f)
OUTPUT_STATIC = $(STATIC_FILES:provider/static/%=_site/%)

all: build

debug/notes: ; $(info $(NOTES_SOURCE))
debug/posts: ; $(info $(POSTS_SOURCE))
debug/pages: ; $(info $(PAGES_SOURCE))

# this is the entry point
build: $(OUTPUT_HTML) \
       $(OUTPUT_STATIC) \
       $(OUTPUT_DIR)/notes/index.html \
       $(OUTPUT_DIR)/posts/index.html \
       $(OUTPUT_DIR)/tags/index.html

$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/%.markdown
	@mkdir -p $(dir $@)
	bin/wrap_html <(bin/convert_to_html $<) > $@

# this generates the top level pages. Why couldn't I combine this with the
# above rule?
$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/pages/%.markdown
	@mkdir -p $(dir $@)
	bin/wrap_html <(bin/convert_to_html $<) > $@


$(OUTPUT_DIR)/%: $(INPUT_DIR)/static/%
	@mkdir -p $(dir $@)
	cp $< $@

$(OUTPUT_DIR)/notes/index.html:
	@mkdir -p $(dir $@)
	bin/wrap_html <(echo "todo: index of notes") > $@

$(OUTPUT_DIR)/posts/index.html:
	@mkdir -p $(dir $@)
	bin/wrap_html <(echo "todo: archive of posts") > $@

$(OUTPUT_DIR)/tags/index.html:
	@mkdir -p $(dir $@)
	bin/wrap_html <(echo "todo: index of all tags") > $@

#build/%/metadata.json: $(INPUT_DIR)/notes/%.markdown #$(NOTES_SOURCE)
#	bin/get_metadata $< > $@

$(OUTPUT_DIR)/index.html: $(INPUT_DIR)/index.html
	@mkdir -p $(dir $@)
	bin/wrap_html $< >$@



# todo: replace this with a pure bash implementation
serve:
	docker run --rm -it -p 80:80 -v $(CURDIR)/_site:/usr/share/nginx/html:ro nginx:stable

clean:
	@mkdir -p $(OUTPUT_DIR)
	find $(OUTPUT_DIR) -mindepth 1 -delete
	#rm -rf $(OUTPUT_DIR)
