# get all files within each subdirecotry
OUTPUT_DIR:=_site
NOTES_SOURCE:=$(shell find provider/notes -type f)
POSTS_SOURCE:=$(shell find provider/posts -type f)
PAGES_SOURCE:=$(shell find provider/pages -type f)

OUTPUT_HTML = $(NOTES_SOURCE:provider/%.markdown=_site/%/index.html)


all: $(OUTPUT_DIR)

debug/notes: ; $(info $(NOTES_SOURCE))
debug/posts: ; $(info $(POSTS_SOURCE))
debug/pages: ; $(info $(PAGES_SOURCE))

# this is the entrypoint
build: $(OUTPUT_HTML)

$(OUTPUT_DIR)/%/index.html:
	@mkdir -p $(dir $@)

build/%/metadata.json: provider/notes/%.markdown #$(NOTES_SOURCE)
	bin/get_metadata $< > $@

build/index.html:


clean:
	rm -rf $(OUTPUT_DIR)
