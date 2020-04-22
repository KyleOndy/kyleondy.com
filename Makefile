MARKDOWNFILES:=$(shell find provider/ -type f -name '*.markdown')
HTMLTARGETS:=$(MARKDOWNFILES:provider/%.markdown=build/%/index.html)

all: $(HTMLTARGETS)

build/%/index.html: provider/%.markdown
	@mkdir -p $(dir $@)
	bin/render $< > $@

clean:
	rm -Rf build
