MARKDOWNFILES:=$(shell find provider/ -type f -name '*.markdown')
HTMLFILES:=$(shell find provider/ -type f -name '*.html')
TMPHTML = $(MARKDOWNFILES:provider/%.md=build/%/index.html) $(HTMLFILES:provider/%.html=build/%/index.html)
OUTPUTHTML = $(TMPHTML:build/index/index.html=build/index.html)
RENDERDEPS=bin/render provider/static/style.css


# get all files within each subdirecotry
NOTES_SOURCE:=$(shell find provider/notes -type f)
POSTS_SOURCE:=$(shell find provider/posts -type f)
PAGES_SOURCE:=$(shell find provider/pages -type f)

debug/notes: ; $(info $(NOTES_SOURCE))
debug/posts: ; $(info $(POSTS_SOURCE))
debug/pages: ; $(info $(PAGES_SOURCE))

all: build

# todo: refine this / put into static dir that just gets coppied over with no
build/robots.txt:
	echo -e "User-agent: *\nAllow: /" > $@

# main entrypoint into website
build/index.html: provider/index.html $(RENDERDEPS)
	@mkdir -p $(dir $@)
	cat provider/templates/site_pre.html > $@
	bin/render $< >> $@
	cat provider/templates/site_post.html >> $@

build/%/index.html: provider/%.md $(RENDERDEPS)
	@mkdir -p $(dir $@)
	bin/render $< > $@

build/%/index.html: provider/%.html
	@mkdir -p $(dir $@)
	cp $< $@

build/static: $(shell find provider/static/ -type f )
	mkdir -p build/
	cp -r provider/static build/

build/favicon.ico: static/favicon.png
	convert $< $@

build/sitemap.xml: $(OUTPUTHTML)
	bin/sitemap $(OUTPUTHTML) > $@

build/atom.xml: $(OUTPUTHTML)
	bin/atom $(OUTPUTHTML) > $@

build: build/static $(OUTPUTHTML) build/robots.txt #build/favicon.ico build/sitemap.xml build/atom.xml

serve:
	bin/serveit -s build "make -j8"

clean:
	rm -Rf build

help:
	@echo "make        # Builds the website into build/"
	@echo "make serve  # Starts a development server at http://localhost:8000/"
	@echo "make clean  # Deletes the build directory"

.DELETE_ON_ERROR:
.PHONY: all build clean serve help
