INPUT_DIR:=provider
OUTPUT_DIR:=_site

.DELETE_ON_ERROR:

NOTES:=$(patsubst $(INPUT_DIR)/notes/%.markdown,$(OUTPUT_DIR)/notes/%/index.html,$(wildcard $(INPUT_DIR)/notes/*.markdown))
PAGES=$(patsubst $(INPUT_DIR)/pages/%.markdown,$(OUTPUT_DIR)/%/index.html,$(wildcard $(INPUT_DIR)/pages/*.markdown))

.PHONY: build
build: $(NOTES) \
       $(PAGES) \
       $(OUTPUT_DIR)/index.html

# todo: need list of posts
$(OUTPUT_DIR)/index.html: $(INPUT_DIR)/index.html
	@mkdir -p $(dir $@)
	bin/wrap_html <(bin/convert_to_html $<) > $@

$(OUTPUT_DIR)/notes/%/index.html: $(INPUT_DIR)/notes/%.markdown
	@mkdir -p $(dir $@)
	cp "$<" $@

$(OUTPUT_DIR)/%/index.html: $(INPUT_DIR)/pages/%.markdown
	@mkdir -p $(dir $@)
	cp "$<" $@

# todo: replace this with a pure bash implementation
serve:
	docker run --rm -it -p 80:80 -v $(CURDIR)/_site:/usr/share/nginx/html:ro nginx:stable

clean:
	@mkdir -p $(OUTPUT_DIR)
	# remove all content in OUTPUT_DIR. but do not remove the folder itslef.
	find $(OUTPUT_DIR) -mindepth 1 -delete
