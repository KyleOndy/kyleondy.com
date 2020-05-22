INPUT_DIR:=provider
OUTPUT_DIR:=_site

.DELETE_ON_ERROR:


# todo: need list of posts
$(OUTPUT_DIR)/index.html:
	@mkdir -p $(dir $@)
	echo "<h1>Hello!</h1>" > $@

$(OUTPUT_DIR)/notes/%/index.html: $(INPUT_DIR)/notes/%.markdown
	@mkdir -p $(dir $@)
	cp "$<" $@


# todo: replace this with a pure bash implementation
serve:
	docker run --rm -it -p 80:80 -v $(CURDIR)/_site:/usr/share/nginx/html:ro nginx:stable

clean:
	@mkdir -p $(OUTPUT_DIR)
	# remove all content in OUTPUT_DIR. but do not remove the folder itslef.
	find $(OUTPUT_DIR) -mindepth 1 -delete
