JS_DEPS := node_modules
JS_SRCS := $(wildcard src/*.js)

BROWSERIFY := npx browserify

.DEFAULT_GOAL := index.js

# Track an empty file as a timestamp of the last `npm install`
$(JS_DEPS)/.stamp: package.json package-lock.json
	npm install
	touch $@

clean:
	rm -fr \
	  index.js \
	  node_modules \
	  pcbdata.json

index.js: $(JS_DEPS)/.stamp $(JS_SRCS) pcbdata.json
	$(BROWSERIFY) src/htmlFunctions.js --debug --outfile $@

pcbdata.json: ../Examples/Eagle/Arduino_MEGA2560_ref/pcbdata.json
	cp $< $@
