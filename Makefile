META_SRCS = $(wildcard ./src/meta/*.md)
WIKI_SRCS = $(wildcard ./src/wiki/*.md)
CSS_SRCS  = $(wildcard ./src/css/*.css)
META_DOCS = $(patsubst ./src/meta/%.md,./docs/meta/%.html,$(META_SRCS))
WIKI_DOCS = $(patsubst ./src/wiki/%.md,./docs/wiki/%.html,$(WIKI_SRCS))
CSS_DOCS  = $(patsubst ./src/css/%.css,./docs/css/%.css,$(CSS_SRCS))

all: ./docs/index.html $(META_DOCS) $(WIKI_DOCS) $(CSS_DOCS)

./bin/translator: ./translator/Main.hs
	@mkdir -p ./bin
	ghc $< -Wall -O3 -o $@

./docs/index.html: $(META_SRCS) $(WIKI_SRCS) ./bin/translator
	./bin/translator --index -i ./src -o ./docs

./docs/meta/%.html: ./src/meta/%.md ./bin/translator
	./bin/translator --meta $* -i ./src -o ./docs

./docs/wiki/%.html: ./src/wiki/%.md ./bin/translator
	./bin/translator --wiki $* -i ./src -o ./docs

./docs/css/%.css: ./src/css/%.css
	@mkdir -p ./docs/css
	cp $< $@

serve: all
	serve ./docs

install:
	cp -r ./docs/* $(out)

clean:
	rm -fr ./bin/* ./docs/*
	rm -f ./translator/*.hi
	rm -f ./translator/*.o
