META_SRCS = $(wildcard ./src/meta/*.md)
WIKI_SRCS = $(wildcard ./src/wiki/*.md)
META_DOCS = $(patsubst ./src/meta/%.md,./docs/meta/%.html,$(META_SRCS))
WIKI_DOCS = $(patsubst ./src/wiki/%.md,./docs/wiki/%.html,$(WIKI_SRCS))

all: ./docs/index.html $(META_DOCS) $(WIKI_DOCS)

./bin/translator: ./translator/Main.hs
	@mkdir -p ./bin
	ghc $< -O3 -o $@

./docs/index.html: $(META_SRCS) $(WIKI_SRCS) ./bin/translator
	./bin/translator --index -i ./src -o ./docs

./docs/meta/%.html: ./src/meta/%.md ./bin/translator
	./bin/translator --meta $* -i ./src -o ./docs

./docs/wiki/%.html: ./src/wiki/%.md ./bin/translator
	./bin/translator --wiki $* -i ./src -o ./docs

serve: all
	serve ./docs

install:
	cp -r ./docs/* $(out)

clean:
	rm -fr ./bin/* ./docs/*
	rm -f ./translator/*.hi
	rm -f ./translator/*.o
