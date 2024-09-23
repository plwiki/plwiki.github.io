SRCS = $(wildcard ./src/*.md)
DOCS = $(patsubst ./src/%.md,./docs/wiki/%.html,$(SRCS))

all: $(DOCS)

./bin/translator: ./translator/Main.hs
	@mkdir -p ./bin
	ghc $< -O3 -o $@

./docs/wiki/%.html: ./src/%.md ./bin/translator
	@mkdir -p ./docs/wiki
	./bin/translator -i $< -o $@

serve: all
	serve ./docs

install:
	cp -r ./docs/* $(out)

clean:
	rm -fr ./bin
	rm -f ./docs/*.html
	rm -f ./docs/wiki/*.html
	rm -f ./translator/*.hi
	rm -f ./translator/*.o
