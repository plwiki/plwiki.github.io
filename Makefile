.PHONY: all install serve clean

all: ./build
	./build all

install: all
	cp -r ./docs/* $(out)

serve: all
	serve ./docs

clean:
	rm -fr ./bin ./docs ./build ./*.hi ./*.o translator/*.hi translator/*.o

./build: ./build.hs
	@mkdir -p ./bin
	ghc ./build.hs -Wall -O3 -o ./build
