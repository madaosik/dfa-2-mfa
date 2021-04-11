all:
	ghc -o dka-2-mka src/*.hs
clean:
	rm src/*.hi src/*.o dka-2-mka

run: all
	./dka-2-mka -t test/test0.in

pack:
	cp app/Main.hs src/Main.hs
	zip -r flp-fun-xlanic04.zip README Makefile doc src/*.hs test

make test: all
	test/test_runner

	