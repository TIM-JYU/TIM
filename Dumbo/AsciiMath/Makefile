CONF_FLAGS = --enable-optimization --enable-test

cabal: deps
	cabal configure $(CONF_FLAGS)
	cabal build
	cp dist/build/asciimath/asciimath .
	cp dist/build/pandoc-asciimath/pandoc-asciimath .

filter-only: deps
	cabal configure $(CONF_FLAGS)
	cabal build pandoc-asciimath
	cp dist/build/pandoc-asciimath/pandoc-asciimath .

deps:
	cabal install --only-dependencies

stack:
	stack install --test

test: 
	cabal configure $(CONF_FLAGS)
	cabal test

clean:
	cabal clean
	stack clean
	rm -rf dist
	rm -f pandoc-asciimath
	rm -f asciimath
