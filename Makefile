GHC=ghc
DATA_DIR=data/

all:
	cabal build

validation:
	for file in `find $(DATA_DIR) -type f | grep "\.${EXTENSION}$$"`; \
	do \
	  cabal run pcgen-rules $$file | grep "parsing failed"; \
	done; \
	[ $$? -ne 0 ] # invert return code

clean:
	cabal clean
