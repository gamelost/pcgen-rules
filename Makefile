GHC=ghc
DATA_DIR=data/

all:
	$(GHC) -Wall -isrc/ --make src/Pcgen -o pcgen-rules

validation:
	for file in `find $(DATA_DIR) -type f | grep "\.${EXTENSION}$$"`; \
	do \
	  ./pcgen-rules $$file | grep "parsing failed"; \
	done; \
	[ $$? -ne 0 ] # invert return code

clean:
	rm *.{o,hi}
