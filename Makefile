GHC=ghc
DATA_DIR=data/

all:
	$(GHC) -Wall --make Pcgen -o pcgen-rules

validation:
	for file in `find $(DATA_DIR) -type f | grep "\.pcc$$"`; \
	do \
	  ./pcgen-rules $$file | grep "parsing failed"; \
	done; \
	[ $$? -ne 0 ] # invert return code

clean:
	rm *.{o,hi}
