VERIFY="./verify.sh"
LST_TYPE:=$(shell echo $(TYPE) | tr '[:lower:]' '[:upper:]')

all:
	cabal build

.PHONY: all

# ex: make test-tags TYPE=language
test-tags:
	@echo 'testing tags that have lst type' $(LST_TYPE)
	$(VERIFY) ${LST_TYPE}

clean:
	cabal clean
