DATA_DIR=data/
PCGEN=./dist/build/pcgen-rules/pcgen-rules # calling `cabal run` repeatedly is slow
LST_TYPE:=$(shell echo $(TYPE) | tr '[:lower:]' '[:upper:]')

all:
	cabal build pcgen-rules

.PHONY: all

# ex: make test-tags TYPE=language
test-tags:
	@echo '** testing tags that have lst type' $(LST_TYPE)
	for file in `grep -R --include="*.pcc" "$(LST_TYPE):" $(DATA_DIR) | grep -v '#' | cut -d ':' -f 3 | cut -d '|' -f 1 | tr -d '\r' | xargs basename -a | sort | uniq`; \
	do \
		echo "processing $$file"; \
		$(PCGEN) $$file $(LST_TYPE); \
	done

clean:
	cabal clean
