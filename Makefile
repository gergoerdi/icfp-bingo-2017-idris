IDRIS		= idris
IPKG		= bingo.ipkg

.phony: build clean

build:
	$(IDRIS) --build $(IPKG)

clean:
	$(IDRIS) --clean $(IPKG)
