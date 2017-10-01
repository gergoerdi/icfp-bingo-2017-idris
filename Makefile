IDRIS		= idris
IDR_PKGS	= contrib js effects lightyear

all: bingo.js

clean:
	rm -f bingo.js

bingo.js: Main.idr
	$(IDRIS) --codegen javascript $(foreach pkg, $(IDR_PKGS), -p $(pkg)) $< -o $@
