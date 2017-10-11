IDRIS		= idris
IDR_PKGS	= contrib js effects

all: bingo.js

clean:
	rm -f bingo.js

bingo.js: src/Bingo/Main.idr
	$(IDRIS) --sourcepath src -i src --codegen javascript $(foreach pkg, $(IDR_PKGS), -p $(pkg)) $< -o $@
