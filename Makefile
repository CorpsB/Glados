.PHONY: all test clean re

all:
	stack build
	cp "$$(stack path --local-install-root)/bin/glados" .

test:
	stack test

clean:
	stack clean
	rm -r .stack-work

fclean:
	$(MAKE) clean
	rm -f glados

re:
	$(MAKE) fclean
	$(MAKE) all