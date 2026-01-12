##
## EPITECH PROJECT, 2025
## Glados
## File description:
## Makefile
##

##
## EPITECH PROJECT, 2025
## Glados
## File description:
## Makefile
##

.PHONY: all test clean re

all:
	stack build
	cp "$$(stack path --local-install-root)/bin/glados" .
	cp "$$(stack path --local-install-root)/bin/glados-vm" .

test:
	stack test

clean:
	stack clean
	rm -r .stack-work

fclean:
	$(MAKE) clean
	rm -f glados
	rm -f glados-vm

re:
	$(MAKE) fclean
	$(MAKE) all

test:
	stack test --coverage
