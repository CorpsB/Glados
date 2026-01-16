##
## EPITECH PROJECT, 2026
## Glados
## File description:
## Makefile
##

SRC = crocus.npy
OUT = crocus

all: $(OUT)

$(OUT): $(SRC)
	glados $(SRC) $(OUT)

run: $(OUT)
	glados-vm $(OUT)

clean:
	rm -f $(OUT)

.PHONY: all run clean
