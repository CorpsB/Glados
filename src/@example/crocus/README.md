# Crocus (Noopy)

This program prints a number in large ASCII art using a custom string as the fill pattern. It is written in the Noopy language.

## Usage

By default, the program is configured to print the number `4242` using the string `hey` as the fill pattern. You can modify these values by editing the `n_arg` and `s_arg` variables in `crocus.npy`.

## How it works

- The program defines ASCII art patterns for digits 0-9.
- It takes a number (as a string) and a fill string, then prints the number in large block letters, using characters from the fill string to draw the digits.
- The logic is implemented using recursive list processing and struct types in Noopy.

## Building

To build the executable, you need the Noopy compiler (`glados`) available in your PATH.

```
make
```

This will produce an executable named `crocus`.

## Running

To run the executable, you need the Noopy Virtual Machine (`glados-vm`) available in your PATH

```
make run
```

## Cleaning

To remove the built executable:

```
make clean
```