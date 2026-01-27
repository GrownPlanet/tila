# TI-lang

A simple, compiled language for the ti83+/ti84+.

This program compiles a program in a small language to assembly, which needs to be compiled using spasm-ng.

## Running

requirements:

- [OCaml](https://ocaml.org/)

how to build: `dune build`

how to run: `dune exec tilang [program]`, `test.tp` is a small reference program you can use.

## todo

short term
- [x] parse the program
- [x] build an ast
- [ ] generate the assembly
- [ ] output the assembly
- [ ] write a script to compile the assembly
- [ ] test it!
- [ ] add more features
  - [ ] mutating variables
  - [ ] if/else statements
  - [ ] for/while loops
  - [ ] math (+, -, >, <, ==, ...)
  - [ ] sprites
  - [ ] variables
  - [ ] more default functions: getkey, clearhome, ...
  - [ ] *advanced* math (*, /, ...; this is more difficult to implement on the ti84)

long term
- [ ] output byte code instead of assembly
- [ ] arguments for functions
