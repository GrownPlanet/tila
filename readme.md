# TI-lang

A simple, compiled language for the ti83+/ti84+.

This program compiles a program in a small language to assembly, which needs to be compiled using spasm-ng.

## Running

Requirements:

- [OCaml](https://ocaml.org/)
- [spasm-ng](https://github.com/alberthdev/spasm-ng), modify run.sh to use the location where you placed spasm-ng
- the ti83+ include header (place it in `out/ti83plus.inc`), this is also included by default in the `inc` folder of spasm-ng
  - [on ticalc.org](https://www.ticalc.org/pub/83plus/asm/source/include/) (download `ti83plus.zip`)
  - [on AsmPrgm in 28 days](https://taricorp.gitlab.io/83pa28d/lesson/week1/day01/index.html) (download `ti83plus.inc`)
  - [on github/konaboy32/ti-asm](https://github.com/konaboy32/ti-asm/blob/master/includes/ti83plus.inc) (download the file)

To build: `dune build`

To run: `dune exec tila [program]`, `test.tp` is a small reference program you can use.

## todo

Short term:

- [x] parse the program
- [x] build an ast
- [x] generate the assembly
- [x] output the assembly
- [x] write a script to compile the assembly
- [x] test it!
- [ ] add more features
  - [ ] mutating variables
  - [ ] if/else statements
  - [ ] for/while loops
  - [ ] math (+, -, >, <, ==, ...)
  - [ ] stack allocated sprites
  - [ ] variables
  - [ ] more default functions: getkey, ...
  - [ ] *advanced* math (*, /, ...; this is more difficult to implement on the ti84)

Long term:

- [ ] output an executable instead of assembly
- [ ] arguments for functions
- [ ] asyncio multithreading
