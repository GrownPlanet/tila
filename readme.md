# TI-lang

A simple, compiled language for the ti83+/ti84+.

This program compiles a program in a small language to assembly, which needs to be compiled using spasm-ng.

## Running

Requirements:

- [OCaml](https://ocaml.org/)
- [spasm-ng](https://github.com/alberthdev/spasm-ng), modify run.sh to use the location where you placed spasm-ng
- the ti83+ include header (place it in `out/ti83plus.inc`)
  - [on ticalc.org](https://www.ticalc.org/pub/83plus/asm/source/include/) (download `ti83plus.zip`)
  - [on AsmPrgm in 28 days](https://taricorp.gitlab.io/83pa28d/lesson/week1/day01/index.html) (download `ti83plus.inc`)
  - [on github/konaboy32/ti-asm](https://github.com/konaboy32/ti-asm/blob/master/includes/ti83plus.inc) (download the file)

How to build: `dune build`

How to run: `dune exec tilang [program]`, `test.tp` is a small reference program you can use.

## todo

Short term:

- [x] parse the program
- [x] build an ast
- [ ] generate the assembly
- [x] output the assembly
- [x] write a script to compile the assembly
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

Long term:

- [ ] output an executable instead of assembly
- [ ] arguments for functions
