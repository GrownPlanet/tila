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

To run: `dune exec tila [program] [output.s]`, `test.tp` is a small reference program you can use. You need to use spasm-ng to compile the generated assembly to a 8xp file (example: `spasm out.s out.8xp`).

## Usage

The language is still in relatively early stages, you can see the things I want to implement in `reference.tl`. The currently existing features are outlined below. Keep in mind that this language has no semicolons.

You can use comments using `//`.

You can create a global variable using `global [type] [name] = [value]`. The types are `string`, `u8`, and `u16`. For example: `global u16 x = 140`.

You can create a function using `fn [name]() { ... code ... }`. Arguments aren't supported yet, so you will need to use global variables for now. The `main` function is the starting point for the program.

You can call a function using `[name]()`.

You can use conditionals using `if [expression] { ... code ... }` with an optional `else { ... code ... }`. For now you can check for equality.

There are a few built in functions:

1. `print([string variable])`
2. `go_home()` put the cursor for text printing on 0, 0
3. `clear_lcd()` clear the lcd

## Todo

Short term:

- [x] parse the program
- [x] build an ast
- [x] generate the assembly
- [x] output the assembly
- [x] write a script to compile the assembly
- [x] test it!
- [ ] add more features
  - [ ] mutating variables
  - [x] if/else statements
    - [x] ==
    - [x] fix variables: they are 8bit instead of 16bit
    - [ ] >, <, >=, <=
  - [ ] for/while loops
  - [ ] math (+, -, >, <, ==, ...)
  - [ ] stack allocated sprites
  - [ ] variables
  - [ ] more default functions: getkey, ...
  - [ ] *advanced* math (*, /, ...; this is more difficult to implement on the ti84)

Long term:

- [ ] output an executable instead of assembly
- [ ] better error messages
- [ ] arguments for functions
- [ ] asyncio multithreading
