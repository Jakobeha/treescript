# Changelog for treescript

## 12/19/18

Started brainstoriming

## 12/21/18

Initial specification

Setup Haskell project

- General architecture/idea from previous project
  - Will work very hard to make less bloated though, no unnecessary features
- Although there are a few style differences:
  - More generics / autoderiving
  - Language extensions - in alphabetical order
  - Imports - `TreeScript` in alphabetical order, then others in alphabetical order
  - Qualified imports always have shortest non-conflicting qualifiers, no special differences for "main" imported class (e.g `Text` in `Data.Text`)
  - Try to put small functions above big functions in modules - still order is datatypes, classes, instances, and functions
  - Modules can be bigger, but they'll still be split when too big (overall module structure still shouldn't matter much) - generally 100 lines is OK, 200 or 300 in some cases OK too

## 12/23/18

Lexing and `Lex` AST

- Passes a few tests but likely still some bugs

General test suite (taken from previous project)

## 12/25/18

Parsing and `Sugar` AST

- Can now analyse this AST and generate code with it even though it might not be well-formed
- Passes a couple tests but likely some bugs

Some docs

- Code design
- Changelog

## 12/29/18

Significantly changed specification, slightly changed lexing and parsing

- Mainly made specification much more precise, and figured out how to implement
- Updated README, lexing, parsing, and tests
- `Sugar` and `Core` ASTs don't have guard support yet, but `Lex` AST has guard lexemes

## 1/01/19

Desugaring and `Core` AST

- Can parse spliced code blocks with plugins
- Still can't parse guards

Basic plugin support

- Plugin settings
- Plugin languages
  - Currently the only languages are `scheme` and `text`
  - `scheme` has a specification and parsing (although dots followed by lists don't work)
  - `text` has a specification
- Plugin printing skeleton

## 1/02/19

C template, translation and `Translate` phase

- Template for C code
- Updated README, added how Descript code will be translated into C, spliced into the template, and this will be compiled to get the Descript executable
- Tested via printing/observing, didn't actually compile code yet

## 1/03/19

Compiling and CLI

- Compiling both C (last step) and Descript (all steps)
- Tests for compiling - currently only work for AST data
- Command-line interface only supports compiling
- Started to change name to "TreeScript"

## 1/04/19

Renamed to `treescript`, added vscode extension to project

Run on source code - `treescript run`

- Parses code into AST data, runs the executable, and prints it back into code
- Now can take a Descript source file and input, and generate output - `treescript compile` the source file, `treescript run` the executable with the input.
- I/O exceptions should now be wrapped in `Result`s, better error messages and handling
- Bugfixes for compiling (fixed memory issue)

## 1/07/19

Started to add functions and "groups"

- Syntax for functions and groups (`Lex` to `Core` phases) - untested, but regular syntax still works
- Some documentation
- TODO Add more documentation, add ability to translate to C (`Translate` phase)

## 1/12/19

Libraries

- Renamed "servers" to libraries
- Changed library specification and implementation:
  - Directly inserts library into compiled C program and calls function, instead of communicating with an external command-line program
  - Functions are qualified by library
  - (From previous changes) functions have fixed numbers of arguments, libraries have names in spec
- Added `Base` library with some simple functions - arithmatic and string appending
  - May change these functions later, e.g. make them fail more gracefully
- Added test for library which also uses Y-combinator
- Bugfixes, refactors, documentation improvements, etc.

Working on VSCode extension / language server on a separate branch

## 2/15/19

Wrote interpreter in Rust, instead of directly compiling into C

- Currently slower than C version
- But should be faster to debug and improve

## Unreleased changes
