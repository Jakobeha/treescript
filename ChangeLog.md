# Changelog for descript-R

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
  - Imports - descript in alphabetical order, then others in alphabetical order
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

## Unreleased changes
