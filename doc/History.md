# History

## Relation to Descript

This language used to be called Descript. It was renamed because TreeScript better describes what it actually does - rename trees - and it sounds similar. It's the third version / "attempt" at creating a syntax transformation language. The other 2 attempts are:

1. [Descript-lang (Haskell)](https://bitbucket.org/jakobeha/descript-lang/src/master/)
2. [Descript-ocaml](https://bitbucket.org/jakobeha/descript-ocaml/src/master/)

### Major Changes from Descript-lang and Descript-ocaml

*TreeScript code is no longer interpreted, it's now compiled into another language.*

As before, there are 2 types of objects - **values** and **reducers**. A program still consists of reducers, but there's no query - all programs read source from `stdin`, transform it, and print it to `stdout`. Reducers are also organized into groups, and reducers can reference groups, which allows abstraction and sort of replaces macros. There are no macros or multiple phases, they're not needed. TreeScript no longer refactors itself directly, and if you want to write code which handles `Add`, `Subtract`, `Multiply`, and `Divide` a common way, you can do reduce these into `Arith[op: ...; left: ...; right: ...]` in the single phase. A program must also declare its records at the top of the file - builtins and source records don't need to be declared.

Primitives are exactly as before.

All record heads are static strings, and records must be declared, like in version #1. Record properties aren't named, and 2 records with the same name must have the same number of properties. "Injections" exist as "functions". Groups are similar to records, but they have named properties, and the names correspond to binds.

"Matchers" and "paths" are renamed to "binds". Now, they both consist of a single string (desugared into an integer). Input binds can contain the empty string (or integer 0), in which case they're like "any" matchers, but not output binds. Like before, binds and functions should encapsulate all abstraction.

There are no "regular" values - all values are input or output values. Thus, all values have the same specification - a value is either a primitive, record, or bind (or with sugar, a code block).

---

Analyze Example

```treescript
js'function \name(\args...) { \body... }': Int_Code[Count[\body]] &Count[];

&Count[]
===
Count[\xs] &IsList[\xs]: #Num_Add[#List_Map[Count[\1]]
```
