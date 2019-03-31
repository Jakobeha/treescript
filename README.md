# TreeScript

A language to transform source code from/between different languages.

```treescript
objc'for (\i = 0; \i < \n; \i++) \expr' &Env[]: swift'for \i in 0..<\n \expr';

&Env[].
---
TypeOf[\i]: "Int";
TypeOf[\n]: "Int";
Modifies[\expr; \i]: False[];
Modifies[\expr; \n]: False[];
```

Install via the following commands:

```sh
stack install treescript

```

## Purpose

TreeScript is a DSL for writing code to analyze and transform syntax trees.

TreeScript is especially designed to:

- **Refactor:** Trivial (e.g. rename a variable) and non-trivial (convert all symbols from camel case to snake case)
- **Transpile:** Convert one language into another (e.g. CoffeeScript to JavaScript, C to Java)
- **Analyze:** - e.g. find the # of occurrences of a certain symbol.
- Allow small groups to slightly customize existing languages, creating their own "mini DSLs".

TreeScript could also:

- **Compile:** - Apply one or more passes, convert a syntax tree into a very basic syntax tree
- **Optimize:** Transform expressions into equivalent ones which run faster
- **Interpret:** "Reduce" a syntax tree as much as possible
- **Print:** Convert a syntax tree into text
- **Parse:** Convert text into a syntax tree

TreeScript is very minimal, but it can easily be extended. It relies on "libraries" to handle logic and operations like function lookup. You can download libraries, or easily create them yourself in any language.

Eventually TreeScript could even transform its own syntax trees (with a TreeScript language plugin).

## Motivation

Often, if you want to transform a language's syntax, you must learn and use a language-specific API (e.g. SourceKit for Swift, HaRe for Haskell, Scalameta for Scala). Or if the language doesn't have an API, you must create one yourself. Furthermore, these APIs are often implemented in languages which aren't designed for syntax transformation.

TreeScript is one API, explicitly designed to transform syntax, which works for many languages. To support a language, TreeScript only needs a specification, parser, and printer. You can transform the syntax of one language using a library created in a completely different language, or transform the syntax of your own language using a library created by someone else.

## How to Use

### Writing Source Code

Basically, a TreeScript program consists of **reducers** (syntax - `<input>: <output>;`). A reducer takes a specific block of code and replaces it with another block of code.

> ```treescript
> r'print(\msg)': r'cat(\msg)';
> ```
>
> The above program transforms:
>
> ```r
> print("Hello")
> print("Hola")
> print("Bonjour")
> ```
>
> into
>
> ```r
> cat("Hello")
> cat("Hola")
> cat("Bonjour")
> ```

See [doc/Semantics.md](./doc/Semantics.md) for informal semantics, and [doc/examples](./doc/examples) for examples.

### Running Source Code

Run `treescript run <EXEC>.tscr <INPUT> -o <OUTPUT>` to take the source code from `<INPUT>`, transform it using `<EXEC>.tscr`, and write the result to `<OUTPUT>`.

### Compiling Source Code

TreeScript programs can also be compiled into executables.

Run `treescript compile <SRC>.tscr` to compile `<SRC>.tscr` into `<SRC>.tprg`. Then run `./<SRC>.tprg <INPUT> -o <OUTPUT>`.

## How to Extend

TreeScript relies a lot on **plugins**. TreeScript comes with built-in plugins, and you can download or create more.

You can add/modify plugins using `treescript plugin update <plugin-path> --type <lib/lang/template-lib/template-lang>` (TODO). Plugins are stored in `<app-data>/treescript/env` (`<app-data>` is `~/Library/Application Support` on OSX and `~/%APPDATA%/` on Windows).

See [Plugins.md](doc/Plugins.md) for how to create plugins.

## Internals

See [Internals.md](doc/Internals.md) for how the TreeScript compiler / interpreter works.

## History

This is based on an earlier project, [Descript](https://bitbucket.org/jakobeha/descript-lang/src/master/).
