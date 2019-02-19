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

TODO Outdated - need to update some things.

<!--ts-->
<!--te-->

## Purpose

TreeScript is a DSL for writing code to transform syntax trees.

TreeScript is especially designed to:

- Convert one language into another (e.g. CoffeeScript to JavaScript, C to Java)
- Perform refactors - trivial (e.g. rename a variable) and non-trivial (move all global variables with a literal prefix into a namespace, and remove the prefix)
- Apply syntax sugar
- Apply one or more passes (part of compilation, related to syntax sugar)
- Optimize source code by transforming expressions into equivalent ones which run faster
- Allow small groups to slightly customize existing languages, creating their own "mini DSLs".

TreeScript could also:

- Analyze code - e.g. find the # of occurrences of a certain symbol.
- Fully interpret a language, by "reducing" its syntax tree as much as possible
- Fully compile a language - convert it's syntax tree into a very basic syntax tree
- Print a language - convert it's syntax tree into text
- Parse a language - convert text into a syntax tree

TreeScript programs can do more when given a library with helpful utility functions. Eventually, though, these functions would be the almost entire program, so the "TreeScript" wouldn't be doing much, and there would be a lot of communication back and forth between the TreeScript program and library, which could be inefficient.

Eventually TreeScript could even transform its own syntax trees (with a TreeScript language plugin).

## How to Use

### Writing Source Code

A TreeScript program consists of **reducers** (syntax - `<input>: <output>;`). A reducer takes a specific block of code and replaces it with another block of code.

> ```treescript
> r'print("Hello")': r'cat("World")';
> ```
>
> The above program transforms:
>
> ```r
> print("Hello")
> print("Hi")
> print("Hello")
> ```
>
> into
>
> ```r
> cat("World")
> print("Hi")
> cat("World")
> ```

A reducer can be abstracted with **binds** (syntax - `\<term>`), so it can transform code of a more general form.

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

Internally, code snippets are represented by **values** - every value is either a **primitive** (number or string, e.g. `"foo"`) or **records** (syntax - `<name>[<property>; ...]`).

> ```treescript
> r'print(\msg)': r'cat(\msg)';
> ```
>
> This program is actually desugared into:
>
> ```treescript
> R_Call[R_Symbol["print"]; Cons[\msg; Nil[]]]: R_Call[R_Symbol["cat"]; Cons[\msg; Nil[]]];
> ```

You can declare your own helper records, which act as intermediate forms while transforming code.

> ```treescript
> Print[msg].
>
> r'print(\msg)': Print[\msg];
> r'write("stdin", \msg)': Print[\msg];
> Print["Hello"]: r'cat("World")';
> Print[\msg]: r'cat(\msg)';
> ```
>
> The above program transforms:
>
> ```r
> print("Hello")
> print("Hola")
> write("stdin", "Bonjour")
> write("stdin", "Hello")
> ```
>
> into
>
> ```r
> cat("World")
> cat("Hola")
> cat("Bonjour")
> cat("World")
> ```

One thing to note is that *reducers won't automatically transform nested statements (e.g. statements in functions)*. You need to define **evaluation reducers** (syntax - `E[<input>]: <output>`) to describe which nested statements to transform.

> ```treescript
> r'print(\msg)': r'cat(\msg)';
> ```
>
> The above program transforms:
>
> ```r
> print("Hello")
>
> foo <- function() {
>   print("World")
> }
> foo()
> ```
>
> into
>
> ```r
> cat("Hello")
>
> foo <- function() {
>   print("World")
> }
> foo()
> ```
>
> Notice that the second `print` *isn't* transformed. To fix this, define an evaluation reducer:
>
> ```treescript
> r'print(\msg)': r'cat(\msg)';
> E[r'\x <- \y']: r'\x <- \(E[\y])';
> E[r'function() { \stmt }']: r'function() { \(E[\stmt]) }';
> ```
>
> Now the program correctly transforms the source code into
>
> ```r
> cat("Hello")
>
> foo <- function() {
>   cat("World")
> }
> foo()
> ```

TODO Describe groups

### Running Source Code

Run `treescript run <EXEC>.tscr <INPUT> -o <OUTPUT>` to take the source code from `<INPUT>`, transform it using `<EXEC>.tscr`, and write the result to `<OUTPUT>`.

### Compiling Source Code

TreeScript programs can also be compiled into executables.

Run `treescript compile <SRC>.tscr` to compile `<SRC>.tscr` into `<SRC>.tprg`. Then run `./<SRC>.tprg <INPUT> -o <OUTPUT>`.

## Plugins

The TreeScript language is extensible. The compiler uses external programs and specifications to handle different languages, and TreeScript programs use external programs and specifications to implement extra computations in functions. These external programs and specifications are located in the TreeScript's compilers appdata, probably e.g. `~/Library/Application Support/TreeScript/`.

The TreeScript compiler uses a builtin library of **language plugins**, it takes an input source file and generates a command-line program. This program takes an optional command-line argument path (used for context), and it reads a stream of **AST data**. It applies all reducers to each tree it encounters, and outputs a stream of transformed AST data.

---

The following **examples** are for a minimal lambda-calculus Scheme specification:

```s
(((lambda (f) ((f 4) 5))
  (lambda (x) (lambda (y) (lambda (z) ((+ x) ((- z) y))))))
 3)
```

(evaluates to 2).

### Language Plugin

A language plugin describes how to parse, print, and analyze a single language. It consists of:

- Language Specification
- Language Parser
- Language Printer

### Language Specification

The language specification defines the language's file extension, and every type of node in the AST. Each node has a name and number of children. The name must consist of only uppercase letters, lowercase letters, and numbers, and it should be CamelCase.

---

**Example:**

```json
{
  "extension": "scm",
  "nodes": [
    {
      "name": "Var",
      "args": 1, //identifier
    },
    {
      "name": "Lambda",
      "args": 2, //bound identifier and body expression
    },
    {
      "name": "App",
      "args": 2, //function and body expressions
    },
    {
      "name": "Integer",
      "args": 1, //literal fixed-precision integer
    }
  ]
}
```

### Language Parser

A language parser is a command-line program which reads a language's source text from stdin and outputs the corresponding AST data. It isn't a TreeScript programs itself (it's a compiled program which could've been written in any language).

Each language parser must (or at least strongly should) handle indexed splices (`\#`, e.g. in `while (\0 < \1) \2++;)`), and convert `\\` occurrences to actual backslashes. When an indexed splice is encountered, in the AST data it gets converted into a `splice` node, which is followed by the splice's index. e.g. for `while (\0 < \1) \2++;)`, `\0` is encoded with `splice 0`, `1` is encoded with `splice 1`, and `2` is encoded with
`splice 2` (these nodes have no children).

Language parsers are specifically designed to be used by the TreeScript compiler, to desugar code blocks. As such, they have a strict specification - there are other ways to get source text into AST data, e.g. not using this specific command-line format, and you can use these other methods when getting the AST data to feed into a compiled TreeScript program. However, they're convenient when also paired with a TreeScript program - the command `cat <input> | <language-parser> | <treescript-program> --stdin --stdout | <language-printer> | <output>` will read source text from `<input>`, apply `<treescript-program>`, and print the write the transformed source text to `<output>`.

### Language Printer

A language printer is the opposite of a language parser - it's a command-line program which reads AST data for a language and outputs it's source text.

### Library

A library provides additonal functions for TreeScript programs. Each function may or may not be for a particular langugage - e.g. it could be a function which takes a method identifier and provides its definition, or an additional math function. Each function is prefixed with the library's name - e.g. if the library `Base` wanted to provide the function `Add`, it would be referenced in code by `#Base_Add`.

A library consists of extra C code and a library specification.

### Library Code

A library is implemented in C code. Every library has one file, `interface.h` - for each of its functions, this header defines a corresponding C function, `value call_<library name>_<function name>(value* args);` (the type `value` is declared in `../types.h`, which the interface should `#include`). When a TreeScript program encounters a function, it calls the defined C function, inputting each argument as a `value`.

### Library Specification

The library specification declares all the functions the library provides. (Like each AST node in a language specification) each function has a name and number of arguments. The name must consist of only uppercase letters, lowercase letters, and numbers, and it should be CamelCase.

---

**Example:**

```json
{
  "name": "Base",
  "functions": [
    {
      "name": "Add",
      "args": 2
    },
    {
      "name": "Subtract",
      "args": 2
    },
    {
      "name": "Multiply",
      "args": 2
    },
    {
      "name": "Divide",
      "args": 2
    },
  ]
}
```

### AST Data

AST data is a text-based format which encodes TreeScript values and abstract syntax trees for any language. It consists of a sequence of "words" separated by spaces and newlines.

AST data encodes a sequence of values, and each value is separated by a newline. A primitive integer, float, or string is represented by the literal "integer", "float", or "string", followed by the (32-bit signed) integer, float, or escaped quoted string which is the content. A record is represented by the literal record's head, followed by the # of properties, followed by each property - e.g. `Foo[Bar[], 5]` is represented by `Foo 2 Bar 0 integer 5`. Thus AST data is prefix notation.

Each AST node corresponds to a record. The head is the created by combining the node's target language, an underscore, and its name - e.g. `Java_While`.

---

**Example:**

```s
App 2 App 2 Lambda 2 string "f" App 2 App 2 Var 1 string "f" Number 1 integer 4 Number 1 integer 5 Lambda 2 string "x" Lambda 2 string "y" Lambda 2 string "z" App 2 App 2 Var 1 string "+" Var 1 string "x" App 2 App 2 Var 1 string "-" Var 1 string "z" Var 1 string "y" Number 1 integer 3
```

## Compilation

A TreeScript source file is compiled into an executable.

More specifically, it gets compiled into a C project, which gets compiled into an executable.

More specifically, it goes through the following steps with the following forms:

- Read source from path (supplied by command-line argument)
- Lex (into `Lex` phase AST)
- Parse (into `Sugar` phase AST)
- Desugar (into `Core` phase AST)
- Generate C code (`Translate` phase)
- Compile C code
- Copy program to path (supplied by command-line argument)

### Source Structure

In the sugar and core phases:

- A TreeScript source file is a list of record declarations followed by group definitions.
- Each record declaration consists of a (string) head, and a sequence of (string) identifiers which determine # of arguments (currently the identifiers are just for documentation).
- Each group definition consists of a group declaration, inherited groups, and reducers.
- Each group declaration consists of a (string) head, and a sequence of (string) identifiers (these are actually significant).
- Each reducer consists of an input clause and output clause.
- Each clause consists of a value and groups.
  - If an input clause references a group, its reducers are contravariant, so that the input clauses actually conform to the output clause specification and vice versa (then).
- Each group consists of a (string) head, and a sequence of group properties.
- Each group property consists of a (string) identifier and value.
- In the sugar phase, each value is either a primitive, record, code block, or bind.
- In the core phase, each value is either a primitive, record, or abstraction.
- Each primitive is either a 32-bit integer, float, or string.
- Each record consists of a (string) head, a sequence of (value) properties, and a boolean to determine whether its a function.
- Each code block consists of an alternating sequence of strings and (spliced) output values.
- In the sugar phase, each bind is a string. In covariant groups, input binds can be empty strings, output binds can't. In contravariant groups, output binds can be empty strings, input binds can't.
- In the core phase, each bind is an unsigned integer. In covariant groups, input binds can be 0, output binds can't. In contravariant groups, output binds can be 0, input binds can't.

Additionally, the sugar phase allows some syntax errors, so it can be parsed context-free:

- A declaration can come after group definitions and reducers, but this is invalid.
- Record declarations, groups, and records can all contain string, value and group properties (e.g. `Foo[bar, Baz[], qux: 5]`)

The translate phase contains blocks of generated C code, which get spliced into a template to create the complete C project.

### Steps

- Read source from path (supplied by command-line argument)
- Lex (into `Lex` phase AST)
- Parse (into `Sugar` phase AST)
- Desugar (into `Core` phase AST)
  - Make identifiers indices
  - Desugar code blocks (e.g. `c'while (\x < \y) \x++;' => C_While[C_LessThan[\x; \y]; C_Inc[\x]]`)
    - Move splices out (`'while (\0 < \1) \2++;)'[\x / \0; \y / \1, \x / \2]`, not the same as indexing identifiers)
    - Run the given language's (plugin) parser (generates AST data)
    - Convert back into records, using the (plugin) language specification, and substituting the free (after-declared) nodes with their corresponding splices
      - Expects a single syntax tree, creates a single record. To encode multiple statements, wrap them in a block (every language specification should define one, even if it's not valid in the actual language, it's convenient for TreeScript to pass data around).
  - (In the future) apply syntax shortcuts like lists
- Generate C code (`Translate` phase)
  - Specifically, generate certain functions which are different in different TreeScript programs:
  - `get_record_num_props` (affected by user-defined record declarations)
  - `reduce_surface` (affected by reducers)
- Compile C code
  - Copy the template - a C project with code shared by all TreeScript programs - into a temporary directory
  - Splice the generated C code into the template copy, yielding a complete C project
  - Run `gcc` on the temporary project, yielding the TreeScript program
- Copy program to path (supplied by command-line argument)

## C Project Structure

In the plugins folder, there's a C project template. This contains C code which is common for all TreeScript programs, and splice markers. The TreeScript compiler will take a TreeScript source file and generate code to fill these splices. To finish compiling the TreeScript source, it'll copy the template and fill in the splices, then compile the resulting C project.

## Examples / Prototypes

### Simple Evaluation

This simple TreeScript program will "interpret" lambda calculus programs written in scheme, by applying the lambdas.

```treescript
Subst[body; old; new].

//Lambda application
scm'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg] &Subst[];
E[scm'(\x \y)']: scm'(\(E[\x]) \(E[\y]))';

&Subst[].
---
Subst[\old; \old; \new]: \new;
Subst[scm'(lambda (\old) \body)'; \old; \]: scm'(lambda (\old) \body)';
Subst[scm'(lambda (\x) \body)'; \old; \new]: scm'(lambda (\x) \(Subst[\body; \old; \new]))';
Subst[scm'(\f \x)'; \old; \new]: scm'(\(Subst[\f; \old; \new]) \(Subst[\x; \old; \new]))';
Subst[\body; \; \]: \body;
E[scm'(\x \y)']: scm'(\(E[\x]) \(E[\y]))';
E[scm'(lambda (\x) \body)']: scm'(lambda (\x) \(E[\body]))';
```

It gets desugared into something like (technically invalid syntax, good for the example):

```treescript
Subst[body; old; new].

//Lambda application
Scheme_Cons[
  Scheme_Cons[Scheme_Symbol["lambda"]; Scheme_Cons[Scheme_Cons[Scheme_Symbol[\1]; Scheme_Nil[]]; \2]];
  Scheme_Cons[\3; Scheme_Nil[]]
]: Subst[\2; \1; \3] &Subst[];
E[Scheme_Cons[\1; Scheme_Cons[\2; Scheme_Nil[]]]]: Scheme_Cons[E[\1]; Scheme_Cons[E[\2]; Scheme_Nil[]]];

&Subst[].
---
Subst[\1; \1; \2]: \2;
Subst[
  Scheme_Cons[Scheme_Symbol["lambda"]; Scheme_Cons[Scheme_Cons[Scheme_Symbol[\1]; Scheme_Nil[]]; \2]];
  \1;
  \
]: Scheme_Cons[Scheme_Symbol["lambda"]; Scheme_Cons[Scheme_Cons[Scheme_Symbol[\1]; Scheme_Nil[]]; \2]];
Subst[
  Scheme_Cons[Scheme_Symbol["lambda"]; Scheme_Cons[Scheme_Cons[Scheme_Symbol[\1]; Scheme_Nil[]]; \2]];
  \3;
  \4
]: Scheme_Cons[Scheme_Symbol["lambda"]; Scheme_Cons[Scheme_Cons[Scheme_Symbol[\1]; Scheme_Nil[]]; Subst[\2; \3; \4]]];
Subst[
  Scheme_Cons[\1; Scheme_Cons[\2; Scheme_Nil[]]];
  \3;
  \4
]: Scheme_Cons[Subst[\1; \3; \4]; Scheme_Cons[Subst[\2; \3; \4]; Scheme_Nil[]]];
Subst[\1; \; \]: \1;
E[
  Scheme_Cons[Scheme_Symbol["lambda"]; Scheme_Cons[Scheme_Cons[Scheme_Symbol[\1]; Scheme_Nil[]]; \2]]
]: Scheme_Cons[Scheme_Symbol["lambda"]; Scheme_Cons[Scheme_Cons[Scheme_Symbol[\1]; Scheme_Nil[]]; E[\2]]];
E[Scheme_Cons[Scheme_Symbol["lambda"]; \1; Scheme_Cons[\2; Scheme_Nil[]]]]: Scheme_Cons[E[\1]; Scheme_Cons[E[\2]; Scheme_Nil[]]];
```

And compiled into somthing like (not necessarily actual output, this was taken from output at one point):

```c
if (in.type == RECORD && strings_equal(in.as_record.head, "Scheme_Cons")) {
  value* in_props = in.as_record.props;
  value in_0 = in_props[0];
  if (in_0.type == RECORD && strings_equal(in_0.as_record.head, "Scheme_Cons")) {
    value* in_0_props = in_0.as_record.props;
    value in_0_0 = in_0_props[0];
    ...
    //printf("<Produce!> ");
    value* out_props = malloc0(sizeof(value) * 3);
    value out_0 = dup_value(matches[1]);
    out_props[0] = out_0;
    ...
    value out = {
      .type = RECORD,
      .as_record = (value_record){
        .head = "Subst",
        .num_props = 3,
        .props = out_props
      }
    };
    free_value(in);
    *x = out;
    return true;
  }
}
...
```

### Functions

This example uses a builtin function to allow integer addition. Note, however, that it's broken.

```treescript
Subst[body; old; new].

//Lambda application
scm'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg] &Subst[];
//Addition
scm'((+ \(Scheme_Atom[\x])) \(Scheme_Atom[\y]))': Scheme_Atom[#Base_Add[\x; \y]];
E[scm'(\x \y)']: scm'(\(E[\x]) \(E[\y]))';

&Subst[].
---
...
```

Specifically, the above example misses some cases, e.g. `(((lambda (f) ((f x) x)) +) 2)` should reduce to `4` but doesn't. This example handles addition properly:

```treescript
Add[lhs].
Subst[body; old; new].

//Lambda application
scm'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg] &Subst[];
//Addition
scm'(+ \(Scheme_Atom[\x]))': Add[\x];
scm'(\(Add[\x]) \(Scheme_Atom[\y]))': Scheme_Atom[#Base_Add[\x; \y]];
E[scm'(\x \y)']: scm'(\(E[\x]) \(E[\y]))';

&Subst[].
---
...
```

### Optimization

This program optimizes R for-loops on data frames, by converting the data frame into a list so it's not copied every time.

```treescript
r'
for (\i in \iter) {
  \x[[\i]] <- \expr
}
': r'
y <- as.list(\x)

for (\i in \iter) {
  y[[\i]] <- \expr
}

\x <- as.data.frame(y)
';
```

e.g. it converts

```r
for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]]
}
```

into

```r
y <- as.list(x)

for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}

x <- as.data.frame(y)
```

There are edge-cases, e.g. when the input isn't a data frame or `y` is used afterward. These can be fixed with groups.

```treescript
r'
for (\i in \iter) {
  \x[[\i]] <- \expr
}
' &Group[]: r'
\y <- as.list(\x)

for (\i in \iter) {
  \y[[\i]] <- \expr
}

\x <- as.data.frame(\y)
';

&Group[i; x; iter; y];
---
\i: R_Var[\; \];
\x: R_Var[\; "data.frame"];
#Includes[\iter; \x]: False[];
R_Var[#FreeId[]; "data.list"]: \y;
```

(example borrowed from [Advanced R](https://adv-r.hadley.nz/names-values.html))

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
