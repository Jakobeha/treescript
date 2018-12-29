# Descript-R

Third version / "attempt" of Descript. The other 2 are:

1. Descript-lang (Haskell)
2. Descript-ocaml

## Purpose

Descript is a DSL for writing code to transform syntax trees.

Descript is especially designed to:

- Convert one language into another (e.g. CoffeeScript to JavaScript, C to Java)
- Perform refactors - trivial (e.g. rename a variable) and non-trivial (move all global variables with a literal prefix into a namespace, and remove the prefix)
- Apply syntax sugar
- Apply one or more passes (part of compilation, related to syntax sugar)
- Optimize source code by transforming expressions into equivalent ones which run faster
- Allow small groups to slightly customize existing languages, creating their own "mini DSLs".

Descript could also:

- Analyze code - e.g. find the # of occurrences of a certain symbol.
- Fully interpret a language, by "reducing" its syntax tree as much as possible
- Fully compile a language - convert it's syntax tree into a very basic syntax tree
- Print a language - convert it's syntax tree into text
- Parse a language - convert text into a syntax tree

Descript programs can do more when given a language server with helpful utility functions. Eventually, though, these functions would be the almost entire program, so the "Descript" wouldn't be doing much, and there would be a lot of communication back and forth between the Descript program and server, which could be inefficient (although apparently socket communication can be very fast).

Eventually Descript could even transform its own syntax trees (although it needs an AST specification and language parser first)

## Overview

The Descript compiler takes an input source file and generates a program. This program takes a file containing an **AST Specification** as a command-line argument. It then reads a stream of **syntax trees** conforming to the specification. It applies all reducers to each tree it encounters, and outputs a stream of transformed trees.

---

The following **examples** are for a minimal lambda-calculus Scheme specification:

```s
(((lambda (f) ((f 4) 5))
  (lambda (x) (lambda (y) (lambda (z) ((+ x) ((- z) y))))))
 3)
```

(evaluates to 2).

### AST Specification

The AST specification defines every type of node in the AST for *all languages*. Each node has a name, target language, and number of children.

In the future, the AST specification can also contain a set of servers. Each server has a URL, port, and a list of functions. Each function has a name, optional target language, and number of arguments. The program will connect to these servers, and whenever it evaluates a function associated with an IP address, it sends the function index and input (as ASTs) to the address, and interprets the response as the function's output. The AST specification is in JSON.

---

**Example:**

```json
{
  "nodes": [
    {
      "name": "Var",
      "args": 1, //identifier
      "language": "scheme"
    },
    {
      "name": "Lambda",
      "args": 2, //bound identifier and body expression
      "language": "scheme"
    },
    {
      "name": "App",
      "args": 2, //function and body expressions
      "language": "scheme"
    },
    {
      "name": "Integer",
      "args": 1, //literal fixed-precision integer
      "language": "scheme"
    }
  ],
  "servers": [
    {
      "url": "localhost",
      "port": "8009",
      "language": "scheme",
      "functions": [
        {
          "name": "CallBuiltin", //fallback for undefined function calls
          "args": "2" //applied function identifier and body expression
        }
      ]
    }
  ]
}
```

### Syntax Tree Data

In the Descript program, every AST node is assigned a name, number of children, and identifier (the node's target language is prefixed to its name). The identifiers are signed 32-bit integers, and nodes get identifiers as follows:

- Arbitrary data (not a node) has identifier 0.
- Nodes in the AST specification have *positive* identifiers. The nodes get the identifiers in the order they were defined.
- Builtin nodes and nodes defined in the source have *negative* identifiers. `Unit[]` has -1, `False[]` has -2, `True[]` has -3, `Nil[]` has -4, `Cons[]` has -5, etc. Builtin nodes have "higher" (lower magnitude) identifiers than user defined ones.

The syntax trees are represented in postfix notation by a continuous stream of integers. When an integer is read, you can determine whether the following data belongs to a block of data, a child node, a sibling / outer node. If the integer is 0, the next piece of data is a *64-bit unsigned* integer which determines the size of the data, and that amount of bytes are the content of the data. Otherwise, the next piece of data is a 32-bit signed integer for another node.

---

**Example:**

```
3 3 2 0 <"f"> 3 3 1 0 <"f"> 4 0 <4> 4 0 <5> 2 0 <"x"> 2 0 <"y"> 2 0 <"z"> 3 3 1 0 <"+"> 1 0 <"x"> 3 3 1 0 <"-"> 1 0 <"z"> 1 0 <"y"> 4 0 <3>
```

### Descript Source Code

Descript source files are compiled in the following steps with the following forms:

- Read (input text)
- Lexed (`Lex` phase AST)
- Parsed (`Sugar` phase AST)
- Desugared (`Core` phase AST)
- Compiled (output text)

In the sugar and core phases:

- (As before), a Descript source file is a list of record declarations followed by reducers.
- Each record declaration consists of a (string) head, and a sequence of (string) identifiers which determine # of arguments (currently the identifiers are just for documentation).
- Each reducer consists of an input value, output value, and a sequence of guards.
- Each guard consists of an (output value) predicate and (input value) match.
- In the sugar phase, each value is either a primitive, record, code block, or bind.
- In the core phase, each value is either a primitive, record, or abstraction.
- Each primitive is either a 32-bit integer, float, or string.
- Each record consists of a (string) head, a sequence of (value) properties, and a boolean to determine whether its a function.
- Each code block consists of an alternating sequence of strings and (spliced) output values.
- In the sugar phase, each bind is a string. Input binds can be empty strings, output binds can't.
- In the core phase, each bind is an unsigned integer. Input binds can be 0, output binds can't.

Additionally, the sugar phase allows some syntax errors, so it can be parsed context-free:

- A declaration can come after reducers, but this is invalid.
- Record declarations can contain value instead of string properties, and vice versa (e.g. `Foo[bar, Baz[]]`)

## Major Changes from v1 and v2

*Descript code is no longer interpreted, it's now compiled into another language.*

As before, there are 2 types of objects - **values** and **reducers**. A program still consists of reducers, but there's no query - all programs read source from `stdin`, transform it, and print it to `stdout`. There are no macros or multiple phases, they're not needed. Descript no longer refactors itself directly, and if you want to write code which handles `Add`, `Subtract`, `Multiply`, and `Divide` a common way, you can do reduce these into `Arith[op: ...; left: ...; right: ...]` in the single phase. A program must also declare its records at the top of the file - builtins and source records don't need to be declared.

Primitives are exactly as before.

All record heads are static strings, and records must be declared, like in version #1. Record properties aren't named, and 2 records with the same name must have the same number of properties. "Injections" exist as "functions"

"Matchers" and "paths" are renamed to "binds". Now, they both consist of a single string (desugared into an integer). Input binds can contain the empty string (or integer 0), in which case they're like "any" matchers, but not output binds. Like before, binds and functions should encapsulate all abstraction.

There are no "regular" values - all values are input or output values. Thus, all values have the same specification - a value is either a primitive, record, or bind (or with sugar, a code block).

## Compilation

### Steps

- Read (e.g. from stdin as input text)
- Lex (into `Lex` phase AST)
- Parse (into `Sugar` phase AST)
- Desugar (into `Core` phase AST)
  - Make identifiers indices
  - Desugar code blocks (e.g. `c'while (\x < \y) \x++;' => C_While[C_LessThan[\x; \y]; C_Inc[\x]]`)
    - Move splices out (`'while (\0 < \1) \2++;)'[\x / \0; \y / \1, \x / \2]`, not the same as indexing identifiers)
    - Run a language parser for the given language (generates syntax tree data)
    - Convert back into records, using the language parser's associated AST specification, and substituting the free (after-declared) nodes with their corresponding splices
      - Expects a single syntax tree, creates a single record. To encode multiple statements, wrap them in a block (every AST specification should define one, even if it's not valid in the actual language, it's convenient for Descript to pass data around).
  - (In the future) apply syntax shortcuts like lists
- Compile (into literal C program)
  - TODO Summarize?
- Write (e.g. to stdout as output text)

### Language Parsers

A language parser is a command-line program which reads a language's source text from stdin and outputs the corresponding syntax tree data. It isn't a Descript programs itself (it's a compiled program which could've been written in any language).

Each language parser has an associated "AST speification" - this specification only defines nodes for the parser's language (remember, each language parser only handles one language), and it shouldn't define any servers or functions (since the language parser can do any advanced operations itself). The Descript compiler gets its language parsers from a library (probably in it's app data).

Each language parser must (or at least strongly should) handle indexed splices (`\#`, e.g. in `while (\0 < \1) \2++;)`), and convert `\\` occurrences to actual backslashes. When an indexed splice is encountered, it gets converted into a node with a positive identifier *immediately* after the last positive identifier in the AST specification (# of nodes in the specification + 1) + the index. e.g. for `while (\0 < \1) \2++;)`, if the defined language has 50 declared nodes, in the syntax tree data `\0` is encoded with `51`, `1` is encoded with `52`, and `2` is encoded with
`53`. Normally these are unbound identifiers, but a program reading the syntax tree data (such as the Descript compiler) will know they refer to an indexed splice (and since they know how many identifiers are in the AST specification, which splice it refers to).

Language parsers are specifically designed to be used by the Descript compiler, to desugar code blocks. As such, they have a strict specification - there are other ways to get source text into syntax tree data, e.g. not using this specific command-line format, and you can use these other methods when getting the syntax tree data to feed into a compiled Descript program. However, they're convenient when also paired with a Descript program - the command `cat <input> | <language-parser> | <descript-program> <language-parser-specification> | <language-printer> | <output>` will read source text from `<input>`, apply `<descript-program>`, and print the write the transformed source text to `<output>`.

## Descript (Compiled) Program Structure

TODO fix

- Descript text is parsed into an AST, which contains records, target language code blocks, and backend language code blocks.
- The AST is partially desugared - splices become explicit (e.g. `Assign[lhs: <x?'is_label($)'; rhs: <?'$ == x']` becomes `Assign[lhs: <?'{ .fill = (x) => is_label(x) }:Descript::Splice<bool>'; rhs: <?#Splice[outer: '{ .fill = (x) => { .fill = (y) => y == x } }:Descript::Splice<Descript::Splice<bool>>'; inner: >lhs]]`), but target language code blocks remain.
- Target language code blocks are desugared. They're parsed into instances of `<target>::Value`, *using the C++ language parser library at compile-time*. This library is linked with a universal program which expects some `value.serialize...()` methods and `<target>::serializationHeader` and uses them to export the values into data, which the Descript compiler imports and replaces the code blocks with.
- The compiler starts with the source code to define `class Descript::Value`:
  - The compiler defines `class Descipt::Primitive` and `class Descript::Record`
  - For all record declarations, the compiler defines `class Descript::<name>(Descript::Value* <prop1>, Descript::Value* <prop2>, ...)`
  - The compiler defines an instance of `Descript::<name>` for every `<target>::Value` record.
- The compiler defines conversion functions to convert instances of `<target>::Value` to `Descript::Value`s.
- The reducers are used to create a `public virtual Descript::Value* Descript::Value::reduce()` method (in functional programming, `reduce : Descript.Value -> Maybe Descript.Value`)
  - Each reducer `N` (whose input surface is a record `<type>`) adds a new case in `<type>`'s override - `if consumeN(inp) { return produceN(inp) }` - and new methods `private bool Descript::<type>::consumeN()` and `private Descript::Value* Descript::<type>::produceN()` (in functional programming, adding a new case `reduce (<type> ...) | consumeN ... = Just (produceN ...)`). At the end of each record's override, `<type>` returns `super.reduce()`. The base class implementation, also covering primitives, returns `null` (`reduce _ = Nothing`).
  - In the record's `consumeN`, each property is checked against its corresponding input property - `return prop1.consumeNProp1() && prop2.consumeNProp2() && ...`. `public virtual Descript::Value* Descript::Value::consumeNPropM`s is defined similar to `reduce`.
    - If the input surface is a primitive `<prim>`, it changes `Primitive<T>`'s override to `return typeof(T) == typeof(<prim's T>) && (<prim's T>)self.value == <prim>.value` (for all primitive types, *`==` must be overloaded to test equivalence, not identity*). The base class implementation, also covering records, returns `false`.
    - If the input surface is a record `<type>`, it changes `<type>`'s override to `return prop1->consumeNPropMProp1() && prop2->consumeNPropMProp2() && ... (&& super.consumeNPropM())`. The base class implementation, also covering primitives, returns `false`.
    - If the input surface is a matcher - containing `Descript::Primitive<Descript::Splice<bool>> <prim>` - it changes the base method to `return <prim>.value.fill(self)`.
  - In the record's `produceN`:
    - If the output surface is a primitive `<prim>`, it changes `produceN` to `return <prim>`.
    - If the output surface is a record `<type>`, it changes `produceN` to `return new <type>(((Prop1Type*)prop1)->produceNProp1(), ((Prop2Type*)prop2)->produceNProp2(), ...)`. `Prop1Type`, `Prop2Type`, etc. are statically resolved, using the input to resolve the type of paths - paths to matchers just get the type `Descript::Value`. `private Descript::Value* Descript::PropMType::produceNPropM`s is defined the same way as `produceN` (the property type is known enough so it doesn't have to be public or virtual).
    - If the output surface is a path - with elements `elem1`, `elem2`, `...` - it changes the base method to `return ((Elem2Type*)((Elem1Type*)self.elem1)->elem2)->...`.
  - `#Splice` records have their own `reduce` override which applies the splice.
- The main program runs as follows:
  - Reads source code from the target language as input (specifically, from stdin).
  - Parses the source code into a `<target>::Value` using the C++ parser linked *at runtime*.
  - Converts this into a `Descript::Value`.
  - Calls the value's `reduce()` method until it returns `null` (define a helper in `Value`, `reduceFull()`), which:
    - Applies the reducers from the Descript source to transform the value from the input shape into the output shape.
    - Applies target-language common reducers (can be explicitly imported by the Descript source, or implicitly defined in an external Descript source) to transform the value from the output shape into a primitive string, containing the output
    - Prints this output (specifically, to stdout)

## Descript Source Structure

A typical Descript program will be written in **Descript** and **"backend-lang" (C++)**, then compiled into a **"backend-lang" (C++)** program, which takes a **"target-lang" (R)** program as input, transforms it, and outputs it.

A typical Descript program will now:

- Use the backend to read input (or if lazy, have input built-in to the query).
- *Use the backend to parse input into a Descript AST* - in theory, Descript can parse input directly, but in practice it takes a ridiculus amount of time.
- Use Descript to transform this AST, by matches against pieces of it and reducing those pieces
- Use Descript (more likely) or the backend (less likely) to print this AST back into source code.

## Examples / Prototypes

### Simple Evaluation

This simple Descript program will "interpret" lambda calculus programs written in scheme, by applying the lambdas.

```descript
Subst[body; old; new].

//Lambda application
scheme'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg]
Subst[Scheme_Var[\old]; \old; \new]: \new
Subst[scheme'(lambda (\old) \body)'; \old; \]: scheme'(lambda (\old) \body)'
Subst[scheme'(lambda (\x) \body)'; \old; \new]: scheme'(lambda (\x) \(Subst[\body; \old; \new]))'
Subst[scheme'(\f \x)'; \old; \new]: scheme'(\(Subst[\f; \old; \new]) \(Subst[\x; \old; \new]))'
Subst[\body; \; \]: \body
```

It gets desugared into something like (technically invalid syntax, good for the example):

```descript
Subst[body; old; new].

//Lambda application
Scheme_App[Scheme_Lambda[\0; \1]; \2]: Subst[\1; \0; \2]
Subst[Scheme_Var[\0]; \0; \1]: Scheme_Var[\1]
Subst[Scheme_Lambda[\0; \1]; \0; \]: Scheme_Lambda[\0; \1]
Subst[Scheme_Lambda[\0; \1]; \2; \3]: Scheme_Lambda[\0; Subst[\1; \2; \3]]
Subst[Scheme_App[\0; \1]; \2; \3]: Scheme_App[Subst[\0; \1; \3]; Subst[\2; \2; \3]]
Subst[\0; \1; \2]: \0
```

And compiled into somthing like (pseudocode, not actual output):

```python
TODO
```

### Functions

This example is broken - it uses a builtin function to allow integer addition.

```descript
Subst[body; old; new].

//Lambda application
scheme'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg]
Subst[Scheme_Var[\old]; \old; \new]: \new
Subst[scheme'(lambda (\old) \body)'; \old; \]: scheme'(lambda (\old) \body)'
Subst[scheme'(lambda (\x) \body)'; \old; \new]: scheme'(lambda (\x) \(Subst[\body; \old; \new]))'
Subst[scheme'(\f \x)'; \old; \new]: scheme'(\(Subst[\f; \old; \new]) \(Subst[\x; \old; \new]))'
Subst[\body; \; \]: \body

//Addition
scheme'((+ \(Scheme_Integer[\x])) \(Scheme_Integer[\y]))': Scheme_Integer[#Add[\x; \y]]
```

However, it misses some cases, e.g. `(((lambda (f) ((f x) x)) +) 2)` should reduce to `4` but doesn't. This example handles addition properly:

```descript
Add[lhs].
Subst[body; old; new].

//Lambda application
scheme'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg]
Subst[Scheme_Var[\old]; \old; \new]: \new
Subst[scheme'(lambda (\old) \body)'; \old; \]: scheme'(lambda (\old) \body)'
Subst[scheme'(lambda (\x) \body)'; \old; \new]: scheme'(lambda (\x) \(Subst[\body; \old; \new]))'
Subst[scheme'(\f \x)'; \old; \new]: scheme'(\(Subst[\f; \old; \new]) \(Subst[\x; \old; \new]))'
Subst[\body; \; \]: \body

//Addition
scheme'(+ \(Scheme_Integer[\x])': Add[\x]
scheme'(\(Add[\x]) \(Scheme_Integer[\y]))': Scheme_Integer[#Add[\x; \y]]
```

### Optimization

This program optimize R for-loops on data frames, by converting the data frame into a list so it's not copied every time.

```descript
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

There are edge-cases, e.g. when the input isn't a data frame or `y` is used afterward. These can be fixed with guards.

```descript
r'
for (\i in \iter) {
  \x[[\i]] <- \expr
}
' | \i => R_Var[\; \];
  | \x => R_Var[\; "data.frame"];
  | #Includes[\iter; \x] => False[];
  | R_Var[#FreeId[]; "data.list"] => \y
  : r'
\y <- as.list(\x)

for (\i in \iter) {
  \y[[\i]] <- \expr
}

\x <- as.data.frame(\y)
';
```

(example borrowed from [Advanced R](https://adv-r.hadley.nz/names-values.html))
