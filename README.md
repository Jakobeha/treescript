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

## Components

The Descript language is extensible. The compiler uses external programs and specifications to handle different languages, and Descript programs use external programs and specifications to implement extra computations in functions. These external programs and specifications are located in the Descript's compilers appdata, probably e.g. `~/Library/Application Support/Descript/`.

The Descript compiler uses a builtin library of **language specifications**, it takes an input source file and generates a command-line program. This program takes an optional command-line argument path (used for context), and it reads a stream of **AST data**. It applies all reducers to each tree it encounters, and outputs a stream of transformed AST data.

---

The following **examples** are for a minimal lambda-calculus Scheme specification:

```s
(((lambda (f) ((f 4) 5))
  (lambda (x) (lambda (y) (lambda (z) ((+ x) ((- z) y))))))
 3)
```

(evaluates to 2).

### Language Specification

A language specification describes how to parse, print, and analyze a single language. It consists of:

- AST Specification
- Language Parser
- Language Printer

### AST Specification

The AST specification defines every type of node in the AST. Each node has a name and number of children. The name must consist of only uppercase letters, lowercase letters, and numbers, and it should be CamelCase.

---

**Example:**

```json
{
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

A language parser is a command-line program which reads a language's source text from stdin and outputs the corresponding AST data. It isn't a Descript programs itself (it's a compiled program which could've been written in any language).

Each language parser must (or at least strongly should) handle indexed splices (`\#`, e.g. in `while (\0 < \1) \2++;)`), and convert `\\` occurrences to actual backslashes. When an indexed splice is encountered, in the AST data it gets converted into a `splice` node, which is followed by the splice's index. e.g. for `while (\0 < \1) \2++;)`, `\0` is encoded with `splice 0`, `1` is encoded with `splice 1`, and `2` is encoded with
`splice 2` (these nodes have no children).

Language parsers are specifically designed to be used by the Descript compiler, to desugar code blocks. As such, they have a strict specification - there are other ways to get source text into AST data, e.g. not using this specific command-line format, and you can use these other methods when getting the AST data to feed into a compiled Descript program. However, they're convenient when also paired with a Descript program - the command `cat <input> | <language-parser> | <descript-program> | <language-printer> | <output>` will read source text from `<input>`, apply `<descript-program>`, and print the write the transformed source text to `<output>`.

### Language Printer

A language printer is the opposite of a language parser - it's a command-line program which reads AST data for a language and outputs it's source text.

### Server

A server is provides additonal functions for Descript programs. The functions may or may not be for a particular langugage - e.g. they can include a function which takes a method identifier and provides its definition, or an additional math function.

A server consists of a command-line program and a server specification.

### Server Program

When a Descript program includes a function which isn't builtin, it looks for a server which provides the function. If it finds a server, it runs its command-line program, inputting the current context, the function name, and each argument as AST data (the current context consists of the program's command-line argument path, and the index of the syntax tree which is being processed when the function is called. The context would be important e.g. if the program wanted to lookup the body of a class whose name was passed as an argument).

### Server Specification

The server specification declares all the functions the server provides. Each function has a name. Functions can take any number of arguments - they can even take variable arguments.

A server doesn't need to fully implement a function it provides - it can return "undefined" for some inputs (e.g. if the wrong number of arguments is provided) - although it must declare all functions it implements.

Multiple servers can provide the same function, and the Descript program will keep trying each server (in undefined order) until it gets a result. If no servers return a result, the Descript program will fail.

---

**Example:**

```json
{
  "functions": [
    "Add",
    "Subtract",
    "Multiply",
    "Divide"
  ]
}
```

### AST Data

AST data is a text-based format which encodes Descript values and abstract syntax trees for any language. It consists of a sequence of "words" separated by spaces and newlines.

AST data encodes a sequence of values, and each value is separated by a newline. A primitive integer, float, or string is represented by the literal "integer", "float", or "string", followed by the (32-bit signed) integer, float, or escaped quoted string which is the content. A record is represented by the literal record's head, followed by it's children - e.g. `Foo[Bar[], 5]` is represented by `Foo Bar integer 5`. Thus AST data is prefix notation.

Each AST node corresponds to a record. The head is the created by combining the node's target language, an underscore, and its name - e.g. `Java_While`.

---

**Example:**

```s
App App Lambda string "f" App App Var string "f" Number integer 4 Number integer 5 Lambda string "x" Lambda string "y" Lambda string "z" App App Var string "+" Var string "x" App App Var string "-" Var string "z" Var string "y" Number integer 3
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
scheme'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg];
Subst[Scheme_Var[\old]; \old; \new]: \new;
Subst[scheme'(lambda (\old) \body)'; \old; \]: scheme'(lambda (\old) \body)';
Subst[scheme'(lambda (\x) \body)'; \old; \new]: scheme'(lambda (\x) \(Subst[\body; \old; \new]))';
Subst[scheme'(\f \x)'; \old; \new]: scheme'(\(Subst[\f; \old; \new]) \(Subst[\x; \old; \new]))';
Subst[\body; \; \]: \body;
```

It gets desugared into something like (technically invalid syntax, good for the example):

```descript
Subst[body; old; new].

//Lambda application
Scheme_App[Scheme_Lambda[\0; \1]; \2]: Subst[\1; \0; \2];
Subst[Scheme_Var[\0]; \0; \1]: Scheme_Var[\1];
Subst[Scheme_Lambda[\0; \1]; \0; \]: Scheme_Lambda[\0; \1];
Subst[Scheme_Lambda[\0; \1]; \2; \3]: Scheme_Lambda[\0; Subst[\1; \2; \3]];
Subst[Scheme_App[\0; \1]; \2; \3]: Scheme_App[Subst[\0; \1; \3]; Subst[\2; \2; \3]];
Subst[\0; \1; \2]: \0;
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
scheme'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg];
Subst[Scheme_Var[\old]; \old; \new]: \new;
Subst[scheme'(lambda (\old) \body)'; \old; \]: scheme'(lambda (\old) \body)';
Subst[scheme'(lambda (\x) \body)'; \old; \new]: scheme'(lambda (\x) \(Subst[\body; \old; \new]))';
Subst[scheme'(\f \x)'; \old; \new]: scheme'(\(Subst[\f; \old; \new]) \(Subst[\x; \old; \new]))';
Subst[\body; \; \]: \body;

//Addition
scheme'((+ \(Scheme_Integer[\x])) \(Scheme_Integer[\y]))': Scheme_Integer[#Add[\x; \y]];
```

However, it misses some cases, e.g. `(((lambda (f) ((f x) x)) +) 2)` should reduce to `4` but doesn't. This example handles addition properly:

```descript
Add[lhs].
Subst[body; old; new].

//Lambda application
scheme'((lambda (\x) \body) \arg)': Subst[\body; \x; \arg];
Subst[Scheme_Var[\old]; \old; \new]: \new;
Subst[scheme'(lambda (\old) \body)'; \old; \]: scheme'(lambda (\old) \body)';
Subst[scheme'(lambda (\x) \body)'; \old; \new]: scheme'(lambda (\x) \(Subst[\body; \old; \new]))';
Subst[scheme'(\f \x)'; \old; \new]: scheme'(\(Subst[\f; \old; \new]) \(Subst[\x; \old; \new]))';
Subst[\body; \; \]: \body;

//Addition
scheme'(+ \(Scheme_Integer[\x])': Add[\x];
scheme'(\(Add[\x]) \(Scheme_Integer[\y]))': Scheme_Integer[#Add[\x; \y]];
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
