# Descript-R

Third version / "attempt" of Descript. The other 2 are:

1. Descript-lang (Haskell)
2. Descript-ocaml

## Changes

*Descript code is no longer interpreted, it's now compiled into another language.*

When the program is compiled, all reducers are applied, but they create branches when dynamic matchers are encountered. Will start with a C++ backend, and can add future backends later.

### Semantics

As before, there are 2 types of objects - **values** and **reducers**. A program still consists of reducers, but there's no query - all programs read source from `stdin`, transform it, and print it to `stdout`. There are no macros or multiple phases, they're not needed. Descript no longer refactors itself directly, and if you want to write code which handles `Add`, `Subtract`, `Multiply`, and `Divide` a common way, you can do reduce these into `Arith[op: ...; left: ...; right: ...]` in the single phase. Programs must also contain record declarations.

Primitives are constructs in this other language - strings, numbers, etc. aren't built into Descript anymore, they're completely handled by the other language (e.g. C++). Also, primitives include functions. Primitives are encapsulated in single quotes, e.g. `'5:int'`, `'readline:string(*)(void)'`. Primitive's types (including splices) must be known at compile-time. Syntax sugar can add the single quotes and types, so `"Hello"` can be desugared to `'"Hello":string'`.

All record heads are static strings, and records must be declared, like in version #1. Records are compiled into structures, e.g. C++ `struct`s (or objects?).

Descript expressions are ultimately compiled into C++ values, and they can be "spliced back into primitives via the **splice** (`%`). The left argument must reduce to a primitive containing at least one `$` - the `$`s are replaced by the right argument, `$2`s are replaced by `$`s, `$3`s are replaced by `$2`s, etc. (use `\$` to escape `S`). e.g. `'print($)'%'"Hello world!"'` is equivalent to `'print("Hello world!")'`, `'add($, $2)'%'2'%'3'` is equivalent to `'add(2, 3)'`. Note that primitives aren't evaluated until they're explicitly forced by `?`. A splice is syntax sugar and gets desugared - `'foo $'%'bar'` becomes `#Splice[outer: 'foo $', inner: 'bar']`

There are 2 types of matchers - the any matcher (`<`) and a matcher which now consists of a primitive expression (`<?'foo($)'`). To test the matcher, the `$` is replaced by the matched input and the predicate is evaluated - it successfully matches if the expression evaluates to `true`. e.g. `<?'true'` is a less efficient version of the any matcher, and `<?'is_string($)'` would be a string matcher, (because of this, simple primitives might need to be wrapped with dynamic type annotations). A matcher is almost syntax sugar and gets desugared - `<` becomes `#MatchAny[]`, `<?'foo($)'` becomes `#MatchPredicate[outer: 'foo($)']`.

Like before, matchers and paths should encapsulate all abstraction. Additionally, `?` should encapsulate all evaluation / forcing.

### Compilation

Steps to compile Descript text into a backend (C++) program:

- Descript text is parsed into an AST, which contains records, target language code blocks, and backend language code blocks.
- The AST is partially desugared - splices become explicit (e.g. `Assign[lhs: <x=?'is_label($)'; rhs: <?'$ == x']` becomes `Assign[lhs: <?'{ .fill = (x) => is_label(x) }:Descript::Splice<bool>'; rhs: <?#Splice[outer: '{ .fill = (x) => { .fill = (y) => y == x } }:Descript::Splice<Descript::Splice<bool>>'; inner: >lhs]]`), but target language code blocks remain.
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

Regular Descript values can be represented in runtime C++ via `class Descript::Value` (in functional programming, a closed variant `data Descript.Value = Primitive Descript.Primitive | Record Descript.Record`):

- Backend language code blocks (with splices applied) are instances of `template <typename T> class Descript::Primitive(T value) : public Descript::Value` (in functional programming, a record `data Descript.Primitive = Primitive Descript.PrimType Descript.BackendCode` or variant `data Descript.Pritive = PrimString Descript.BackendString | Prim)`
  - The primitive type is stored in `T`, e.g. `Descript::Primitive<string>` for string templates. Primitives requiring splices are reflected in the type - `Descript::Primitive<Descript::Splice<T>>`, where `template <T> struct Descript::Splice { T (*fill)(Descript::Value* inner); }`
  - Applied splices are transformed into records - `#Splice[outer: ...; inner: ...]`
- Records are instances of `class Descript::Record : public Descript::Value` (in functional programming, a compile-time open variant `open data Descript.Record abs`)
- Target language code blocks also become instances of `Descript::Record` (like structures with type tags). *This means some backend code - the code to parse target language blocks into backend objects - should run compile-time. Either the compiler calls an existing C++ program which reuses the parsing code, the parsing code is compile-time optimized (`constexpr`), or the parsing code is run at runtime, but it runs at the start of every program (lazy solution).*
- Matchers and paths don't exist in the backend language. Instead, their logic is all added to reducers.

The C++ target language parser and `<target>::Value` are *separate* from the generated `C++` program and `Descript::Value`. The target language parser is a library linked with 2 programs, one to desugar target blocks at compile time and one to parse for generated code at runtime. The parser and `<target>::Value` can be structured however it wants, as long as the custom AST records implement `<target>::Value` and there's a global constant `<target>::serializationHeader`.

## Purpose

*Descript is now a DSL for writing code to transform other languages* (no longer just to refactor itself, although in theory you could write Descript to transform Descript, it would be the same as writing Descript to transform R). Specifically, will focus on using Descript to optimize R programs.

In order to do this faster and easier, Descript is compiled a *third* langauge.

## Program Structure

A typical Descript program will be written in **Descript** and **"backend-lang" (C++)**, then compiled into a **"backend-lang" (C++)** program, which takes a **"target-lang" (R)** program as input, transforms it, and outputs it.

A typical Descript program will now:

- Use the backend to read input (or if lazy, have input built-in to the query).
- *Use the backend to parse input into a Descript AST* - in theory, Descript can parse input directly, but in practice it takes a ridiculus amount of time.
- Use Descript to transform this AST, by matches against pieces of it and reducing those pieces
- Use Descript (more likely) or the backend (less likely) to print this AST back into source code.

### Example / Prototype

```descript
r'
for (\(<i?'is_name($)') in \(<iter)) {
  \(<x?'is_name($)')[[i]] <- \(<expr)
}
': r'
y <- as.list(x)

for (i in iter) {
  y[[i]] <- expr
}

x <- as.data.frame(y)
';
```

---

... TODO Desugaring and phases

```descript
//Transformation ("main" code)
ForIn[
  bind: <?'is_name($)'
  array: <?'!modifies($2, $)'%>body>first>target>base
  body: $[
    Assign[
      target: Bracket2[
        base: <?'is_name($2) && $2 != $'%>bind;
        index: <?'$2 == $'%>bind
      ]
      new: <?'!modifies($2, $)'%>body>first>target>base
    ]
  ]
]: ...


```

---

This program will optimize R for-loops on data frames, by converting the data frame into a list so it's not copied every time. e.g. it converts

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

(example borrowed from [Advanced R](https://adv-r.hadley.nz/names-values.html))
