# Plugins

## Libraries

TreeScript uses **libraries** to handle “typical” programming (e.g. arithmetic, algorithms, data structures), and more complex syntax operations (e.g. function declaration lookups, extra source info). Each library is an external program. When TreeScript evaluates the function call `#<Lib>_<Fun>[<arg>, ...]`, it sends the record `<Fun>[<arg>, ...]`, encoded in **AST data**, to  `stdin` of the `<Lib>` library. The library sends a value back to TreeScript through `stdout`, and that result replaces the call.

Libraries are stored in `<app-data>/treescript/env/libraries`.

## Language Plugins

In order to support a language, TreeScript must have a corresponding **language plugin**. A language plugin consists of an specification, parser, and printer. The specification defines each node in the language's AST, the "parser" converts source code into **AST data**, and the printer converts **AST data** into source code.

Language plugins are stored in `<app-data>/treescript/env/languages`.

---

The following **examples** are for a minimal lambda-calculus Scheme specification:

```s
(((lambda (f) ((f 4) 5))
  (lambda (x) (lambda (y) (lambda (z) ((+ x) ((- z) y))))))
 3)
```

## Language Specification

The language specification defines the language's file extension, and every type of node in the AST. Each node has a name and number of children. The name must consist of only uppercase letters, lowercase letters, and numbers, and it should be CamelCase.

---

(evaluates to 2).

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

## Language Parser

A language parser is a command-line program which reads a language's source text from stdin and outputs the corresponding AST data.

Each language parser must (or at least strongly should) handle indexed splices (`\#`, e.g. in `while (\0 < \1) \2++;)`), and convert `\\` occurrences to actual backslashes. When an indexed splice is encountered, in the AST data it gets converted into a `splice` node, which is followed by the splice's index. e.g. for `while (\0 < \1) \2++;)`, `\0` is encoded with `splice 0`, `1` is encoded with `splice 1`, and `2` is encoded with
`splice 2` (these nodes have no children).

Language parsers are specifically designed to be used by the TreeScript compiler, to desugar code blocks. As such, they have a strict specification - there are other ways to get source text into AST data, e.g. not using this specific command-line format, and you can use these other methods when getting the AST data to feed into a compiled TreeScript program. However, they're convenient when also paired with a TreeScript program - the command `cat <input> | <language-parser> | <treescript-program> --stdin --stdout | <language-printer> | <output>` will read source text from `<input>`, apply `<treescript-program>`, and print the write the transformed source text to `<output>`.

## Language Printer

A language printer is the opposite of a language parser - it's a command-line program which reads AST data for a language and outputs it's source code.

## Templates

You can create a plugin from a template using `treescript plugin new <name> -l <language-of-plugin-source>`. Library templates define datatypes for `Value`s and handle parsing and printing, so you only need to write the core function logic. Language templates also define `Value`s. TreeScript comes with built-in templates for some languages, and you can add more. (TODO)

Templates are stored in `<app-data>/treescript/env/templates`.
