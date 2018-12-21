# Summary

TODO Update

## Specification

### Value

A **value** is a data structure.

There are 2 types of values:

- A **primitive** is a number, string, image, etc.
- A **record** consists of a **head** and **properties**. The head is a string, and each property consists of a key (string) and value (a value). For example, the record `Foo[bar: 5]` consists of the head `"Foo"` and a single property, `bar: 5` - the property's key is `bar`, the value is `5`.

> A value can encode any type of data. Examples:
>
> - Physical objects: `Flower[color: Blue[]]`, `Forest[]`, `Ocean[name: “Atlantic”]`.
> - Math equations: `Equals[left: 4, right: Plus[left: Multiply[left: 1, right: 2], right: 2]`.
> - Source code: `HTML[head: $[Title[content: “My Website”]], body: $[Div[width: 100, height: 100, ...], ...]]`.
> - Operations: `Apply2[func: Append2[], left: “Hello”, right: “ world”]`.
>
> Furthermore, many different values encode the same form of data. Reusing the above examples:
>
> - Physical objects (alt): `Tulip[]`, `$[Tree[], Tree[], Tree[], ...]`, `”The Atlantic Ocean”`.
> - Math equations (alt): `”4 = ((2 * 1) + 2)”`.
> - Source code (alt): `Node[name: “html”, children: $[Node[name: “head”, children: $[Node[name: “title”, children: $[Text[content: “My Website”]]]]]], $[Node[name: “body”, children: $[Node[name: “div”, attributes: $[Entry[key: “width”, value: “100”], Entry[key: “height”, value: “100”]]]]]]`.
> - Operations (alt): `Append[left: “Hello”, right: “ world”]`.
>
> _There’s no “ideal” way to represent any type of data in a value. Some representations are clearer to the programmer, some are clearer to the computer, some are clearer to mathematicians, some are clearer to scientists, some expose broad details but hide narrow ones, some expose narrow details but hide broad ones._

### Reducer

A **reducer** is a data transformer, like a function.

A reducer consists of an **input** value and **output** value. For example, `Foo[]: "Bar"` consists of the input value `Foo[]` and the output value `"Bar"`.

When a reducer is applied to a value, if its input **matches** the value, it will transform ("reduce") the value by replacing it with its output. For example, `Foo[]: "Bar"` will reduce `Foo[]` into `"Bar"`.

> A reducer can transform any value into any other value. These reducers directly transform the examples from the first section into those from the second section.
>
> - Physical objects: `Tulip[]: Flower[color: Blue[]]`, `Forest[]: $[Tree[], Tree[], Tree[], ...]`, `Ocean[name: “Atlantic”]: ”The Atlantic Ocean“`.
> - Math equations: `Parse[“4 = ((2 * 1) + 2)”]: Equals[left: 4, right: Plus[left: Multiply[left: 1, right: 2], right: 2]`.
> - Source code: `HTML[head: $[Title[content: “My Website”]], body: $[Div[width: 100, height: 100, ...], ...]]: Node[name: “html”, children: $[Node[name: “head”, children: $[Node[name: “title”, children: $[Text[content: “My Website”]]]]]], $[Node[name: “body”, children: $[Node[name: “div”, attributes: $[Entry[key: “width”, value: “100”], Entry[key: “height”, value: “100”]]]]]]`.
> - Operations: `Apply2[func: Append2[], left: “Hello”, right: “ world”]: Append[left: “Hello”, right: “ world”]`.

### Matcher / path

> _Matchers and paths are the abstractions of Descript._

A **matcher** is an input value, and it will match any value of a particular type.

There are 6 matchers:

- `<`: matches any value.
- `<Prim`: matches a primitive.
- `<Record`: matches a record.
- `<Number`: matches a number primitive.
- `<String`: matches a string primitive.
- `</regexp/`: matches a String which conforms to the regular expression `regexp` (e.g. `</.*_(Foo|Bar)/` matches `”abc_Foo”`, `xyz_Bar`, etc.).

```
Foo[a: <]
-- matches --
Foo[a: 4]
Foo[a: "Hello"]
Foo[a: Bar[c: XYZ[]]]
Foo[a: "example@mail.com"]

Foo[a: <Prim]
-- matches --
Foo[a: 4]
Foo[a: "Hello"]
Foo[a: "example@mail.com"]

Foo[a: <Record]
-- matches --
Foo[a: Bar[c: XYZ[]]]

Foo[a: <Number]
-- matches --
Foo[a: 4]

Foo[a: <String]
-- matches --
Foo[a: "Hello"]
Foo[a: "example@mail.com"]

Foo[a: </\w@\w\.(com|org|net|edu)]
-- matches --
Foo[a: "example@mail.com"]
```

> The reducers in the above sections each only transform a single value, becuase they only match one value (their input). A reducer with a matcher can match and transform multiple values.

```
Demolish[item: <]: Dust[];
-- equivalent to --
Demolish[item: "Car"]: Dust[];
Demolish[item: Tree[]]: Dust[];
Demolish[item: Mailbox[color: "Blue"]]: Dust[];
Demolish[item: Building[height: 40]]: Dust[];
...
```

A **path** is an output value. It **references** a part of another value.

The path `>` references the entire value. The path `>^` references the value's head (the value must be a record). The path `>a` references the value's property `a`. `>a>b` references the value's `a` property’s `b` property. And so on.

```
> of Foo[a: Bar[a: 5], b: Baz[c: Qux[]]] is Foo[a: Bar[a: 5], b: Baz[c: Qux[]]]
>^ of Foo[a: Bar[a: 5], b: Baz[c: Qux[]]] is "Foo"
>a of Foo[a: Bar[a: 5], b: Baz[c: Qux[]]] is Bar[a: 5]
>a>a of Foo[a: Bar[a: 5], b: Baz[c: Qux[]]] is 5
>b>c>^ of Foo[a: Bar[a: 5], b: Baz[c: Qux[]]] is "Qux"
```

When a reducer transforms a value and its output is a path, instead of replacing the original value with the path, it will replace with the part of the original value which the path references.

```
Foo[a: <]: >a
---
Foo[a: "abc"] => "abc"
Foo[a: "xyz"] => "xyz"

Foo[a: <]: Bar[b: >a]
---
Foo[a: "abc"] => Bar[b: "abc"]
Foo[a: "xyz"] => Bar[b: "xyz"]
```

> A reducer with matchers and paths can perform complex operations, like recursion.

```
Build[n: 0, val: <]: Nil[]
Build[n: <Number, val: <]: Cons[first: >val, rest: Build[n: Sub1[n: >n], val: >val]]
Forest[]: Build[n: 10, val: Tree[]]

Forest[] => Cons[first: Tree[], rest: Cons[first: Tree[], rest: Cons[first: Tree[], rest: ...]]] = $[Tree[], Tree[], Tree[], ...] //Syntax shortcut
```

### Remainder

A **remainder** substitutes a regular property in an input or output value. A remainder has the syntax `...: _` - it has the same syntax as a regular property, but the key is the literal `...`.

In an input value, the remainder will match an arbitrary number of properties which also match its value.

```
Color[...: <Number; alpha: 0]: Clear[]
---
Color[red: 0.7; green: 0.3; blue: 0.8; alpha: 0] => Clear[]
Color[cyan: 0.4; magenta: 0.6; yellow: 0.8; lightness: 0.3; alpha: 0] => Clear[]
Color[hue: 0.9; saturation: 0.1; lightness: 0.5; alpha: 0] => Clear[]
Color[black: 1; alpha: 0] => Clear[]
Color[white: 1; alpha: 0] => Clear[]
Color[alpha: 0] => Clear[]
```

In an output value, the remainder value must contain a path with a **remainder path element** (a literal `...` in a path). The remainder path element matches all of a record's properties, and these will be "spliced" into the remainder's position.

```
Map[f: <; as: Vector[...: <]]: Vector[...: Apply[f: >f; a: >as>...]]
Apply[f: Add[left: <Integer]; a: <Integer]: #Add[left: >f>left; right: >a]
---
Map[f: Add[left: 3]; as: Vector[x: 2; y: 3; z: 4]] => Vector[x: 5; y: 6; z: 7]
Map[f: Add[left: -2]; as: Vector[x: 0; y: 1]] => Vector[x: -2; y: -1]
Map[f: Add[left: 10]; as: Vector[w: 64; x: 32; y: 16; z: 8]]: Vector[w: 74; x: 42; y: 26; z: 18]
```

The remainder path element `...` will match all of a record's properties. A remainder path can exclude some properties, for example `...-foo-bar` will match all properties except for `foo` and `bar`

```
Darken[Color[...: <Number; alpha: <Number]]: Color[
  ...: #Multiply[left: >a>...; right: 0.75]
  alpha: >a>alpha
]
---
Darken[Color[red: 0.8; green: 0.4; blue: 0.2; alpha: 0.5]]: Color[
  red: 0.6
  green: 0.3
  blue: 0.15
  alpha: 0.375
  alpha: 0.5
] //Not right

Darken[Color[...: <Number; alpha: <Number]]: Color[
  ...: #Multiply[left: >a>...-alpha; right: 0.75]
  alpha: >a>alpha
]
---
Darken[Color[red: 0.8; green: 0.4; blue: 0.2; alpha: 0.5]]: Color[
  red: 0.6
  green: 0.3
  blue: 0.15
  alpha: 0.5
] //Better
```

2 separate remainder paths can be in the same remainder, as long as they refer to the same keys in the same order. A path can contain multiple remainder elements, in which case it needs to be in nested remainders.

```
Zip[left: List[...: <]; right: List[...: <]]: List[...: Tuple[left: >left>...; right: >right>...]]
---
Zip[
  left: List[a: 5; b: 10; c: 15]
  right: List[a: 20; b: 25; c: 30]
]: List[
  a: Tuple[left: 5; right: 20]
  b: Tuple[left: 10; right: 25]
  c: Tuple[left: 15; right: 30]
]

Transpose[List[...: List[...: <]]]: List[...: List[...: >a>...>...]]
---
Transpose[List[
  a: List[a: 1; b: 2; c: 3]
  b: List[a: 2; b: 3; c: 4]
  c: List[a: 3; b: 4; c: 5]
  z: List[a: 0; b: 0; c: 0]
]] =>
List[
  a: List[a: 1; b: 2; c: 3; z: 0]
  b: List[a: 2; b: 3; c: 4; z: 0]
  c: List[a: 3; b: 4; c: 5; z: 0]
]
```

> A reducer with remainders can transform a record with arbitrary properties. A reducer with remainders and a matcher head can transform an arbitrary record.

```
{<}[...: <]: XYZ[]
-- equivalent to --
<Record: XYZ[]
```

### Macro

A **macro** is a reducer which gets applied to other reducers - specifically, it gets applied to the input and output values in the reducers.

Programs are organized into **phases**. Every program contains at least one phase, the **bottom phase**, which contains a single value called the **query**. All other phases contain reducers. Reducers in higher phases are applied to reducers in lower phases. Phases are separated by `——-`.

```
Speak[text: <String; target: <]: Habla[mensaje: >text; blanco: >target];
Print[text: <String]: Mostra[mensaje: >text];
User[]: Usario[];
“Hello”: “Hola”;
——-
Speak[text: <String; target: User[]]: Print[text: >text];
Michael[]: User[];
——-
Speak[text: “Hello”; target: Michael[]]?

===

Habla[mensaje: <String; blanco: Usario[]]: Mostra[mensaje: >mensaje];
Michael[]: Usario[];
——-
Habla[mensaje: “Hola”; blanco: Michael[]]?

===

Mostra[mensaje: “Hola”];
```

#### Side reduction

When a reducer is applied to another reducer's input value, it will transform the corresponding references in the reducer's output value, and vice versa. These transformations are called **side reductions** (combines "side effect" and "reduction").

```
Minus[left: <Number; right: <Number]: Subtract[this: >right; from: >left];
---
Minus[left: <Number; right: <Number]: #Subtract[left: >left; right: >right];
Minus[left: <Number; right: Minus[left: <Number; right: <Number]]: Plus[left: Minus[left: >left; right: >right>left]; right: >right>right];

===

Subtract[this: <Number; from: <Number]: #Subtract[left: >from; right: >this];
Subtract[this: Subtract[this: <Number; from: <Number]; from: <Number]: Subtract[this: >this>this; from: Subtract[this: >from>this; from: >from]];
```

TODO

#### Instant edit

TODO

- - - -

## Conventions

### Value Head

Typically, a record's head will be a symbol, e.g. `Foo[]` has the head `Foo`. However, a record head can also be a value, surrounded by `{...}`. For example, `{Foo[]}[bar: 3]` is a valid record. A symbol head is equivalent to a string head - e.g. `Foo[]` could also be represented as `{"Foo"}[]`.

Regexp matchers as heads are especially useful, because they create reducers which operate on arbitrary records.

```
Apply[func: {</.*\D+2/}[]; left: <Any; right: <Any]: {#Regex[pattern: “(.*\D+)2”, input: >func>^]}[left: >left; right: >right];
ZipWith[func: <; left: Nil[]; right: Nil[]]: Nil[];
ZipWith[func: <; left: Cons[first: <; rest: <]; right: Cons[first: <; rest: <]]: Cons[
  first: Apply[func: >func; left: >left>first; right: >right>first];
  rest: ZipWith[func: >func; left: >left>rest; right: >right>rest]
];
Add[left: <Number; right: <Number]: #Add[left: >left, right: >right];
Subtract[left: <Number; right: <Number]: #Subtract[left: >left, right: >right];
---
Res[
  a: ZipWith[
    func: Add2[];
    left: Cons[first: 3; rest: Cons[first: 2; rest: Cons[first: 5; rest: Nil[]]]];
    right: Cons[first: 2; rest: Cons[first: 8; rest: Cons[first: 15; rest: Nil[]]]]
  ];
  b: ZipWith[
    func: Subtract2[];
    left: Cons[first: 3; rest: Cons[first: 2; rest: Cons[first: 5; rest: Nil[]]]];
    right: Cons[first: 2; rest: Cons[first: 8; rest: Cons[first: 15; rest: Nil[]]]]
  ]
]?

=>

Res[
  a: Cons[first: 1; rest: Cons[first: -6; rest: Cons[first: -10; rest: Nil[]]]];
  b: Cons[first: 5; rest: Cons[first: 10; rest: Cons[first: 20; rest: Nil[]]]]
]
```

### Reducer abstraction

In Descript, you can’t create a “reducer reducer”, e.g. `(Foo[]: Bar[]): (Baz[]: Qux[])`. However, you can still abstract a reducer, by writing 2 separate macro reducers - one for its input, and another for its output.

```
TODO => (Add[left: Approx[a: <Any[]], right: Approx[a: <Any[]]]: Approx[a: Add[left: >left>a, right: >right>a]) | (Subtract[left: Approx[a: <Any[]], right: Approx[a: <Any[]]]: Approx[a: Subtract[left: >left>a, right: >right>a]) | ...
```

These macro reducers can be further abstracted by reducers in even higher phases.

```
TODO => (Add[left: Approx[a: <Any[]], right: Approx[a: <Any[]]]: Approx[a: Add[left: >left>a, right: >right>a]) | (Subtract[left: Approx[a: <Any[]], right: Approx[a: <Any[]]]: Approx[a: Subtract[left: >left>a, right: >right>a]) | ...
```

However, _be careful not to abstract too much. Often, the more abstraction, the less code you need to write, but the harder it is to read the code._
