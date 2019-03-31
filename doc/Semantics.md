# Semantics

## Basic

TreeScript programs are made up of **statements**. A statement is a **reducer** or **group**. A reducer transforms one AST expression into another.

```treescript
c’println(\str)’: js’console.log(\str)’;
```

A **group** organizes other statements.

```treescript
&Print[];

&Print[].
——-
c’print(\str)’: js’out += \str’;
c’println(\str)’: js’out += \str + “\n”’;
c’fprint(stderr, \str)’: js’err += \str’;
c’fprintln(stderr, \str)’: js’err += \str’;
```

When a value enters a group with an **evaluation context** sub-statement, and none of its statements match it, it’s sub-statements will transform nested expressions, based on the context reducer.
```treescript
&Arith1[].
——-
hs’\x + \y’: hs’(+) \x \y’;
E[hs’(\x)’]: hs’(\(E[\x]))’;

hs’(3 + 4)’ => hs’((+) 3 4)’;
```
```treescript
&Arith1[].
——-
hs’\x + \y’: hs’(+) \x \y’;
E[&Rec[]];

&Rec[].
——-
E[hs’\x \y \z’]: hs’\(E[\x]) \(E[\y]) \(E[\z])’;
E[hs’(\x)’]: hs’(\(E[\x]))’;

hs’(mod (49) ((3 + 4)))’ => hs’(mod (49) (((+) 3 4)))’;
```

When an AST enters a program or group defined with `——-`, every sub-statement, in sequence, transforms it once. When an AST enters a group defined with `===`, and one of its sub-statements matches it, the AST goes back to the start and the entire group tries to transform it again.

```treescript
&Arith1[].
——-
hs’\x + \y’: hs’(+) \x \y’;
hs’\x - \y’: hs’(-) \x \y’;
E[&Rec[]];

&Rec[].
——-
E[hs’\x \y \z’]: hs’\(E[\x]) \(E[\y]) \(E[\z])’;
E[hs’(\x)’]: hs’(\(E[\x]))’;

hs’(2 - 5) + (3 + 4)’ => hs’(+) (2 - 5) (3 + 4)’;
```
```treescript
&Arith1[].
===
hs’\x + \y’: hs’(+) \x \y’;
hs’\x - \y’: hs’(-) \x \y’;
E[&Rec[]];

&Rec[].
——-
E[hs’\x \y \z’]: hs’\(E[\x]) \(E[\y]) \(E[\z])’;
E[hs’(\x)’]: hs’(\(E[\x]))’;

hs’(2 - 5) + (3 + 4)’ => hs’(+) ((-) 2 5) ((+) 3 4)’;
```

A **function** performs an operation such as arithmetic or a symbol lookup. Functions come from [[Libraries]], the functions which are available depend on the libraries which are locally installed.

```treescript
c’\(C_Number[\x])’ + \(C_Number[\y])’: C_Number[#Base_Add[\x; \y]];

c’3 + 4’: c’7’;
```

When a reducer matches and transforms an expression, if there are any groups immediately after its output, they’ll further transform the expression.

TODO Subst example

When a reducer’s input matches an expression, if there are any groups immediately after, they’ll **guard** the expression. If one of the guards fails, the expression won’t be transformed.

```treescript
js’\xs.forEach(\f)’ &Func[]: js’for (\x in \xs) \body’;

&Func[].
——-
#Js_Lookup[\f]: js’function(\x) \body’;

js’
var add3 = function(n) { console.log(n + 3); };
[2, 5, 7].forEach(add3);
‘ => js’
for (n in [2, 5, 7]) {
  console.log(n + 3);
}
‘;

```

## Transformation and Guarding
When a reducer transforms an AST expression, it tries to match the expression against its input (left) value. If it matches, each associated input group guards the expression. If all guards succeed, the reducer successfully transforms the expression - it replaces it with its output (right) value, then applies associated output guards.

An expression matches an input it equals the input, except for binds (e.g. `\foo`, `\`). Binds can match any expression, except if multiple named binds have the same name, they must all match the same expression. Once an expression matches, the binds are “filled” with the corresponding sub-expressions - they’re substituted in output values and associated groups.
```treescript
Foo[Bar[\foo]; Baz[\foo]; \; \]
//Matches
Foo[Bar[Baz[5]]; Baz[Baz[5]]; 7; 8]
Foo[Bar[Baz[5]]; Baz[Baz[5]]; Baz[5]; 9]
//Doesn’t match
Foo[Bar[Baz[5]]; Baz[Qux[]]; 7]
Foo[Baz[5]; Baz[5]; 7]
```


- - - -

Complex guard example:

```treescript
hs’\x \infix \y’ &Env[]: hs’\postfix \x \y’;

&Env[].
——-
\x: &GetFixity[]: Postfix[\];
\infix &GetFixity[]: Infix[\postfix];

&GetFixity[].
——-
hs’+’: Infix[hs’(+)’];
hs’-’: Infix[hs’(-)’];
hs’*’: Infix[hs’(*)’];
hs’/’: Infix[hs’(/)’];
hs’`\x`’: Infix[hs’\(Haskell_Symbol[\x])’];
Haskell_Symbol[\x]: Postfix[hs’`\x`’];
\_: Postfix[Null[]];
```
