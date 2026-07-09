# Lexical structure and grammar

*v0.1 draft. Normative for the parser (ADR-0017). The executable complement
is the conformance suite: every `conformance/**/*.thse` MUST parse under this
grammar except `diagnostics/0007-parse.thse`, which MUST be rejected with
`T0001`.*

## Lexical structure

Comments run from `--` to end of line. Whitespace (including newlines) is
insignificant — the language is layout-insensitive and ML-flavored.

### Token classes

```text
UpperName ::= [A-Z][A-Za-z0-9_]*      -- type names and data constructors
lowerName ::= [a-z][A-Za-z0-9_]*      -- iso names, pattern variables, type params
Nat       ::= [0-9]+                  -- numeric literal (elaborated per §Types)
String    ::= '"' (any char except '"' or newline)* '"'   -- prim registry key only
```

**Case discipline is significant and disambiguating** (normative). An
identifier beginning with an uppercase letter is *always* a type name or a
data constructor; one beginning with a lowercase letter is *always* an iso
name, a pattern variable, or a type parameter. This is what lets `ctor` and
`var` be told apart in a pattern with no backtracking (e.g. in `Just b`,
`Just` is a constructor and `b` is a variable). Constructors and type names
share the uppercase class and are disambiguated by grammatical position (a
name in constructor position of a `typedecl` or in a pattern is a
constructor; a name in a `ty` is a type).

### Reserved words

`type`, `iso`, `prim`, `where`; and reserved for v0.2: `fold`, `unfold`,
`for`. Reserved words are not `lowerName`s and cannot be used as identifiers.

### Punctuation and operators

`(` `)` `,` `|` `:` `;` `*` `+` `~` `†` `$` `=` and the arrow `<->`.

`<->` is a single indivisible token. A partial arrow such as `<-` (see
`diagnostics/0007-parse.thse`) is not `<->`; encountering it where `<->` is
required is a `T0001` parse error. `†` is a Unicode alias for `~` and lexes
to the same token.

## Grammar

```text
program  ::= decl*
decl     ::= typedecl | isodecl | primdecl

typedecl ::= "type" UpperName tyvar* "=" ctordef ("|" ctordef)*
ctordef  ::= UpperName atomTy*                 -- Nothing | Just Bit | L Bit
tyvar    ::= lowerName                         -- type parameter (monomorphized, §Types)

isodecl  ::= "iso" lowerName ":" ty "<->" ty isobody
isobody  ::= clauses where?
           | "=" expr  where?
clauses  ::= ("|" pattern "<->" pattern)+
where    ::= "where" isodecl+

primdecl ::= "prim" lowerName ":" ty "<->" ty "=" String

ty       ::= UpperName                          -- named ADT or stdlib numeric (Bit, U8, …)
           | tyvar                              -- type parameter
           | "0" | "1"                          -- core empty / unit types
           | "(" ty ("," ty)* ")"               -- grouping (1 elem) / product (≥2 elems)

pattern  ::= UpperName atomPat*                 -- constructor, nullary if no args
           | "$" lowerName atomPat*             -- label form ($go, $go b, $go (b,(O,O)))
           | atomPat
atomPat  ::= lowerName                          -- variable
           | UpperName                          -- nullary constructor
           | Nat                                -- numeric literal
           | "(" pattern ("," pattern)* ")"     -- grouping / tuple pattern

expr     ::= "~" expr
           | expr ";" expr
           | expr "*" expr
           | expr "+" expr
           | "(" expr ")"
           | lowerName                          -- reference to an iso
```

Notes on the productions:

- **A bare `UpperName`** matches both `pattern ::= UpperName atomPat*` (with
  zero arguments) and `atomPat ::= UpperName`; both denote the nullary
  constructor and the choice is immaterial. A constructor is applied form
  when followed by one or more `atomPat` starters.
- **`0` and `1`** are the core empty/unit *types* in `ty` position and
  numeric *literals* (`Nat`) in `atomPat` position; the two readings never
  collide because type and pattern positions are grammatically distinct.
- **`String`** appears only as a `primdecl` registry key; it is not an
  `expr` atom.
- **`where`** attaches to a single `isodecl`. Its nested `isodecl`s are in
  scope in the parent's `isobody` (clauses or `expr`) and shadow outer
  bindings of the same name; they may reference one another and outer names
  (forward references permitted). v0.1 admits no recursive isos, so the local
  call graph is acyclic.

## Tuples nest right-associatively (normative)

A parenthesized list of two or more elements is right-associated sugar for
nested binary products:

```text
(t1, t2, …, tn)  ≡  (t1, (t2, (… , tn)))        for n ≥ 2   -- in ty
(p1, p2, …, pn)  ≡  (p1, (p2, (… , pn)))        for n ≥ 2   -- in patterns
```

So `(Bit, Bit, Bit)` (P0006) is `(Bit, (Bit, Bit))`, and a 3-place pattern
`(O, b, t)` is `(O, (b, t))`. A single parenthesized element is grouping:
`(Bit) ≡ Bit`. This convention is what makes P0015's re-association
(`(Bit,(Bit,Bit)) <-> ((Bit,Bit),Bit)`) a well-typed structural iso and fixes
the leaf order the canonicalizer flattens (ADR-0014; test-spec-m1 ex. 4). It
also fixes the value-space size the coverage checker enumerates — `|(Bit,Bit,
Bit)| = 8` either way, but the *shape* over which patterns are matched is the
nested one.

## Operator precedence and associativity (normative)

| Level | Operator | Arity/assoc | Meaning |
|---|---|---|---|
| 1 (tightest) | `~` (alias `†`) | prefix | adjoint |
| 2 | `*` | infix, left | parallel (product) |
| 3 | `+` | infix, left | choice (sum) |
| 4 (loosest) | `;` | infix, left | sequential, diagram order |

Consequences an implementation MUST reproduce: `~f ; g` ≡ `(~f) ; g`
(adjoint does not distribute over the sequence without parentheses);
`a ; b * c` ≡ `a ; (b * c)`; `a + b * c` ≡ `a + (b * c)`.
Left-associativity is semantically inert (all three infix operators are
associative) but is normative for AST shape, pretty-printing, and span
attribution.

Constructor order in `typedecl` is significant (it is observable through
tooling and lowering documentation, and fixes the structural sum order — P0010
pins `L` = left injection). Layout-insensitive; ML-flavored.

## Coverage of the conformance suite

Every construct exercised by the suite is generated by the grammar above:
named nullary/unary ADTs and `type … = C | C Bit` declarations (P0001–03, 10);
tuple types and patterns including 3-place and nested (P0004–06, 15);
`= expr` bodies over `~ ; * +` (P0007–10); label patterns `$go …` (P0011);
`prim … = "…"` with a string key (P0012, D0009); `where` (P0013); `Nat`
literals against `U1` (P0014). The sole rejection is `diagnostics/0007-parse.
thse`'s malformed `<-` → `T0001`.
