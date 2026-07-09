# Lexical structure and grammar

*Draft skeleton — normative text lands with the parser (ADR-0017).*

Comments: `--` to end of line. Whitespace is insignificant
(layout-insensitive).

Reserved words include: `type`, `iso`, `prim`, `where`, and — reserved for
v0.2 — `fold`, `unfold`, `for`. `†` is a Unicode alias for `~`.

```text
decl    ::= typedecl | isodecl | primdecl
typedecl::= "type" Name params? "=" ctor ("|" ctor)*
isodecl ::= "iso" name ":" ty "<->" ty (clauses | "=" expr) where?
clauses ::= ("|" pattern "<->" pattern)+
pattern ::= ctor pat* | var | literal | "$" label pat | "(" pattern ("," pattern)* ")"
expr    ::= name | "~" expr | expr ";" expr | expr "*" expr | expr "+" expr | "(" expr ")"
```

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
tooling and lowering documentation). Layout-insensitive; ML-flavored.
