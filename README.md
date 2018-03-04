## Credit:
Credit for design and implementation of Theseus goes entirely to
Roshan P. James and Amr Sabry of Indiana University. This is currently
just a mirror, though it would be cool to hack on it.

# Theseus, the programming language.

Why create another programming language in this time and age? And why
make such an obscure finicky one wherein it's not obvious how one can
write a web server or an iphone app?  Theseus exists due to a certain
philosophical point of view on computation. What follows is a casual
overview.

The paradox of the Ship of Theseus was originally proposed by Plutarch
in the setting of the ship that the Greek hero Theseus used for his
adventures. Since the Athenians admired Theseus they took on the task
of preserving the ship in his memory. Over time, as parts of the ship
got spoiled, they replaced them with equivalent new parts. Thus the
ship was always in good shape.

However one day, many years later, someone makes the observation that
the ship no longer had any original parts. The entirety of the ship
had been replaced piece by piece. This raises the question: is this
ship really still the ship of Theseus? 

And, if not, when does the change take place? When the first part is
replaced? When the last one is replaced? When about half the parts are
replaced?

Further, to compound the question, Thomas Hobbes added the following
corollary: Imagine a junk yard outside Athens where all the discarded
parts of the ship had been collected. If the proprietor of the junk
assembled the parts into a ship, which ship is now the real ship of
Theseus?

This is a philosophical paradox about the nature of equality and
identity. The question also applies to people. Since we are constantly
changing, cells and thought patterns continuously being replaced, is
any person the same person they were some time ago? Can anyone ever
not change? Is a lactose free, sugar free, gluten free cupcake still a
cupcake?

Our programming language is named Theseus because, like in the
paradox, the main computation step involves replacing a value by an
isomorphic value. Since isomorphic things really are the same, have we
changed the value? And if not, what have we computed? Have we computed
at all? As we keep doing these replacement in our program we will have
transformed the value that was the input to the value that is the
output.

There are many answers to the paradox of the ship of Theseus. The
obvious answers of saying either 'yes, it is the same ship' or 'no
it's not' have their justifications. Other answers exist too. One is
to say that the question is wrong and that it meaningless to ask this
sort of question about identity. Like a river whose very nature is to
flow and change from moment to moment, so is nature of the ship. Over
time the ship changes, just like the river, and it is meaningless to
ask if is the same ship. It is our flawed notion of time and identity
that makes us assume that the ship is not like the river and to expect
one to have static identity and the other one to not.

Theseus owes its design to a certain philosophical point of view about
computation. Turing machines and the lambda-calculus are abstract
models of computation. They were invented by people as mental models
of how computation may be expressed. These are meant to serve as
conceptual entities and we may think of them as being free of physical
constraints, i.e. it does not matter if you have the latest laptop, a
whole data center or no computer at all, the ideas underlying the
abstract models apply equally well. However there is something wrong
with this view. 

Abstract models of computation are meant to apply to our physical
reality. We might be constrained by the resources we have at hand to
do some computations. For instance, we might not have enough memory to
run some programs. However, we do expect that models are compatible
with our physics. For example, if we imagined that each step of a
Turing machine took only half the amount of time the previous step
took to execute, after 2 units of time time all Turing machines would
have observable results. Such a machine is impossible to build (except
maybe in the movie Inception where you could fall asleep causing time
to speed up in each recursive dream step). Its not just the difficulty
of building it thats at issue here.  This violates the our notion of
space and time in a deep way. Futher it gives rise to issues in our
mathemaics and formal logics since it resolves the halting problem. We
think of such models of computations that violate physics merely as
curiosities and not as modeling computation in the physical world.

The line of work that Theseus comes from stems from the idea that
abstract models of computation, must in their essence be compatible
with our physics. Mordern physics at the level of quantum mechanics
describes a universe where every fundamental action is reversible and
fundamental quantities such as energy, matter and information are
conserved. The notion of conservation of information stems from a line
of thought originating from Maxwell's paradox of the "demon" that
seemed to violate the Second law of Thermodynamics and its current
accepted resolution set forth by Ralph Landauer. Landauer argued
that the demon must do thermodynamic work to forget the information
that its learns about the speed of each particle. This act of
forgetting information implied that information should be treated as a
physical quantity subject to conservation laws. In more recent years,
Pasquele Malacaria and others wrote about the entropy of computation
and Eric Lutz and others experimentally verified Landauer's
principle. ("Maxwell's Demon 2" by Leff and Rex is a pretty good read
on the general topic; the first part of the book is pretty accessible
and the later part is largely a collection of historically relevant
papers.)

So the physics that Theseus is concerned with is this strange new
physics where information is not longer a concept of the human mind,
like love and peace, but a physical entity. This has happened to
computation before; before Turing worked out that computation itself
had a formal notion that can be cpatured by abstract computational
models, computation itself was considered an activity of the human
mind not subject to the rigors of mathematics and logic.

The model of computation that was originally devised in this regard
came out under the longish name of 'dagger symmetric traced bimonoidal
categories'. The name was too long, was somewhat imprecise and didn't
really say much at all. The underlying idea was that we could look for
computation in the rules of equality. 

If every allowed operation is a transformation of one quantity for
another equivalent quantity, then computation should preserve
information. However, can we even compute like this? After all, our
conventional models of computation allows us to do "logical or" and
"logical and" reducing two bools into one bool. Changing the value of
a variable means loosing the information in that variable
forever. Conditional statements and loops seem to inherently be lossy
operations because it is unclear how to execute them in reverse
without knowing which banches were originally taken. And more
importantly, in the words of Barry Mazur, when is one thing equal to
some other thing?

We settled on simplest notions of equality, those familiar from
arithmetic. Rules like: 

```
0 + x       = x
x + y       = y + x
(x + y) + z = x + (y + z)

1 * x       = x
x * y       = y * x
(x * y) * z = x * (y * z)

x * 0       = 0
x * (y + z) = x * y + x * y

if x = y and y = z, then x = z
if x = y and w = z, then x + w = y + z
if x = y and w = z, then x * w = y * z
if x + y = x + z, then y = z
```

We then took the numbers represented by `x`, `y` etc to be a measure
of the "amount" of information and we only allowed operation that
corresponded to these equalities. Thus each operation preserved the
amount of information.

For a while it was unclear that what we had was even a model of
computation, i.e. it took us a while to learn to express programs in
it. Figuring out how to do the equivalent of a conditional took a long
time in that setting. The resulting model of computation was one where
every primitive operation was a sort of type isomorphism. When we add
recursive types to the mix, the model becomes Turing
complete. Programs in this early model were easier to represent as
diagrams. Each program was essntially a complex wiring diagram where
each wire had a type and process of "running" the program was the
process of tracing the flow of particles through these wires. In the
references below you can find lots of details about all this.

## A Gentle Introduction to Theseus

While all of this worked out in theory, it was tedious constructing
programs. For attempts of programming directly in the above model see
[PDF](http://www.cs.indiana.edu/~sabry/papers/cat-rev.pdf),
[PDF](http://dl.acm.org/citation.cfm?id=2103667&dl=ACM&coll=DL&CFID=370820997&CFTOKEN=65718506)
and [PDF](http://link.springer.com/chapter/10.1007%2F978-3-642-36315-3_5).
This was when Theseus happened and we realized that we could express
the computations of the above information preserving model in a format
that looked somewhat like a regular functional programming
language. Much like the situation of replacing ship parts with other
equivalent ship parts, Theseus computes by replacing values with
equivalent values.

All the programs in Theseus are reversible and the type system will
prevent you from writing anything that isnt. You can program in
Theseus without knowing anything about the body of theory that
motivated it. 

```haskell
-- booleans
data Bool = True | False

-- boolean not 
iso not :: Bool <-> Bool
| True  <-> False
| False <-> True
```

Theseus has algebraic data types. It has no GADTs or polymorphism yet,
but those are boring and can be added later. For the purpose of this
presentation I will pretend that we do have polymorphism though. The
equivalent of functions in Theseus are the things called `iso`. The
`iso` called `not` maps `Bool` to `Bool`. In a coventional functional
language the part on the left hand side of the `<->` is called the
pattern and the part on the right is called the expression. In
Theseus, both the LHS and the RHS of the `<->` are called patterns. In
Theseus patterns and expressions are the same thing.

Now here is the interesting bit: The patterns on the right hand side
and the patterns on the left hand side should entirely cover the
type. On the RHS we have

```
:: Bool <->
| True  <->
| False <->
```

which do indeed cover all the cases of the type `Bool`. On the LHS we
have

```
:: <-> Bool
|  <-> False
|  <-> True
```

which also covers the type bool. Patterns cover a type, when every
value of the type is matched by one and only one pattern. So the
following single pattern does not cover the type `Bool` since there
the value `True` is unmatched.

```
:: Bool
| False
```

The following also does not cover the type `Bool` since the value
`False` can be matched by both patterns. Here the variable `x` is a
pattern that matches any value.

```
:: Bool
| False
| x
```

The following does cover the type `Bool`:

```haskell
:: Bool
| x
```

Here is another type definition and another `iso`:

```haskell
data Num = Z | S Num 

iso parity :: Num * Bool <-> Num * Bool
| n, x                   <-> lab $ n, Z, x
| lab $ S n, m, x        <-> lab $ n, S m, not x
| lab $ Z, m, x          <-> m, x
where lab :: Num * Num * Bool
```

Here the `lab` is called a label and labels are followed by a `$`
sign. All the patterns that have no label should cover the type of the
corresponding side of the function.

```haskell
:: Num * Bool <-> Num * Bool
| n, x        <-> 
|             <-> 
|             <-> m, x
```

Here `n, x` on the LHS do cover the type `Num * Bool`. The variable
`n` matches any `Num` and the variable `x` matches any `Bool`. The RHS
is covered similalry by `m, x`.  The label `lab` has the type `Num *
Num * Bool` and the intention is that the patterns of the label on the
LHS and the RHS must each should cover the type of label.

```haskell
:: Num * Num * Bool <->
|                   <-> 
| lab $ S n, m, x   <-> 
| lab $ Z, m, x     <-> 
```

On the LHS we see that every value of the type `Num * Num * Bool` is
matched by one of the two patterns. The same applies to the RHS
patterns of the `lab` label.

Labels are a way of doing loops. When a pattern on the LHS results in
a label application on the RHS, control jumps to the LHS again and we
try to match the resulting value against a pattern of teh label on the
RHS. This continues till we end up in a non-labelled pattern on the
right. So lets trace the execution of `parity` when we given it the
input `S S S Z, True` of the type `Num * Bool`.

```
S S S Z, True           -> lab $ S S S Z, Z, True 
lab $ S S S Z, Z, True	-> lab $ S S Z, S Z, False
lab $ S S Z, S Z, False -> lab $ S Z, S S Z, True
lab $ S Z, S S Z, True	-> lab $ Z, S S S Z, False
lab $ Z, S S S Z, False -> S S S Z, False 
``` 

So parity of `S S S Z, True` applied `not` to `True` three times,
resulting in `S S S Z, False`.  Here is one more `iso`:

```haskell
iso add1 :: Num <-> Num 
| x            <-> ret $ inR x
| lab $ Z      <-> ret $ inL () 
| lab $ S n    <-> lab $ n
| ret $ inL () <-> Z
| ret $ inR n  <-> S n
where ret :: 1 + Num
      lab :: Num
```

This `iso` has two labels `lab` and `ret` and one can verify that the
same coverage contraints hold. Here the type `1 + Num` would be
written as `Either () Num` in Haskell, i.e. `1` is the unit type that
has only one value. The value is denoted by `()` and is read as
"unit".  One can run `add1` on any `n` of type `Num` and verify that
we get `S n` back as the result.

Now here is the next interesting bit: For any Theseus iso, we can get
its inverse iso by simply swapping the LHS and RHS of the
clauses. This also get at the essence of the idea of information
preservation: if we have a program and an input to it, we can run the
input through the program and get the program and the output.  Using
the program and the output, we can recover the input.  

Some programs may not terminate on some inputs and in such cases it is
meaningless to ask about reverse execution. Given that Theseus is a
Turing complete language it is not surprising that some executions are
non-terminating. What however may be surprising that information
preservation holds in the presence of non-termination.

Here the reverse execution of `add1`, lets call it `sub1` does indeed
diverge on the input `Z`. For every other value of the form `S n` it
returns `n`. Its worth tracing this execution and thinking about why
this comes about in Theseus.

Theseus also supports the notion of parametrizing an iso with another
one. For example:

```haskell
iso if :: then:(a <-> b) -> else:(a <-> b) -> (Bool * a <-> Bool * b)
| True, x  <-> True, then x
| False, x <-> False, else x
```

Here the iso called `if` takes two arguments, `then` and `else`, and
decides which one to call depending on the value of the boolean
argument. Theseus doesn't really have higher-order or first-class
functions. The parameter isos are expected to be fully inlined before
transofrmation of the value starts. Theseus will only run a value
`v:t1` on an iso of type `t1 <-> t2`, and the `->` types in the above
should be fully instantiated away. 

Running `if ~then:add1 ~else:sub1` on the input `True, S Z` gives us
the output `True, S S Z` and running the same function with input
`False, S Z` gives us `False, Z`. The syntax of labelled arguments is
similar to that used by OCaml.

Reverse evaluation works by flipping LHS and RHS in the same way as
before. However, you wonder, what happens when there is a function
call in a left hand side pattern? Here is a simple example:

```haskell
iso adjoint :: f:(a <-> b) -> (b <-> a)
| f x <-> x
```

Here is the interesting bit again: An iso call on the left side
pattern is the dual of an iso call on the right hand side. When `f x`
appears on the right we know that we have an `x` in hand and the
result we want is the result of the application `f x`.  When `f x`
appears on the left it means that we have the result of the
application `f x` and we want to determine `x`. We can infer the value
of `x` by tracing the flow of the given value backwards through `f`
and the result of the this backward execution of `f` is `x`. We can do
this because isos represent information preserving transformations.

So if we had `add1` and its inverse `sub1`, then `adjoint ~f:add1` is
equivalent to `sub1` and `adjoint ~f:(adjoint ~f:add1)` is equivalent
to `add1`. 

Some references for additional reading.

* Roshan P. James and Amr Sabry. Theseus: A High Level Language for
  Reversible Computing. Work-in-progress report in the Conference on
  Reversible Computation, 2014.
  [PDF](http://www.cs.indiana.edu/~sabry/papers/theseus.pdf)

  This is the paper that introduces Theseus.  The syntax of Theseus as
  presented here differs somewhat from what is in the paper, but most
  of the these syntactic differences are superficial and largely an
  artifact of the difficulty of having Parsec understand location
  sensitive syntax.  Please see below for some notes on how the
  implementation of Theseus differs from that in the paper. For more
  of the academically relevant citations, you can chase the references
  at the end of the paper.

* Harvey Leff and Andrew F. Rex. Maxwell's Demon 2 Entropy, Classical
  and Quantum Information, Computing. CRC Press, 2002. 
  [Amazon](http://www.amazon.com/Maxwells-Entropy-Classical-Information-Computing/dp/0750307595)

  This book is a survey of about a century and a half of thought about
  Maxwell's demon with significant focus on Landauer's principle and
  the work surrounding it.

# Running Theseus Programs

For now Theseus does not have a well developed REPL and is still work
in progress. We use the Haskell REPL to run it. Roughly:

```shell
$ cd examples/
$ ghci ../src/Theseus.hs 
[...]
Ok, modules loaded: Theseus.
*Theseus> run "peano.ths"
[...]
-- {Loading bool.ths}
Typechecking...
Evaluating...
eval toffoli True, (True, True) = True, (True, False)
eval toffoli True, (True, False) = True, (True, True)
[...]
*Theseus> 
```

Like the `run` function above, we also have `echo` which parses the
given file and echoes it on screen and `echoT` which prints out all
the definitions after parsing the given file and inlining all the
imports. `echoT` also checks for violations of non-overlapping and
exhaustive pattern coverage and reports these. For instance:

```shell
*Theseus> echoT "test.ths"
[...]
iso f :: Bool <-> Bool
| True <-> False
| x <-> True
 
Error: LHS of f: Multiple patterns match values of the form : True
*Theseus> 
```

# Differences between the implementation and the RC 2014 paper

The current implementation of Theseus is of experimental status. There
are many niceties and tools required for a proper programming language
that are not available as yet. When editing Theseus programs turn on
Haskell mode in Emacs and that tends to work out ok. 

Here are some notable differences from that of the paper.

* The type checker is not yet implmented. However, Theseus will check
  strict coverage and complain if there are clauses that are
  overlapping or non-exhaustve. It does not check types of the
  variables or that they are used exaclty once. The strict coverage of
  function call contexts is also not checked.

* We need to specify a keyword called `iso` when defining maps. 

* The import and file load semantics currently creates one flat list
  of definitions. This can violate the expected static scope of top
  level definitions.

* The paper allows for dual definitions as shown below. The current
  implementation does not handle simulataneous definition of the
  inverse function `:: sub1`

```
  add1 :: Num <-> Num :: sub1
  | ... 
```
   
* All constructors take only one type or value arugment, similar to
  OCaml constructor definitions. Hence one has to write

```
data List = E | Cons (Num * List)

f :: ...
| ... Cons (n, ls) ...  
```

instead of 
```
data List = E | Cons Num List

f :: ...
| ... (Cons n ls) ...  
```

