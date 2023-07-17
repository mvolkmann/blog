---
eleventyNavigation:
  key: Prolog
layout: topic-layout.njk
---

## Overview

Prolog is a logic-based programming language.
The name is a contraction of "programming in logic".

Prolog first appeared in 1972. It was designed by three computer scientists,
Alain Colmerauer (France), Phillipe Roussel (France), and
Robert Kowalski (USA/Britain).

Prolog uses a declarative syntax rather than a procedural one.
Instead of writing code that describes "what" to do,
the code describes relationships between data values.

Nearly all Prolog code has one of these four purposes:

1. Describe a relationship that is always true (aka holds) ... a fact.

   For example:

```prolog
father(richard, mark).
father(mark, amanda).
father(mark, jeremy).
```

1. Describe a relationship that is conditionally true ... a rule.

   For example, the following rule states that `G` is a grandfather of `C`
   if `G` is the father of `P` AND `P` is either the father or mother of `C`:

   ```prolog
   grandfather(G, C) :=
     father(G, P),
     (father(P, C); mother(P, C)).
   ```

1. Ask whether a specific relationship is true ... a question with no variables.

   For example, `?- grandfather(richard, amanda).` outputs `true`.

1. Ask for values for which a relationship is true ...
   a question with variables.

   For example, `?- grandfather(G, amanda).` sets `G` to `richard`
   Sometimes there are multiple values for which a question holds.
   For example, the question `?- grandfather(richard, G).`
   sets `G` to `amanda` and then `jeremy`.
   Note how a rule can be used to find values for any of its arguments,
   searching in multiple directions.

Questions (aka queries) perform "unification" which basically means
finding values for variables that cause a relationship to hold.
This requires pattern matching search and backtracking.
Unification relies on the properties of {% aTargetBlank
"https://en.wikipedia.org/wiki/Horn_clause", "Horn clauses" %}.
A related term is "ground" which refers to an expression
that contains no uninstantiated variables.

The set of facts and rules supplied to the Prolog engine is called a database.
Prolog is highly optimized to handle searching large databases.

Prolog is a homoiconic language, which means its code can be treated as data.

Prolog has many uses including artificial intelligence,
abstract problem solving, symbolic equation solving, and more.

"IBM Watson is a question-answering computer system
capable of answering questions posed in natural language."
It is partially implemented in Prolog.

## Use Cases

Prolog is used in many kinds of applications including:

- problem solving
- parsing
- natural language processing (NLP)
- specification language

  For example, the Java Virtual Machine (JVM) specification uses Prolog.

- rule-based systems
- automation systems
- robot planning
- natural language understanding
- intelligent database retrieval

## Learning Curve

The primary concepts in Prolog such as facts, rules, and questions
are easy to understand after seeing a few examples.

However, writing programs that are entirely
based on relationships rather than imperative code
makes Prolog very different from other programming languages.
This makes learning Prolog somewhat challenging initially.
For example, to write a program that compiles some source code
into its target representation, define the relationship that describes
when source code is related to the target representation.

Prolog supports a large number of operators and built-in predicates.
These take considerable time to learn and master.

## Resources

- {% aTargetBlank "https://mitpress.mit.edu/9780262691635/the-art-of-prolog/",
  "The Art of Prolog" %} Second Edition book by Sterling and Shapiro

- {% aTargetBlank "https://www.youtube.com/watch?v=dKn-BbS_zQQ",
  "A Brief Introduction to Prolog" %} YouTube video by Erik Schierboom

- {% aTargetBlank "https://link.springer.com/book/10.1007/978-3-642-58274-5",
  "Clause and Effect" %} book by Clocksin

- {% aTargetBlank "https://www.covingtoninnovations.com/mc/plcoding.pdf",
  "Coding guidelines for Prolog" %}

- {% aTargetBlank "http://www.gprolog.org", "GNU Prolog" %}

- {% aTargetBlank "https://www.iso.org/standard/21413.html",
  "ISO Standard for Prolog" %}

- {% aTargetBlank "http://lpn.swi-prolog.org/lpnpage.php?pageid=online",
  "Learn Prolog Now!" %}

- {% aTargetBlank "https://www.metalevel.at/prolog", "The Power of Prolog" %}
  free, online book with accompanying videos by
  {% aTargetBlank "https://www.metalevel.at", "Markus Triska" %}
  from the University of Technology in Austria

- {% aTargetBlank "https://link.springer.com/book/10.1007/978-3-642-55481-0",
  "Programming in Prolog" %} Fifth edition book by Clocksin and Mellish

- {% aTargetBlank "https://www.swi-prolog.org", "SWI-Prolog" %}

- {% aTargetBlank
  "https://www.linuxlinks.com/excellent-free-books-learn-prolog/",
  "13 Excellent Free Books to Learn Prolog" %}

- {% aTargetBlank "https://en.wikipedia.org/wiki/Prolog", "Wikipedia" %}

## Implementations

There are many implementations of the Prolog programming language.
The most popular are described below.

Many Prolog implementations conform to the {% aTargetBlank
"https://www.iso.org/standard/21413.html", "ISO Standard for Prolog" %}.
This defines the minimum requirements for conforming implementations.
Many implementations add features beyond the ISO standard.

The ISO standard does not require implementations to provide a
top level or REPL, but most do.
A top level supports interactive entry and evaluation of Prolog questions.
The output is a new question, often containing semicolons,
that describes the possible solutions.
Some top level implementations also supports interactive debugging.

Several Prolog implementations compile source code to
abstract machine code for the Warren Abstract Machine (WAM).
Searches in WAM use an efficient hash-based approach.

### SWI-Prolog

<img alt="SWI-Prolog logo" style="width: 20%"
  src="/blog/assets/swipl-logo.png?v={{pkg.version}}"
  title="SWI-Prolog logo">

> SWI-Prolog offers a comprehensive free Prolog environment.
> Since its start in 1987, SWI-Prolog development has been
> driven by the needs of real world applications.
> SWI-Prolog is widely used in research and education
> as well as commercial applications."

> Its main author is Jan Wielemaker.
> The name SWI is derived from Sociaal-Wetenschappelijke Informatica
> ("Social Science Informatics"), the former name of the group at
> the University of Amsterdam, where Wielemaker is employed.

{% aTargetBlank "https://www.swi-prolog.org", "SWI-Prolog" %}
is implemented in C (48%) and Prolog (39%).

Documentation of built-in predicates uses **argument mode indicators**
that are documented at
<a href="https://www.swi-prolog.org/pldoc/man?section=preddesc" target="_blank">
Notation of Predicate Descriptions</a>.
Argument mode indicators include `++` (ground), `+` (instantiated),
`-` (output), `--` (unbound), `?` (partially bound),
`:` (meta-argument such as a goal), `@` (will not be instantiated),
and `!` (mutable).

To install the terminal command `swipl` in macOS,
enter `brew install swi-prolog`

To start a SWI-Prolog top level from a terminal, enter `swipl`.

To specify configuration for all top level sessions,
create the file `$HOME/.config/swi-prolog/init.pl`.
For example, this file might set [prolog flags](#prolog-flags).

#### Packs

Packs are add-on libraries.

To see a list of known packages, browse {% aTargetBlank
"https://www.swi-prolog.org/pack/list?p$=$plml",
"Packs (add-ons) for SWI-Prolog" %}.
Alternatively, enter `pack_list(substring)` where
substring is part of one or more pack names.

To install a pack, enter `pack_install(name)`.
This will download the code from `https://www.swi-prolog.org/pack/question`
and install it.

The "reif" pack (reified if) implements the `if_` predicate
which is similar to the `->` operator, but has some advantages.
I could not find good examples of using and could not get it to work.

The {% aTargetBlank "https://www.swi-prolog.org/pack/list?p=gvterm",
"gvterm" %} pack generates a graphviz file from a Prolog term.
This is useful for visualizing the tree structure of a term.

#### Debugging

For information on using the debugger in SWI-Prolog, see {% aTargetBlank
"https://www.swi-prolog.org/pldoc/man?section=debugoverview",
"Overview of the Debugger" %}.

The `trace` predicate enables tracing of the search to find a question solution.

The following code defines fact about my family and a rule about grandfathers.

```prolog
female(amanda).
female(judi).
female(tami).

male(jeremy).
male(mark).
male(richard).

father(richard, mark).
father(mark, amanda).
father(mark, jeremy).

mother(judi, mark).
mother(tami, amanda).
mother(tami, jeremy).

grandfather_of(X, Y) :-
  father(X, P),
  (father(P, Y); mother(P, Y)).
```

To trace the execution of the question `grandfather_of(richard, X).`,
enter `trace.` and then the question.
The screenshot below shows the output.
After each line in the trace, press the spacebar
to advance to the next term to be evaluated.
After a solution is found, press the semicolon key
to begin searching for the next solution.

<img alt="SWI-Prolog trace" style="width: 60%"
  src="/blog/assets/swi-prolog-trace.png?v={{pkg.version}}"
  title="SWI-Prolog trace">

The `trace` predicate enables both the trace and debug modes.
To disable these, enter `notrace.` and `nodebug.`

#### Executables

In SWI-Prolog, to compile a Prolog source file to an executable,
enter `swipl -o {exe-name} -c {source-name}.pl`.
For example, `swipl -o sukuko -c suduko.pl`.
Running this executable with `./suduko` starts a top level session
and loads the compiled facts and rules.

### GNU Prolog

<img alt="GNU Prolog logo" style="width: 20%"
  src="/blog/assets/gnu-prolog-logo.png?v={{pkg.version}}"
  title="GNU Prolog logo">

{% aTargetBlank "http://www.gprolog.org", "GNU Prolog" %}.
is implemented in C (84%) and Prolog (15%).

To install the terminal command `gprolog` in macOS,
enter `brew install gnu-prolog`.

To start a GNU Prolog top level from a terminal, enter `gprolog`.

### Scryer Prolog

<img alt="Scryer Prolog logo" style="width: 20%"
  src="/blog/assets/scryer-prolog-logo.png?v={{pkg.version}}"
  title="Scryer Prolog logo">

{% aTargetBlank "https://www.scryer.pl", "Scryer Prolog" %}
"is a free software ISO Prolog system intended to be an
industrial strength production environment and
a testbed for bleeding edge research in logic and constraint programming."

Scryer Prolog is implemented in Rust (64%) and Prolog (36%).

To install, enter the following commands:

```bash
git clone https://github.com/mthom/scryer-prolog
cd scryer-prolog
curl https://sh.rustup.rs -sSf | sh # if cargo is not yet installed
cargo build --release
```

This creates the executable file `target/release/scryer-prolog`.
Define an alias like `scryerp` to make this easier to run.

To start a Scryer Prolog top level from a terminal, enter `scryerp`.

To specify configuration for all top level sessions,
create the file `$HOME/.scryerrc`.

### Ciao Prolog

{% aTargetBlank "https://ciao-lang.org", "Ciao" %}
"is a modern Prolog implementation that builds up from a
logic-based simple kernel designed to be portable, extensible, and modular."

Ciao is implemented in Prolog (72%) and C (23%).

To install:

1. Install emacs. In macOS, this can be done by entering `brew install emacs`
1. Enter `curl https://ciao-lang.org/boot -sSfL | sh`
1. Create an alias to the executable. For example:

   ```bash
   alias ciao="$HOME/.ciaoroot/v1.22.0-m5/build/bin/ciao"
   ```

To start a Ciao top level from a terminal, enter `ciao`.

### Other Implementations

- {% aTargetBlank "http://tau-prolog.org", "Tau" %}
  implemented in JavaScript (95%) and Prolog (5%)
- {% aTargetBlank "https://github.com/trealla-prolog/trealla", "Trealla" %}
  implemented in C (82%) and Prolog (18%)

## Online Top Level

To enter and run Prolog code in a web browser, browse
{% aTargetBlank "https://swish.swi-prolog.org", "SWISH" %}.

Enter facts and rules in the left pane.
Enter a question in the lower-right pane.
Press the "Run!" button or ctrl-return to execute the question.

## Exiting

To exit from any Prolog interpreter, enter `halt.` or press ctrl-d.

## Terminology

| Term              | Meaning                                                       |
| ----------------- | ------------------------------------------------------------- |
| term              | the only datatype; has four subtypes listed below             |
| - number          | integer or floating point                                     |
| - atom            | identifier that represents a specific thing                   |
| - variable        | represents a value to be determined                           |
| - compound term   | specific combination of terms; more detail below              |
| structure         | another name for a compound term                              |
| fact              | description of something that is true                         |
| rule              | relationship involving one or more unknown things (variables) |
| predicate         | collection of clauses with the same functor                   |
| clause            | a fact or rule                                                |
| question          | asks if a term is true or asks for satisfying variable values |
| database          | collection of predicates                                      |
| functor name      | name of a predicate                                           |
| arity             | number of predicate arguments                                 |
| functor           | function name and its arity; written with a slash between     |
| goal              | compound term in a rule body or question                      |
| list notation     | comma-separated terms inside square brackets; ex. `[a, B, 7]` |
| operator notation | terms separated by operators; ex. `Y = m*X + b`               |
| function notation | operators are written as function calls; ex. `*(3, +(1, 2))`  |
| unification       | process of searching for variable values that satisfy a goal  |
| choice point      | represents a choice in the search for a solution              |
| conjunction       | and'ing terms with comma operator                             |
| disjunction       | or'ing terms with semicolon operator                          |

A string is treated as a list of atoms where each atom represents a character.
This makes it a compound term.

Every compound term can be written as
a functor name followed by an argument list.
Each argument can be an atom, a variable, or another compound term.
All compound terms, including nested ones,
can be represented by a tree structure.

A rule can be thought of as a special kind of fact
that depends on a set of other facts

Numbers can include underscores for readability.
For example, `1_234_567` makes it more clear
that this number is greater than one million.

The functor for `foo(bar, baz)` is written as `foo/2`.

For more, see <a href="https://swi-prolog.org/pldoc/man?section=glossary"
target="_blank">Glossary of Terms</a>.

## Syntax

Prolog programs are composed of facts, rules, and questions.
All of these are terminated by a period.

### Facts

A fact states that some relationship is always true.

A fact is written as a functor (atom) followed by
an argument list that is surrounded by parentheses.
The argument list contains only atoms, not variables.

For example:

```prolog
runner(mark). % says mark is a runner
likes(mark, prolog) % says mark likes prolog
```

These are facts that say comet is a whippet and spots is a cheetah:

```prolog
whippet(comet).
cheetah(spots).
```

Facts provide a convenient way to define constants
that are used in several places within a program.
For example, a "size" constant can be defined and used as follows:

```prolog
size(6).

some_rule(Arg1, Arg2) :-
  size(Size),
  format('The size is ~w.~n', [Size]).
```

### Rules

Rules are written as a head and a body separated by
the "if" operator `:-` and terminated by a period.
They states that the head holds if all the goals in the body hold.
Rules do not return a value like functions in other programming languages,
but they can set the values of variables provided as arguments.

The head is a functor name followed by
an argument list that is surrounded by parentheses.
The head syntax is similar to that of a fact,
but its argument list can contain variables.

The body is a comma (means "and") or semicolon (means "or")
separated list of goals.
Typically each goal is written on a separated line and indented,
but this is not required.

The following rules state that
something is fast if it is a cheetah or a whippet.

```prolog
fast(X) :- cheetah(X).
fast(X) :- whippet(X).

% The previous two lines can be replaced with the following
% where the `;` operator or's its left and right values.
% fast(X) :- cheetah(X); whippet(X).
```

A fact is a degenerate case of a rule whose body only contains `true`.
For example, the following are equivalent:

```prolog
likes(mark, ice-cream).
likes(mark, ice-cream) :- true.
```

The following rules define what it means
for two people to be siblings or sisters.

```prolog
% This rule states that siblings must have the same father and the same mother.
% It has four goals.
sibling(X, Y) :-
  father(F, X),
  father(F, Y),
  mother(M, X),
  mother(M, Y).

sister_of(X, Y) :-
  \+ X = Y, % can't be sister of self
  female(X),
  sibling(X, Y).
```

Often functor names describe the arguments
whose relationship is being described, separated by underscores.
For example, the rule head `parent_child(P, C)` makes it clear that
the first argument represents a parent
and the second argument represents a child.
Some might also use the functor name `parent_of` or simply `parent`.

Good functor names are general and describe relationships
without implying a direction. Bad function names do neither.
For example, functor names containing words like "count", "drop", or "find"
imply that the clause is meant to be used in a specific direction.

The `is` operator evaluates its right-hand side as an arithmetic expression
and assign the result to its left-hand side.
It should only be used when the right-hand side is an arithmetic expression.
For example:

```prolog
area(circle, Radius, X) :- X is pi * Radius^2.
area(square, Side, X) :- X is Side^2.
area(rectangle, Width, Height, X) :- X is Width * Height.

?- area(circle, 2, X).
X = 12.566370614359172.
```

TODO: See the version of the area rule that uses clpr in geometry.pl.

Rules can only set the value of each argument and variable one time.
Once set, they cannot be modified.

The last goal in a rule can be the built-in predicate `true` to always succeed.
It can also be `false` or `fail` to always fail.
None of these approaches are commonly used.

Rules can be recursive.
The following rules compute the factorial of an integer:

```prolog
factorial(0, 1) :- !.

factorial(N, F) :-
  N1 is N - 1,
  factorial(N1, F1),
  F is N * F1.

?- factorial(5, F).
% output is F = 120.
```

Also see the `sum` example in the "Lists" section.

When a rule is not working as expected,
it may be too general (true for invalid values)
or too specific (false for valid values).
A common way to fix a rule that is too general is to add more goals.
A common way to fix a rule that is too specific
is to add more versions of the rule.

## Questions

Questions test whether a term is true or
they find variable values for which the term is true.
Questions are written after the characters `?-`.

For example:

```prolog
% This is a question that asks whether comet is fast.
?- fast(comet). % true

% This is a question that asks for something that is fast.
?- fast(X). % comet
```

Suppose the following facts are loaded:

```prolog
likes(mark, tacos).
likes(mark, books).
likes(mark, running).
```

The question `likes(X, running)` will find "mark".

The question `likes(mark, X)` will find
"tacos", "books", and "running" that in that order.
When a question has multiple matches, as in this example,
the interpreter will wait for further input.

To search for the next match, press the semicolon key.
SWI-Prolog also supports pressing the n, r, space, or tab keys to do this.
Scryer Prolog supports pressing a to output all remaining solutions.

To stop searching for matches before the last one is found,
press the return key.
SWI-Prolog also supports pressing the c or a keys to do this.

After the last match is found, a prompt for the next question will appear.

Variables can be used for any argument of a predicate.
The unification process will find each set of variable values
that cause the predicate to succeed, one set at a time.

### Conjunctions

The comma operator, read as "and", is used in rules or questions
where multiple goals must be met.
For example:

```prolog
% This rule says that mark likes females that like cycling.
likes(mark, x) := female(X), likes(X, cycling).

% This question asks if mark loves tami AND tami loves mark.
?- loves(mark, tami), loves(tami, mark)`

% This question searches for things that both mark and tami love.
% X stands for the same value in both goals.
?- loves(mark, X), loves(tami, X)
```

Variables retain their values across question conjunctions,
but their values are lost when a question ends.
This is a feature of {% aTargetBlank
"https://en.wikipedia.org/wiki/Static_single-assignment_form",
"static single-assignment" %} (SSA) that is used by Prolog.
In SSA,
"each variable to be assigned exactly once and defined before it is used."
and "every definition (Prolog fact, rule, or question) gets its own version."

For example:

```prolog
X is 6, Y is X * 2, Z is Y / 3.
% output is X = 6, Y = 12, Z = 4.
% Subsequent questions cannot access these values.
```

### Disjunctions

The semicolon operator, read as "or", is used in rules or questions
where one of a set of goals must be met.

Earlier we saw a rule that stated something is fast
if it is a cheetah or whippet.
The following way of writing the rule uses disjunction:

```prolog
% fast(X) :- cheetah(X); whippet(X).
```

## Typical Flow

To start a Prolog top level, enter an implementation-specific command
such as `swipl` or `gprolog`.

To evaluate a question in the top level,
enter the question terminated with a period.
If the question does not contain any variables
then `true` or `false` will be output.
If the question does contain variables, a lazy search will be performed
to find the first set of values that satisfy the question will be output.
To see the next set, press the semicolon key.
A period will be output after the last set is found.

To evaluate arithmetic operators that result in a numeric value,
assign the expression to a variable using the `is` operator.
For example, entering `X is 1 + 2.` will output `X = 3.`

The typical steps to run a Prolog program are:

1. Add facts and rules to a Prolog source file that has an extension of `.pl`
1. Load Prolog source files into the Prolog app.
1. Enter questions in the Prolog app.

Unfortunately Prolog and Perl use the same file extension
for their source files.

To load a `.pl` file in the top level,
enter `['file-path'].` or `consult('file-path').`
If the file is in the current directory,
its name can be used without the `.pl` extension
and quotes are not needed inside the square brackets.
For example, to load the file `demo.pl` in the current directory,
enter `[demo].`

Alternatively, pass a source file to the top level when starting it.
For example, `swipl demo.pl`

In SWI-Prolog, after modifying source files that have already been loaded,
enter `make.` to reload all of them.

To enter new facts and rules in a running session:

- Enter `[user].` to open stdin as a pseudo file.
- Enter facts and rules.
- Press ctrl-d to close and load the pseudo file.

It seems this can replace existing facts rather than add to them.

## Common Errors

When an attempt to run a Prolog program fails,
it is often for one of these reasons.

- A token intended to be a variable starts with
  a lowercase letter instead of uppercase.
- A rule has `:=` between its head and body instead of `:-`.
- A goal in an intended conjunction ends with a period instead of a comma.
  Look for the error message "Full stop in clause-body?".
- The last argument to the `format` predicate is a single value
  instead of a list of values to be inserted in the format string.
- An attempt assign an arithmetic expression to a variable
  uses the `=` operator instead of the `is` operator
  resulting in the expression not being evaluated.

## Compiler Directives

Compiler directives in source files begin with `:-`.
They have several purposed including:

- setting compiler flags
- including other source files
- including a module or library
- evaluate goals when the source file is loaded

### Compiler Flags

Prolog flags configure the operation of a Prolog compiler.

To get the value of a prolog flag, use the `current_prolog_flag` predicate.
For example, `current_prolog_flag(double_quotes, F)` sets `F` to the value.

To set the value of the prolog flag, use the directive `set_prolog_flag`.
For example:

```prolog
:- set_prolog_flag(double_quotes, chars).
```

For details on the `double_quotes` flag, see the [Strings](#strings) section.

All the prolog flags supported by SWI-Prolog are documented at
<a href="https://www.swi-prolog.org/pldoc/man?section=flags" target="_blank">
Environment Control (Prolog flags)</a>.

### Including Source Files

One Prolog source file can textually include another
using the `include/1` directive.
For example:

```prolog
:- include(util). % includes the source file util.pl
```

### Including a Module/Library

To include a library (ex. clpfd), include a line like the following:

```prolog
:- use_module(library(clpfd)).
```

### Evaluating Goals on Load

To evaluate goals when a source file is loaded, precede them with `:-`.
For example:

```prolog
:- writeln('Hello, World!').
```

A source file can contain any number of these.

Alternatively, to specify a conjunction of several goals to be evaluated,
use `initialization`. For example:

```prolog
:- initialization
  goal1,
  goal2,
  goal3
```

## Tree Representation

Every Prolog term can be represented as a tree
where parent nodes are functors and arguments are children.
For example, `a(b, c(d, e), f)` can be represented as the following tree:

- a/3
  - b
  - c/2
    - d
    - e
  - f

## Data Structures

ISO Prolog supports three data structures, structures, lists, and pairs.
Some Prolog implementations, such as SWI-Prolog, also support dicts.

SWI-Prolog also supports dicts (a.k.a dictionaries).
See {% aTargetBlank "https://eu.swi-prolog.org/pldoc/man?section=bidicts",
"Dicts: structures with named arguments" %}.

### Structures

Structures (a.k.a. compound terms) are a bit like
structs in some other programming languages.
They group related values.

For example, `dog(whippet, comet)` is a structure
that describes a dog whose breed is "whippet" and whose name is "comet".
In this example, one could think of `dog` as the type,
and whippet and comet are the components of the structure.
Developers determine the meaning and order of the components.

The syntax for a structure is the same as the syntax for a fact.

Structures can be used in facts and rules.
Components of structures can be atoms or variables.
For example:

```prolog
owns(tami, pet(dog, comet)).
owns(amanda, pet(dog, maisey)).
owns(amanda, pet(dog, oscar)).
owns(jeremy, pet(dog, ramsay)).

% This takes a pet structure and destructures its kind and name.
print_pet(pet(Kind, Name)) :-
  format('~w is a ~w.~n', [Name, Kind]).

main :-
  owns(tami, A),
  format('pet = ~w~n', A), % pet(dog,comet)

  print_pet(A), % comet is a dog.

  owns(tami, pet(dog, B)),
  format('name = ~w~n', B), % comet

  owns(tami, pet(C, D)),
  format('kind = ~w, name = ~w~n', [C, D]). % dog and comet

:- main.
```

Structures can be nested.
For example:

```prolog
person(mark, address('123 Some Street', 'Somewhere', 'MO', 12345)).

main :-
  person(mark, address(S, _, _, _)),
  format('street = ~w~n', S). % 123 Some Street

:- main.
```

### Lists

Lists are commonly used to hold collections of elements when
there can be any number of elements (even zero), their order matters,
and all the elements have the same type (ex. all numbers).

A list can be written as a comma-separated set of terms
surrounded by square brackets.
For example, `[red, green, blue]` is a list of atoms
and `[R, G, B]` is a list of variables that can be
unified with any list containing exactly three elements.

An empty list is written as the atom `[]` which is called "nil".

There are other ways to construct a list.

The dot function (`./2`) is the list constructor.
It is passed the head and the tail of the list to be constructed.
For example, `.(H, T)` creates a list
where `H` is a single element that is the head
and `T` is a list of elements in the tail.
Often variables that represent lists end in "s" (ex. `.(E, Es)`).

SWI-Prolog uses a different function name for the list constructor.
For example:

```prolog
L = '[|]'(a, [b, c]).
% output is L = [a, b, c].
```

The head-tail separator `|` in `[H|T]` creates a list
where H is a single element that is the head
and `T` is a list of elements in the tail.
For example, `[a | []]` is a list containing only `a`
and `[a | [b, c]]` is equivalent to `[a, b, c]`.
Use of the `|` operator can be nested.
For example, `[a | [b | [c]]]` is also equivalent to `[a, b, c]`.

The following are all equivalent ways to write the same list:

```prolog
[red, green, blue] % list notation

.(red, .(green, .(blue, []))) % function notation
% In SWI-Prolog, the dots must be replaced by `'[|]'`.

[red | [green | [blue]]] % head-tail separator notation
% Specifying a tail of [] for [blue] is optional.
```

Lists can be nested. For example:

```prolog
[a, [b, c], d, [e, [f, g, h]]]
```

A "partial list" is a term that could become a list.
For example:

```prolog
[a | T] % will be a list if T is a list
[a | b] % not a list because b is not a list
[A, B, C | T] % will be a list of at least three elements if T is a list
```

To get the sum of numbers in a list:

```prolog
L = [1, 2, 3], sum_list(L, Sum).
% output is Sum = 6.
```

If the `sum_list` predicate didn't exist,
it could be implemented with a recursive rule as follows:

```prolog
sum_list(List, Sum) :-
  % If the list is empty then the sum is zero.
  List = [] -> Sum = 0;

  % Otherwise ...
  List =
    % Get the first number and a list of the remaining numbers.
    [Head|Tail],
    % Compute the sum of the remaining numbers.
    sum_list(Tail, TailSum),
    % The result is the first number plus that sum.
    % Note the use of the "is" keyword to assign the
    % result of an arithmetic expression to the Sum argument.
    Sum is TailSum + Head.

?- sum_list([1, 2, 3], X).
% output is X = 6.
```

Anonymous variables (`_`) can be used to destructure values from a list.
For example, the following gets the first and third values.
The `| _` syntax at the end of the list on the left side
indicates that we do not care about values in the tail of the list
which includes all values after the third.

```prolog
[V1, _, V3 | _] = [9, 8, 7, 6, 5].
% output is
% V1 = 9,
% V3 = 7.
```

The `|` operator can be used to get the head and tail of a list.
For example:

```prolog
% This destructures the list passed in into its head and tail.
print_list_parts([H|T]) :-
  format('head is ~w, tail is ~w', [H, T]).

?- print_list_parts([red, green, blue]).
% output is head is red, tail is [green,blue]
```

The `|` operator can be used in a recursive rule
to iterate over all elements of a list.
Destructuring a list into its head and tail can be done in the argument list.
For example:

```prolog
print_elements([]).

print_elements([H|T]) :=
  writeln(H),
  print_elements(T).

?- print_elements([red, green, blue]).
% output is
% red
% green
% blue
```

The `subtract` predicate can be used to create a list
that is derived by removing a given element from another list.

The following code shows how this can be implemented.
It serves as one more example of using the `|` operator.

```prolog
% Removing anything from an empty list matches an empty list.
list_without([], _, []).

% H is the head of the 1st argument list and T is its tail.
% E is the element to removed.
% L is a list containing all elements of the first list
% with all occurrences of E removed.
list_without([H|T], E, L) :-
  % If H matches E ...
  H == E ->
    % Then the result list L is the same as T with E removed.
    list_without(T, E, L);
    % Otherwise the result list L is a new list with head H
    % and tail is the same as T with E removed.
    list_without(T, E, L2), L = [H|L2].
```

The `maplist` predicate can be used to create a list
that is derived by applying a given predicate to each element of another list.
Predicates like this that take another predicate as an argument
are called "higher-order predicates".
For example:

```prolog
double(A, B) :-
  var(A) -> A is B / 2; B is A * 2.

:- maplist(double, [1, 2, 3], L), writeln(L).
% output is [2, 4, 6]
```

The `reverse` function creates a new list containing
all the values in a given list in reverse order.
For example:

```prolog
?- reverse([1, 2, 3], X).
% output is X = [3, 2, 1].
```

To get the length of a list:

```prolog
L = [a, b, c], length(L, X).
% output is X = 3.
```

To determine if two lists have the same length:

```prolog
L1 = [a, b, c], L2 = [9, 8, 7], same_length(L1, L2).
% doesn't output true, but also doesn't fail
```

To test whether a value is a member of a list:

```prolog
L = [3, 7, 9], member(7, L).
% doesn't output true, but also doesn't fail

L = [3, 7, 9], member(4, L).
% output is false
```

To test whether all elements in a list are a given value:

```prolog
maplist(=(V), L).
```

To test whether a list begins with a given sub-list:

```prolog
L = [a, b, c, d], prefix([a, b], L).
% doesn't output true, but also doesn't fail
```

To get the first element of a list:

```prolog
L = [a, b, c, d], [H|_] = L.
% output is H = a.
```

To test whether a list ends with a given sub-list:

```prolog
L = [a, b, c, d], last(L, d).
% doesn't output true, but also doesn't fail
```

To get the last element of a list:

```prolog
L = [a, b, c, d], last(L, E).
% output is E = d.
```

The `member` function can also be used to iterate over the values in a list.
or example, `member(X, [3, 7, 9])` will set `X`
to each value in the list one at a time.

To get the list element at a given index:

```prolog
L = [a, b, c], nth0(1, L, E). % zero-based index
% output is E = b.

L = [a, b, c], nth1(2, L, E). % one-based index
% output is E = b.
```

To get the index of a given element in a list:

```prolog
L = [a, b, c], nth0(Index, L, b). % zero-based index
% output is Index = 1.

L = [a, b, c], nth1(Index, L, b). % one-based index
% output is Index = 2.
```

To create a copy of a list, or any term, including nested lists:

```prolog
copy_term(ListIn, ListOut)
```

To create a new list that results from adding a value
to the beginning of an existing list:

```prolog
L1 = [b, c, d], L2 = [a | L1].
% output is L2 = [a, b, c, d].
```

To create a new list that results from adding a value
to the end of an existing list:

```prolog
L1 = [a, b, c], append(L1, [d], L2).
% output is L2 = [a, b, c, d].
```

To create a new list that results from inserting a value
at a given zero-based index in an existing list:

```prolog
% Inserts x after 2nd element.
L1 = [a, b, c], nth0(2, L2, x, L1).
% output is L2 = [a, b, x, c].
```

To create a new list that results from
removing only the first occurrence of a given value:

```prolog
selectchk(b, [a, b, c, b], L).
% output is L = [a, c, b].
```

The `select/3` predicate is similar to `selectchk/3`,
but it iterates through every possible removal.

To create a new list that results from
replacing only the first occurrence of a given value:

```prolog
selectchk(b, [a, b, c, b], x, L).
% output is L = [a, x, c, b].
```

The `select/4` predicate is similar to `selectchk/4`,
but it iterates through every possible replacement.

To create a new list that results from
removing every occurrence of given values:

```prolog
subtract([a, b, c, b], [b], L). % could remove more than just b elements
% output is L = [a, c].
```

To create a new list that results from either retaining or removing
all elements from an existing list that satisfy or fail to satisfy a goal:

```prolog
even(N) :- mod(N, 2) =:= 0.
odd(N) :- mod(N, 2) =:= 1.

include(even, [1, 2, 3, 4], L).
% output is L = [2, 4].

exclude(odd, [1, 2, 3, 4], L).
% output is L = [2, 4].
```

To get all permutations of a list:

```prolog
L = [a, b, c], permutation(L, Ps).
% output is
% Ps = [1, 2, 3] ;
% Ps = [1, 3, 2] ;
% Ps = [2, 1, 3] ;
% Ps = [2, 3, 1] ;
% Ps = [3, 1, 2] ;
% Ps = [3, 2, 1] ;
false.
```

To flatten nested lists:

```prolog
L1 = [[a, b], c, [d, [e, f]]], flatten(L1, L2).
% output is [a, b, c, d, e, f].
```

To perform run length encoding:

```prolog
L = [dog, dog, cat, dog, dog, dog, rabbit, rabbit], clumped(L, C).
% output is C = [dog-2, cat-1, dog-3, rabbit-2].
```

To get the smallest or largest value in a list:

```prolog
L = [3, 9, 2, 4], min_list(L, Min).
% output is Min = 2.

L = [c, a, d, b], min_member(Min, L).
% output is Min = a.

L = [3, 9, 2, 4], max_list(L, Max).
% output is Max = 9.

L = [c, a, d, b], max_member(Max, L).
% output is Max = d.

younger(P1, P2) :-
  person(_, A1) = P1,
  person(_, A2) = P2,
  A1 < A2.

?- P1 = person(ann, 35),
   P2 = person(bob, 50),
   P3 = person(carl, 19),
   People = [P1, P2, P3],

   min_member(younger, Py, People),
   person(N1, A1) = Py,
   format('youngest is ~w at age ~w~n', [N1, A1]),
   % output is youngest is carl at age 19

   max_member(younger, Po, People),
   person(N2, A2) = Po,
   format('oldest is ~w at age ~w~n', [N2, A2]).
   % output is oldest is bob at age 50
```

To create a list containing a range of sequential integers:

```prolog
numlist(3, 7, L).
% output is L = [3, 4, 5, 6, 7].
```

To determine if all the elements in a list are unique:

```prolog
L = [b, a, c], is_set(L).
% doesn't output true, but also doesn't fail
L = [b, a, b], is_set(L).
% output is false.
```

To remove duplicate elements from a list:

```prolog
L = [b, a, a, b, c, d, c], list_to_set(L, S).
% output is S = [b, a, c, d].
```

To find the intersection of two lists:

```prolog
L1 = [a, b, c], L2 = [c, b, d], intersection(L1, L2, L3).
% output is L3 = [b, c].
```

To find the union of two lists:

```prolog
L1 = [a, b, c], L2 = [c, b, d], union(L1, L2, L3).
% output is L3 = [a, c, b, d].
```

To sort a list:

```prolog
L = [c, b, d, a], sort(L, S).
% output is [a, b, c, d].

age_compare(>, person(_, A1), person(_, A2)) :- A1 > A2.
age_compare(<, person(_, A1), person(_, A2)) :- A1 < A2.
age_compare(=, person(_, A1), person(_, A2)) :- A1 = A2.

?- P1 = person(ann, 35),
   P2 = person(bob, 50),
   P3 = person(carl, 19),
   People = [P1, P2, P3],

   predsort(age_compare, People, Sorted),
   writeln(Sorted).
   % output is [person(carl,19),person(ann,35),person(bob,50)]
```

To determine if one list contains a subset of another:

```prolog
L = [c, d, b, a], subset([b, c], L).
% doesn't output true, but also doesn't fail

L = [c, d, b, a], subset([b, e], L).
% outputs false
```

There are no built-in predicates that determine if
every or some element of a list satisfies a given predicate.
Those can be implemented as follows:

```prolog
% This succeeds if every element in List satisfies Predicate
% and fails otherwise.
every(Predicate, List) :- maplist(Predicate, List).

% This succeeds if at least one element in List satisfies Predicate
% and fails otherwise.
some(_, []) :- fail.
some(Predicate, [H|T]) :-
  call(Predicate, H), !; some(Predicate, T).
```

The `every` and `some` predicates defined above can be used as follows:

```prolog
every(clpfd:even, [2, 6, 8]) % succeeds
every(clpfd:even, [2, 5, 8]) % fails
some(clpfd:even, [1, 6, 9]) % succeeds
some(clpfd:even, [1, 5, 9]) % fails
```

For implementations of map, filter, and reduce, see {% aTargetBlank
"https://pbrown.me/blog/functional-prolog-map-filter-and-reduce/",
"Functional Prolog" %}.

#### Appending

The built-in predicate `append` can
create a new list by appending two existing lists.
For example:

```prolog
?- append([1, 2, 3], [4, 5], X).
% output is X = [1, 2, 3, 4, 5]
```

The `append` predicate can also create a new list by appending multiple lists.
For example:

```prolog
L1 = [a, b], L2 = [c, d, e], L3 = [f], append([L1, L2, L3], L4).
% output is L4 = [a, b, c, d, e, f].
```

If `append` were not built-in, it could be implemented as follows:

```prolog
% Appending an empty list to any list gives the second list.
append([], L, L).

% Appending two lists is the same as appending
% the head of the first list (H) to the result of appending
% the tail of the first list (L1) to the second list (L2).
append([H|L1], L2, [H|L3]) :- append(L1, L2, L3).
```

Here are several examples of how `append` can be used:

```prolog
% Is the result of appending two lists a given result list?
?- append([1, 2], [3, 4], [1, 2, 3, 4]).
true.

% What is the result of appending two lists?
?- append([1, 2], [3, 4], X).
X = [1, 2, 3, 4].

% What list must be appended to a given list to obtain a given result?
?- append([1, 2], X, [1, 2, 3, 4]).
X = [3, 4].

% What list must be prepended to a given list to obtain a given result?
?- append(X, [3, 4], [1, 2, 3, 4]).
X = [1, 2]

% What lists can be appended to obtain a given result?
?- append(X, Y, [1, 2, 3, 4]).
X = [],
Y = [1, 2, 3, 4] ;
X = [1],
Y = [2, 3, 4] ;
X = [1, 2],
Y = [3, 4] ;
X = [1, 2, 3],
Y = [4] ;
X = [1, 2, 3, 4],
Y = [] ;
```

### Pairs

A Prolog "pair" is a key and a value.
There are two ways to write a pair, `k-v` or `-(k, v)`.

Searches for specific pairs in a list of pairs are sequential.
For a more efficient key lookup see the "Dicts" section.

For details on predicates that operate on pairs, see {% aTargetBlank
"https://eu.swi-prolog.org/pldoc/man?section=pairs",
"library(pairs): Operations on key-value lists" %}.

To sort a list of pairs on their keys:

```prolog
?- keysort([c-cow, b-bear, a-apple], Ps).
% output is Ps = [a-apple, b-bear, c-cow].
```

To get the keys from a list of pairs:

```prolog
?- pairs_keys([c-cow, b-bear, a-apple], Ks).
% output is Ks = [c, b, a].
```

To get the values from a list of pairs:

```prolog
?- pairs_values([c-cow, b-bear, a-apple], Vs).
% output is Vs = [cow, bear, apple].
```

To get both the keys and the values from a list of pairs:

```prolog
?- pairs_keys_values([c-cow, b-bear, a-apple], Ks, Vs).
% output is Ks = [c, b, a], Vs = [cow, bear, apple].
```

When a list contains pairs with duplicate keys is sorted on those keys,
we can get a new list where the keys are unique values
and their values are lists of values.
For example:

```prolog
%- group_pairs_by_key([a-apple, a-apricot, b-banana, b-blueberry, c-cherry], G).
% output is G = [a-[apple, apricot], b-[banana, blueberry], c-[cherry]].
```

To swap keys and values in a list of pairs AND sort the pairs on their key:

```prolog
?- transpose_pairs([c-cow, b-bear, a-apple], Ts).
% output is Ts = [apple-a, bear-b, cow-c].
```

The `map_list_to_pairs` predicate takes a predicate and a list of list.
It creates a list of pairs where the key of each pair is
the result of passing one of the sub-lists to a predicate
and the associated value is the sub-list.
For example, the following uses the `length` predicate.

```prolog
?- map_list_to_pairs(
     length,
     [[apple, apricot], [banana, blueberry], [cherry]],
     Ps).
% output is Ps = [2-[apple, apricot], 2-[banana, blueberry], 1-[cherry]].
```

The following code implements rules to determine if
a queen on a chess board can attach another piece.
Note that:

- Each board position is represented by a row-column pair.
- The arguments destructure the keys and values
  of pairs that are passed in.
- Underscore anonymous variables are used for values that do not matter.
- The cut operator `!` stops searching after the first solution is found.

```prolog
queen_can_attack(R-_, R-_) :- !. % same row
queen_can_attack(_-C, _-C) :- !. % same column
queen_can_attack(R1-C1, R2-C2) :- % same diagonal
  abs(R1 - R2) =:= abs(C1 - C2).
```

### Dicts

A dictionary, or dict for short, is a hash map.
To create a dict, specify a tag followed by an open curly brace,
key/value pairs where there is a colon between each key and value,
and the pairs are separated by commas, and a closing curly brace.
The tag can optionally begin with a module name and a colon.
Then it must specify an atom or variable, which can be `_`.

Values in dicts can be other dicts.

For example:

```prolog
report(P) :-
  format('Hello, ~w ~w!~n', [P.first, P.last]),
  format('I see you are ~w years old~n.', P.age),
  format('Your zip is ~w.~n', P.address.zip).

?- P = person{
  first: 'Mark',
  last: 'Volkmann',
  age: 62,
  address: _{
    street: '123 Some Street',
    city: 'Somewhere',
    state: 'MO',
    zip: 12345
  }
},
report(P).
% The output is:
% Hello, Mark Volkmann!
% I see you are 62 years old.
% Your zip is 12345.
```

To get the value of a key in dict:

```prolog
Value = MyDict.get(key) % returns false if key is missing?
Value = MyDict.get(key, defaultValue) % uses default value if key is missing
```

To use a default value when a kys is missing:

```prolog
MyDict = demo{a: 1, b: 2},
Key = b,
Value = MyDict.get(Key, 0),
format('key ~w = ~w~n', [Key, Value]).
```

To test whether a key exists in a dict:

```prolog
MyDict = demo{a: 1, b: 2},
Key = b,
(Value = MyDict.get(Key) ->
    format('key ~w = ~w~n', [Key, Value]);
    format('key ~w not found', Key)
).
```

TODO: Add more detail on working with dicts.

### Linked Lists

Dictionary (`dict`) instances can have properties that refer to other instances.
This can be used to implement a linked list.
The following code demonstrates how to iterate over such instances:

```prolog
% This prints all the values found in a linked list.
print_list(nil) :- !.
print_list(Node) :-
  writeln(Node.value),
  print_list(Node.next).

% This relates a node in a linked list to
% a list of values found in all reachable nodes.
linked_list(nil, []).
linked_list(Node, L) :-
  linked_list(Node.next, L2),
  % This appends in the order encounter.
  % append([Node.value], L2, L).
  % This appends in reverse order.
  append(L2, [Node.value], L).

:- initialization
  N1 = node{value: 'alpha', next: nil},
  N2 = node{value: 'beta', next: N1},
  N3 = node{value: 'gamma', next: N2},

  print_list(N3), % prints gamma then beta then alpha

  linked_list(N3, L), % [alpha, beta, gamma]

  % This creates a string containing joined values from a list.
  atomics_to_string(L, ',', S),

  writeln(S), % alpha,beta,gamma
  halt.
```

## Type Checking

ISO Prolog provides many built-in predicates that can be used
to assert the type of arguments.
These are not supported in Scryer Prolog.
Instead see {% aTargetBlank "https://www.scryer.pl/si.html", "Module si" %}.

| Predicate                             | Meaning                                                       |
| ------------------------------------- | ------------------------------------------------------------- |
| `var(V)`                              | V is a variable                                               |
| `nonvar(V)`                           | V is not a variable                                           |
| `number(V)`                           | V is any kind of number                                       |
| `integer(V)`                          | V is an integer                                               |
| `float(V)`                            | V is a floating point number                                  |
| `rational(V)`                         | V is a rational number                                        |
| `rational(V, Numerator, Denominator)` | V is a rational number with given a numerator and denominator |
| `atom(V)`                             | V is an atom                                                  |
| `blob(V)`                             | V is a blob                                                   |
| `string(V)`                           | V is a string                                                 |
| `atomic(V)`                           | V is not a variable or compound term                          |
| `compound(V)`                         | V is a compound term                                          |
| `callable(V)`                         | V is an atom or a compound term                               |
| `ground(V)`                           | V is not a variable or a compound term that holds variables   |
| `cyclic_term(V)`                      | V contains cycles (circular references)                       |
| `acyclic_term(V)`                     | V does not contain cycles (circular references)               |

The following code in the file `type_checking.pl` implements simple rules that
perform type checking of an argument.

```prolog
% The cut operator `!` tells Prolog to stop searching once a solution is found.
demo(V, T) :- integer(V), T = 'integer', !.
demo(V, T) :- float(V), T = 'float', !.
demo(V, T) :- rational(V), T = 'rational', !.
% In SWI-Prolog, only double-quoted strings are considered strings.
demo(V, T) :- string(V), T = 'string', !.
demo(V, T) :- atom(V), T = 'atom', !.

demo2(V, T) :- number(V), T = 'number', !.

demo3(V, T) :- atomic(V), T = 'atomic', !.
demo3(V, T) :- compound(V), T = 'compound', !.

demo4(V, T) :- ground(V) -> T = 'ground'; T = 'nonground', !.
```

The following unit test code in the file `type_checking.pl`
demonstrates using the rules defined above:

```prolog
% To run these tests, enter `swipl type-checking.plt`

:- consult(type_checking).
:- begin_tests(type_checking).

test(integer) :- demo(5, T), T == 'integer'.
test(float) :- demo(1.2, T), T == 'float'.
% See rational_syntax flag to enable 2/3 to be treated as rational.
test(rational) :- demo(2r3, T), T == 'rational'.
test(number) :- demo2(1.2, T), T == 'number'.
test(string) :- demo('hello', T), T == 'string'.
test(atom) :- demo(a, T), T == 'atom'.

test(atomic) :- demo3(true, T), T == 'atomic'.
test(atomic) :- demo3(5, T), T == 'atomic'.
test(atomic) :- demo3(1.2, T), T == 'atomic'.
test(atomic) :- demo3('Hello', T), T == 'atomic'.

test(compound) :- demo3(2 + 3, T), T == 'compound'.
test(compound) :- demo3(a(b), T), T == 'compound'.
test(compound) :- demo3(a-b, T), T == 'compound'.
test(compound) :- demo3([a, b], T), T == 'compound'.

test(ground) :- demo4(5, T), T == 'ground'.
test(ground) :- V = 5, demo4(V, T), T == 'ground'.

% The next line suppresses warning about singleton variable V.
:-style_check(-singleton).
test(ground) :- demo4(V, T), T \== 'ground'.

:- end_tests(type_checking).
:- run_tests.
:- halt.
```

## Dynamic Predicates

By default predicates cannot be added or deleted in a top level session.
To enable this, run a `dynamic` question on a specific predicate.
For example, to enable adding and removing "likes" predicates
that take two arguments:

```prolog
dynamic(likes/2).
```

Once this is done, a predicate of that type can be
added to the beginning with `asserta` or added to the end with `assertz`.
Also, predicates of that type can be removed
with the `retract` and `retractall` functions.

For example, suppose we have the file `likes.pl` containing the following:

```prolog
likes(mark, books).
likes(mark, running).
likes(tami, bikes).
```

A session can do the following:

```prolog
[likes].
dynamic(likes/2).
assertz(likes(mark, reeces)). % adds after existing predicates
retract(likes(mark, books)). % removes
likes(mark, X). % outputs running and reeces

retractall(likes(mark, _)). % removes everything that mark likes
likes(X, Y). % outputs X = tami, Y = bikes.
retractall(likes(_, _)). % removes everything that anybody likes
likes(X, Y). % outputs false.
```

## Input

Input can be read from a stream using the ???.

There are two stream aliases, `user_input` (defaults to stdin)
and `current_input` (defaults to stdin).
The stream associated with `user_input`
can be changed by the `set_prolog_IO` predicate.
The stream associated with `current_input`
can be changed by the `set_input` and `see` predicates.

Additional streams can be opened with the `open` predicate
and closed with the `close` predicate.

The following predicates write to the `current_output` stream
or a specified stream: `read`, `get_byte`, `get_char`, and `get_code`.
For example:

```prolog
greet :-
  write('Enter your name: '),
  read(Name),
  format('Hello, ~w!', [Name]).

greet().
```

Enter a name in single or double quotes followed by a period.
This is an odd requirement for users!
Entering 'Mark'. results in the following output: `Hello, Mark!`.

The `get` function reads a single character
and sets a variable to its integer ASCII value.

To read from a file and write the contents to stdout,
use the `open`, `get_char`, `get_code`, and `close` functions.
For example:

```prolog
processStream(end_of_file, _) :- !. % a "cut" that stops execution

processStream(Char, Stream) :-
  write(Char),
  get_char(Stream, NextChar),
  processStream(NextChar, Stream).

readFile(File) :-
  open(File, read, Stream),
  get_char(Stream, Char),
  processStream(Char, Stream),
  close(Stream).

readFile('demo.txt').
```

To read from a text file in Scryer Prolog:

```prolog
:- use_module(library(pio)). % for phrase_from_file and phrase_to_stream
phrase_from_file(seq(Cs), "input.txt"),
phrase_to_stream(Cs, user_output). % writes to stdout
```

## Output

There are three output stream aliases,
`user_output` (defaults to stdout),
`user_error` (defaults to stderr),
and `current_output` (defaults to stdout).
The streams associated with `user_output` and `user_error`
can be changed by the `set_prolog_IO` predicate.
The stream associated with `current_output`
can be changed by the `set_output` and `tell` predicates.

Additional streams can be opened with the `open` predicate
and closed with the `close` predicate.

The following predicates write to the `current_output` stream
or a specified stream: `write`, `writeln`, `format`,
`put_byte`, `put_char`, `put_code`, and `nl` (writes a newline character).
For example:

```prolog
write('Hello World!'), nl.
write(current_output, 'Hello World!\n'). % same as previous line
writeln('Hello World!'). % same as previous line
writeln(current_output, 'Hello World!'). % same as previous line
```

The {% aTargetBlank "https://www.swi-prolog.org/pldoc/man?predicate=format/2",
"format" %} predicate can also write to the current output stream.
It takes a format string and a list of values
to be substituted into the format string.
(In Scryer Prolog, `use_module(library(format)).`
and use the `format_` predicate.)

The format string can contain the following control sequences
that all begin with a tilde:

- `~d`: decimal
- `~D`: decimal with commas every three digits
- `~e`: floating point in exponential notation
- `~E`: same as `~e` but with a capital E
- `~f`: floating point without exponential notation
- `~i`: ignores corresponding value; no output
- `~Nf`: float value with only N decimal places
- `~n`: single newline character
- `~Nn`: N newline characters
- `~Nr`: integer converted to radix N
  For example, `~2r` outputs a number in binary,
  `~16r` outputs a number in lowercase hexadecimal,
  and `~16R` outputs an integer in uppercase hexadecimal.
- `~s`: literal string
- `~t`: inserts multiple spaces up to the next tab stop;
  used to left, center, or right align what follows
- `~w`: writes value of a variable or atom
- `~N|`: sets a tab stop at column N
- `~N+`: sets a tab stop at N columns past last tab stop
  (A tab stop is assumed at column zero.)

For more control sequences, see the "format" link above.

For example:

```prolog
% This is the equivalent of JavaScript console.log.
format('MyVariable = ~w~n', [MyVariable]).

format('~w likes ~s.', [mark, 'Prolog']).
% outputs "mark likes Prolog."
```

Rules can write to the current output stream.
For example:

````prolog
greet(Name) :- format('Hello, ~w!', [Name]).

greet('Mark')
% outputs "Hello, Mark!"
```

Tab stops can be used to output aligned columns.
For example:

```prolog
print_row(Row) :-
  % The 1st list element is left-aligned.
  % The 2nd list element is center-aligned.
  % The 3rd list element is right-aligned.
  format('~w~t~10+~t~w~t~10+~t~w~10+~n', Row).

:- Rows = [
     ["foo", "bar", "baz"],
     ["foolish", "barking", "bazooka"]
   ],
   maplist(print_row, Rows).
````

The output is:

```text
foo          bar           baz
foolish     barking     bazooka
```

The 3-argument version of `format` can write to any stream, including a string.
For example, the following code sets `S` to a formatted string.

```prolog
Language = 'Prolog',
Assessment = 'fun',
format(string(S), '~w is ~w.~n', [Language, Assessment]).
```

The `put` function writes a single ASCII value to the current output stream.
It is the counterpart to the `get` function.

To write to a file, use the `open`, `write`, and `close` functions.
For example:

```prolog
writeFile(File, Text) :-
  open(File, write, Stream),
  write(Stream, Text), nl,
  close(Stream).

writeFile('demo.txt', 'first line\nsecond line').
```

To write to a text file in Scryer Prolog:

```prolog
:- use_module(library(pio)). % for phrase_to_file
phrase_to_file('This is\na test.', "output.txt").
```

To write to a stream in Scryer Prolog (in this case stdout):

```prolog
:- use_module(library(pio)). % for phrase_to_stream
phrase_to_stream('Hello, World!', user_output). % writes to stdout
```

To create a stream associated with a file, use the `open` predicate,
passing it a file path string, a mode (`read`, `write`, `append`, or `update`),
and a variable to capture the stream. For example:

```prolog
open('demo.txt', write, Stream)
```

It is also possible to capture everything a goal writes to stdout in a string.
For example:

```prolog
my_goal :-
  writeln('line #1'),
  writeln('line #2').

:- initialization
  with_output_to(string(S), my_goal),
  write(S).
```

Another way to write to a string is to use a {% aTargetBlank
"https://www.swi-prolog.org/pldoc/man?section=memory-files", "memory file" %}
which may be specific to SWI-Prolog.
This has the advantage that rules can be written to accept any stream,
allowing them to write to a file or a string.
For example:

```prolog
% Create a handle for a memory file.
new_memory_file(Handle),
% Open a stream to a memory file.
open_memory_file(Handle, write, Stream, [free_on_close(true)]),
% Write to the stream.
writeln(Stream, 'line #1'),
writeln(Stream, 'line #2'),
close(Stream),
% Copy the contents of the stream into a string.
memory_file_to_string(Handle, S),
write(S).
```

## Special Characters

| Characters    | Meaning                     |
| ------------- | --------------------------- |
| `:-`          | if; used to define rules    |
| `,`           | logical and                 |
| `;`           | logical or                  |
| `not`         | logical not                 |
| `?-`          | begins a question           |
| `.`           | terminates all commands     |
| `%`           | begins single-line comment  |
| `/*` and `*/` | delimits multi-line comment |

## Naming Conventions

Atoms are sequences of letters, numbers, and underscores
that begin with a lowercase letter.
They can also be any text enclosed in single quotes (allows spaces).
There are also the following special atoms:
`;`, `!`, `[]`, and `{}`.

Variables are also sequences of letters, numbers, and underscores,
but they begin with an uppercase letter or an underscore.
An underscore by itself represents an anonymous variable.
These can be used as arguments to predicates
when the value of an argument does not matter.

## Operators

Prolog operators can be prefix, infix, or postfix.
Of the built-in operators, most are infix,
a few are prefix, and none are postfix.
Prefix operators are noted below.

Each operator has left, right, or no associativity.

Operators can be used in function form.
For example, infix operators like `+` whose usage s typically
written like `a + b` can instead be written as `+(a, b)`.
As another example, `X is 3 * (1 + 2).` gives the same result (`9`)
as `X is *(3, +(1, 2)).`

The `write_canonical` predicate takes any term
and outputs it in its equivalent function notation.
For example, entering `write_canonical(3 * 1 + 2).`
outputs `*(3,+(1,2))`.

### Number Operators

Prolog supports the following relational operators
for numbers and arithmetic expressions.
When the left and/or right side is an expression (ex. `X * 2`)
it is evaluated before the comparison is performed.

| Operator | Meaning               |
| -------- | --------------------- |
| `=:=`    | equal value           |
| `=\=`    | not equal value       |
| `<`      | less than             |
| `>`      | greater than          |
| `=<`     | less than or equal    |
| `>=`     | greater than or equal |

### Atom and String Operators

Prolog supports the following relational operators
for atoms and strings (also works with numbers):

| Operator | Meaning                              |
| -------- | ------------------------------------ |
| `@<`     | alphabetically less than             |
| `@=<`    | alphabetically less than or equal    |
| `@>`     | alphabetically greater than          |
| `@>=`    | alphabetically greater than or equal |

### Term Operators

Prolog supports the following relational operators
for single and compound terms:

| Operator | Meaning                           |
| -------- | --------------------------------- |
| `==`     | identical terms                   |
| `\==`    | not identical terms               |
| `=@=`    | structurally equivalent terms     |
| `\=@=`   | not structurally equivalent terms |

The `dif/2` predicate is an alternative to the `\==` operator.
For example, `dif(a, b)` is the same as `a \== b`.

The odd syntax for "equal" and "not equal"
was chosen because `=` is used for unification.

The odd syntax for "less than or equal" was
chosen so it doesn't look like an arrow.

The following tests demonstrate many of the relational operators:

```prolog
test(equal) :-
  X is 1,
  Y is 1,
  X =:= Y.

test(not_equal) :-
  X is 1,
  Y is 2,
  X =\= Y.

test(alphabetically) :-
  'dog' @< 'fox',
  'fox' @> 'dog',
  'dog' == 'dog',
  'dog' @=< 'dog',
  'dog' @>= 'dog'.

test(identical) :-
  x(A, B) == x(A, B). % same functor name and argument variables

test(not_identical) :-
  x(A, B) \== x(C, D). % different argument variables

test(structurally_equivalent) :-
  [a, [b, c], d] =@= [a, [b, c], d],
  x(A, B) =@= x(C, D).

test(not_structurally_equivalent) :-
  x(A, B) \=@= x(C, D, E), % different arity
  x(A, B) \=@= y(C, D). % different functor name
```

### Arithmetic Operators

Prolog supports the following arithmetic operators:

| Operator | Meaning                        |
| -------- | ------------------------------ |
| `+`      | addition (infix and prefix)    |
| `-`      | subtraction (infix and prefix) |
| `*`      | multiplication                 |
| `/`      | floating point division        |
| `//`     | integer division               |
| `div`    | integer division               |
| `rem`    | remainder of integer division  |
| `rdiv`   | rational number division       |
| `mod`    | modulo                         |
| `**`     | exponentiation                 |
| `^`      | exponentiation                 |

### Bitwise Operators

Prolog supports the following bitwise operators:

| Operator | Meaning              |
| -------- | -------------------- |
| `/\`     | bitwise and          |
| `\/`     | bitwise or           |
| `xor`    | bitwise exclusive or |
| `\`      | bitwise not (prefix) |
| `<<`     | bitwise shift left   |
| `>>`     | bitwise shift right  |

### Constraint Logic Programming (CLP)

The CLP libraries implements Constraint Logic Programming
through a series of new operators.
These supports two primary use cases:

- declarative integer arithmetic
- combinatorial problems

<a href="https://www.swi-prolog.org/pldoc/man?section=clp" target="_blank">CLP<a>
provides a different way of expression Prolog constraints
for values in specific domains.
There are four supported domains:

- <a href="https://www.swi-prolog.org/pldoc/man?section=clpfd"
  target="_blank">CLP(FD)<a> for integers
- <a href="https://www.swi-prolog.org/pldoc/man?section=clpb"
  target="_blank">CLP(B)<a> for Booleans
- <a href="https://www.swi-prolog.org/pldoc/man?section=clpqr"
  target="_blank">CLP(Q)<a> for rational numbers
- <a href="https://www.swi-prolog.org/pldoc/man?section=clpqr"
  target="_blank">CLP(R)<a> for floating point numbers

When using SICStus Prolog, consider using {% aTargetBlank
"https://github.com/triska/clpz", "CLP(Z)" %} as an alternative to CLP(F).

Each of these libraries define new operators
and must be loaded in order to use them.
For example:

```prolog
:- use_module(library(clpfd)).

% Find sub-ranges of the given ranges for X and Y
% where the X value is larger than the Y value.
X in 5..10, Y in 7..14, X #> Y, label([X, Y]).
% output is:
% X in 8..10, % determined that X cannot be less than 8
% Y#=<X+ -1, % means Y is less than or equal to X - 1
% Y in 7..9. % determined that Y cannot be greater than 9

% Find values for X and Y in their respective ranges
% where the X value is larger than the Y value.
% The label predicate finds specific values for which this holds.
X in 5..10, Y in 7..14, X #> Y, label([X, Y]).
% output is:
% X = 8, Y = 7 ;
% X = 9, Y = 7 ;
% X = 9, Y = 8 ;
% X = 10, Y = 7 ;
% X = 10, Y = 8 ;
% X = 10, Y = 9.
```

The following operators are provided by the `clpfd` library:

| Operator             | Meaning                                                                  |
| -------------------- | ------------------------------------------------------------------------ |
| `#=`                 | evaluates arithmetic expression on right and assigns to variable on left |
| `#\=`                |                                                                          |
| `#<`                 |                                                                          |
| `#=<`                |                                                                          |
| `#>=`                |                                                                          |
| `#>`                 |                                                                          |
| `in`                 |                                                                          |
| `ins`                |                                                                          |
| `indomain`           |                                                                          |
| `label`              |                                                                          |
| `labelling`          |                                                                          |
| `all_distinct`       |                                                                          |
| `global_cardinality` |                                                                          |
| `#<===>`             |                                                                          |
| `fd_dom`             |                                                                          |

When using CLP, compare values with `#=` instead of `=:=`.

The following rules describe the relationship between a geometry shape
and its area using operators defined by the "clpr" library:

```prolog
:- use_module(library(clpr)).

area(circle, Radius, X) :- Pi is pi, {X = Pi * Radius^2}.
area(square, Side, X) :- {X = Side^2}.
area(rectangle, Width, Height, X) :- {X = Width * Height}.
```

The following is another way to describe the relationship
between a circle and its area without using CLP:

```prolog
radius_area(R, A) :-
    ground(R), % tests whether R is not a free variable
    A is pi * R^2.

radius_area(R, A) :-
    ground(A), % tests whether A is not a free variable
    R is sqrt(A / pi).
```

### Other Operators

Prolog supports the following additional operators:

TODO: Finish documenting the meaning of some of these operators.

| Operator | Meaning                                                                           |
| -------- | --------------------------------------------------------------------------------- |
| `:-`     | prefix; appears before a compiler directive                                       |
| `:-`     | infix; appears between the head and body of every rule; read as "if"              |
| `?-`     | prefix operator that appears before every question                                |
| `\|`     | separates the head and tail of a list in `[H\| T]`                                |
| `,`      | separates terms to be and'ed                                                      |
| `;`      | separates terms to be or'ed                                                       |
| `->`     | similar to ternary operator `?:` in other languages; called "if-then"             |
| `-->`    | used in DCG grammar rules for implementing parsers                                |
| `\+`     | prefix operator that succeeds when the goal that follows does not hold            |
| `=`      | attempts to unify by finding satisfying variable values on LHS and RHS            |
| `\=`     | tests whether two terms cannot be unified                                         |
| `=..`    | creates a goal from a list containing a functor name and arguments; called "univ" |
| `is`     | attempts to unify LHS with RHS arithmetic expression result                       |
| `>:<`    | partial unification between to dictionaries                                       |
| `!`      | cut; prevents further backtracking                                                |
| `$`      | similar to `!` TODO How does it differ?                                           |
| `*->`    | soft cut; rarely used                                                             |
| `:=`     | evaluates RHS as JavaScript (odd!)                                                |
| `:<`     | succeeds when LHS is a sub-dict of RHS dict                                       |
| `?`      | TODO: Does this compose two predicates?                                           |
| `:`      |                                                                                   |
| `\_`     |                                                                                   |
| `/`      |                                                                                   |
| `.`      |                                                                                   |
| `as`     |                                                                                   |
| `=>`     |                                                                                   |

Directives provide information to the Prolog compiler.
They are preceded by the `:-` prefix operator.
Highlights include:

- `dynamic(functors)`: allows adding and removing clauses for given functors
- `if`, `elif`, `else`, and `endif`: support conditional compilation
- `include(file)`: includes a given source file
- `initialization(goal)`: executes a given goal at startup;
  multiple initializations are executed in the order they appear
- `module`: declares a module definition
- `multifile(functors)`: allows clauses for given functors
  to reside in multiple source files
- `op(priority, kind, name)`: defines a custom operator
- `public(functors)`: allows inspecting clauses of given functors
- `set_prolog_flag(flag, value)`: sets a flag to change compiler operation
- `use_module(library)`: declares usage of a module;
  for example, `:- use_module(library(clpfd)).`

Some Prolog implementations allow placing `:-` before any goals
to execute it when the file is loaded.
But the ISO standard requires using the `initialization` directive.
The goals that follow do not need to be wrapped in parentheses.
For example:

```prolog
:- initialization
  writeln('Hello'),
  writeln('Goodbye').
```

The `?-` operator precedes questions.
The top level of most Prolog implementations displays that operator as a prompt.
I have not found a reason to actually used the `?-` operator in code.

There are two primary differences between `=` and `is`.
The first is that the RHS of `is` must be an arithmetic expression.
The second is that `is` evaluates the RHS whereas `=` does not.

One way to evaluate a mathematical expression is to assign it to a variable.
For example, we can compute the angle in degrees
whose `sin` is `0.5` as follows:

```prolog
% The asin function returns an angle in radians.
?- Angle is asin(0.5) * 180 / pi.
Angle = 29.999999999999996.
```

After evaluating this, the variable `Angle` is no longer defined.

The `->` operator provides the equivalent of an "if" statement
or ternary operator in other programming languages.
The expression on the left can be a goal or conditional expression.
There can be two parts on the right separated by a semicolon.
The first part is used if the left side holds and
the second part is used if it does not.
Note that Prolog does not support the concept of Booleans,
so this is not decided based on whether the left side evaluates to "true".

See the "Conditional Logic" section for details on the `->` operator.

The `=..` operator is typically used in conjunction with the `call` predicate
to dynamically create a goal and execute it. For example:

```prolog
Format = 'X=~w and Y=~w~n',
Args = [1, 2],
Goal =.. [format, Format, Args],
call(Goal). % outputs "X=1 and Y=2"
% This is equivalent to the non-dynamic
% format('X=~w and Y=~w~n', [1, 2])
```

### Custom Operators

Custom operators can be defined.
There are two required parts, declaration and implementation.
The `op` predicate declares the precedence, type, and name of an operator.

The precedence is a number between 0 and 1200
where 0 removes the declaration and 1 is the highest precedence.
This is used to determine the order in which operators are evaluated
in expressions that include multiple operators.

The type defines whether the operator is:

- prefix: `fx` or `fy`
- infix: `xfx`, `xfy`, or `yfx`
- postfix: `xf` or `yf`

The name can be any symbol.

The following code defines an operator named `dbl` that doubles a number:

```prolog
:- arithmetic_function(dbl/1).
:- op(10, fx, dbl).
dbl(X, Y) :- Y is X * 2.

?- X is dbl 5. % gives 10
```

The expression `dbl 5` generates the arithmetic expression `5 * 2`
and the `is` operator evaluates that to get `10`.

Existing operators, except the comma operator, can be redefined.
The `|` operator can only be redefined as an infix operator
whose precedence is at least 1001.

The `current_op` predicate questions operators.
For example:

```prolog
% Get the priority and type of the "is" operator.
current_op(P, T, 'is').
% output is P = 700, T = xfx.

% Get all prefix operators.
current_op(P, fx, N).
% output is P = 1, N = ($); and many more
% All the names are output inside parentheses. Why?
```

## Booleans

There is no Boolean type in Prolog.
Often the atoms `true` and `false` are used to represent Boolean values,
but these do not have a predefined meaning.

Rather than writing a rule that set an argument to `true` or `false`,
it is preferable to write a rule that succeeds or fails.

## Numbers

The following rules determine whether a given number is even or odd:

```prolog
even(N) :- mod(N, 2) =:= 0.
odd(N) :- mod(N, 2) =:= 1.
```

TODO: Add more detail here!

## Strings

Literal strings can be delimited with single or double quotes.
To escape a quote inside a literal string, precede it with a backslash.

The `double_quotes` compiler flag specifies how
double-quoted (not single) strings are treated.
When set to `string` (default in SWI-Prolog), they remain strings.
When set to `chars`, (default in Scryer Prolog),
they become lists of single-character atoms.
When set to `codes`, they become lists of Unicode code-point integers.
When set to `atom`, they become a single atom containing all the characters.
For example:

```prolog
?- set_prolog_flag(double_quotes, chars).
L = "abc". % atoms, not characters
% L = [a, b, c].
```

The benefits of representing strings as lists of characters are that
list predicates can be used to operate on them and
they can be partially instantiated with variable characters.

When the `double_quotes` flag is set to `chars`, the following are equivalent:

- `""` and `[]`
- `"a"` and `[a]`
- `"abc"` and `[a, b, c]`

{% aTargetBlank "https://www.swi-prolog.org/pldoc/man?section=string",
"The string type and its double quoted syntax" %} section 5.2.3
discusses the pros and cons of the string options.
In some cases it seems best to use single quotes
so the meaning stays the same regardless of the flags set.

A single quoted string containing no special characters such as spaces
is equivalent to an atom with the same characters.
For example, `'demo' == demo` is true.

When strings become lists, predicates that operate on lists can be used.

To get the length of a string, use the `atom_length` function.
For example:

```prolog
?- atom_length('Mark', X).
X = 4.
```

To test whether a string contains a given substring,
which could be a single character, use the `sub_string` predicate.
For example:

```prolog
% 1st argument is the string to search.
% 2nd argument is the number of characters before the substring.
% 3rd argument is the number of characters in the substring.
% 4th argument is the number of characters after the substring.
% 5th argument is the substring to find.
% It is not necessary to capture arguments 2-4.
once(sub_string(S, _, _, _, C)) ->
  writeln('found');
  writeln('not found').
```

To create a list of ASCII values from a literal string,
use the `name` function.
For example:

```prolog
?- name('ABC', X).
X = [65, 66, 67].
```

To create a string from a list of ASCII values,
also use the `name` function.
For example:

```prolog
?- name(X, [65, 66, 67]).
X = 'ABC'.
```

To append two strings, convert them to lists of ASCII codes,
append those lists, and convert the result back to a string.
For example:

```prolog
appendStrings(S1, S2, SR) :-
  name(S1, L1),
  name(S2, L2),
  append(L1, L2, LR),
  name(SR, LR).

appendStrings('first ', 'second', X).
X = 'first second'
```

To append multiple atomic values, including strings,
use the `atomics_to_string` predicate. For example:

```prolog
atomics_to_string(["foo", 3, 'bar'], S).
% output is "foo3bar"
```

The above approach will not work with double-quoted strings
if the `double_quotes` flag is set to `chars` because in that case
double-quotes strings will be treated as lists of atoms
and lists are not atomic.

To join multiple atomic values with a delimiter between each,
use the 3-argument version of `atomics_to_string`. For example:

```prolog
atomics_to_string(["foo", 3, 'bar'], '|', S).
% output is "foo|3|bar"
```

To get a single character from a string, convert it to a list of ASCII codes,
and use the `nth0` function.
For example:

```prolog
?- name('Mark', L), nth0(2, L, C), put(C). % 114 (ASCII code for 'r')
```

To get the tail of a string when the `double_quotes` flag is set to `chars`,
use the `append` predicate. For example:

```prolog
append("foo", T, "foobarbaz")
% sets T to "barbaz"
```

To split a string on a delimiter such as a space
when the `double_quotes` flag is set to `chars`,
use the `append` predicate. For example:

```prolog
append(Word1, [' '|Rest], "Red Green Blue").
% sets Word1 to "Red" and Rest to "Green Blue" as the first solution
% sets Word1 to "Red Green" and Rest to "Blue" as the second solution
```

## Arithmetic Functions

Prolog supports a large number of functions that return a number.
See {% aTargetBlank "https://www.swi-prolog.org/pldoc/man?section=functions",
"Arithmetic Functions" %}.
These include

- `abs`: absolute value
- bitwise operations such as shift and `xor`
- `ceiling`: smallest integer that is greater than or equal to a value
- `e`: value of e (2.71828...)
- `floor`: largest integer that is less than or equal to a value
- `exp`: e to a given power
- `gcd`: greatest common denominator
- `inf`: positive infinity
- `lcm`: least common multiple
- `log`: logarithm base e (natural logarithm)
- `log10`: logarithm base 10
- `max`: maximum of two values
- `min`: minimum of two values
- `nan`: not a number value
- `pi`: value of pi (3.14159...)
- `random`: random integer between zero and an upper bound
- `random_float`: random float between zero and one (exclusive on both ends)
- `succ`: successive value; `succ(2, X)` gives `3`; `succ(X, 3)` gives `2`
- `truncate`: similar to `floor`, but rounds toward zero for negative numbers
- trigonometry functions

## Conditional Logic

Prolog does not have the equivalent of an `if` or `select` statement
found in many other programming languages.
But it does have the `->` operator which is somewhat like
the ternary operator in other programming languages.
An "else" part is often chained onto this using the `;` operator.
For example:

```prolog
sign_word(N, Word) :-
  (N = 0 -> Word = 'zero';
  (N > 0 -> Word = 'positive';
  Word = 'negative')).
```

When expressions using the `->` operator appear in a conjunction
(comma-separated list of goals), it must be wrapped in parentheses
in order to be treated as a single goal in the conjunction.

When the `;` operator is not used,
it is treated as if it were specified with the `fail` predicate.
This means that `condition -> true-part` is
the same as `condition -> true-part; fail`.
For example:

```prolog
N = -3,
writeln('before'), % will print
(N > 0 -> writeln('positive')), % will not print due to failing
writeln('after'), % will not print due to failing
```

To fix the scenario above, replace the arrow line with the following:

```prolog
(N > 0 -> writeln('positive'); true),
```

## Iteration

Iteration in Prolog is done with recursion.

To get all the integers starting from one integer and ending at another,
use the `between` function.
For example:

```prolog
% This sets V to 3, 4, 5, 6, and 7.
?- between(3, 7, V).
```

The `between` predicate can also be used to test
whether a value is between two numbers inclusively.
For example, the following are equivalent:

```prolog
Row >= 0, Row =< 7.
between(0, 7, Row).
```

## List of Solutions

The combination of the `findall` and `label` predicates are useful for
creating a list of values that satisfy given constraints.
For example:

```prolog
:- use_module(library(clpfd)).

add(A, B, C) :- C #= A + B.

:- initialization
  findall(
    [A, B], % transform each solution into the list [A, B]
    % Find all pairs of integers that satisfy these constraints.
    (add(A, B, 5), A in 1..5, A #> B, label([A, B])),
    Results % set this
  ),
  format('Results = ~w~n', [Results]), % [[3,2],[4,1],[5,0]]
  halt.
```

## Currying (Runtime Predicates)

Prolog supports a form currying by passing
fewer arguments to a predicate than it requires.
The result must be passed to the `call` predicate in order to evaluate it.
For example:

```prolog
:- use_module(library(clpfd)).

sum2(X, Y, Z) :-
  Z #= X + Y.

sum3(A1, A2, A3, A4) :-
  A4 #= A1 + A2 + A3.

:- initialization
  sum3(1, 2, 3, R1),
  writeln(R1), % 6

  sum3(1, R2, 3, 6),
  writeln(R2), % 2

  % Currying one argument.
  call(sum3(10), 20, 30, R3),
  writeln(R3), % 60

  % Currying two arguments.
  call(sum3(10, 20), 30, R4),
  writeln(R4), % 60

  % Currying goal passed to maplist.
  Numbers = [1, 2, 3],
  maplist(sum2(10), Numbers, R5),
  writeln(R5), % [11,12,13]

  P = <, % could be set to a different relational operator
  Term =.. [P, 3, 5], % builds term from list containing functor and arguments
  % call(Term), % evaluates term
  (call(Term) -> writeln('yes'); writeln('no')), % yes

  halt.
```

A predicate can be placed in a variable at runtime
and later used to create a term with the `:..` operator
which is evaluated using the `call` predicate.
For example:

```prolog
P = <, % could be set to a different relational operator
Term =.. [P, 3, 5], % builds term from list containing functor and arguments
call(Term). % evaluates term
```

## Help

To find information about built-ins related to a specific word in SWI-Prolog,
enter `apropos(word).`. For example, `apropos(pair).` outputs the following:

```prolog
% LIB pairs_keys/2                        Remove the values from a list of Key-Value pairs.
% LIB pairs_values/2                      Remove the keys from a list of Key-Value pairs.
% LIB pairs_keys_values/3                 True if Keys holds the keys of Pairs and Values the values.
% SWI dict_pairs/3                        Bi-directional mapping between a dict and an ordered list of pairs (see secti ...
%   C 'PL_is_pair'()                      Returns non-zero if term is a compound term using the list constructor.
% SWI stream_pair/3                       This predicate can be used in mode (-,+,+) to create a stream-pair from an in ...
% SEC 'summary-lib-pairs'                 library(pairs)
% LIB protobuf_map_pairs/3                Convert between a list of protobuf map entries (in the form DictTag{key:Key,  ...
% LIB transpose_pairs/2                   Swap Key-Value to Value-Key.
% LIB json_dict_pairs/2                   This hook may be used to order the keys of an object.
% LIB map_list_to_pairs/3                 Create a Key-Value list by mapping each element of List.
% LIB group_pairs_by_key/2                Group values with equivalent (==/2) consecutive keys.
% ISO keysort/2                           Sort a list of pairs.
% LIB all_distinct/1                      True iff Vars are pairwise distinct.
%   C 'PL_clear_hash_table'()             Delete all key-value pairs from the table.
%   C 'PL_advance_hash_table_enum'()      Get the next key-value pair from a cursor.
%   C 'PL_new_hash_table'()               Create a new table for size key-value pairs.
% SEC pairs                               library(pairs): Operations on key-value lists
% LIB assoc_to_list/2                     Translate Assoc to a list Pairs of Key-Value pairs.
%   C 'PL_STRINGS_MARK'()                 These macros must be paired and create a C block ({...}).
% Showing 20 of 52 matches
%
% Use ?- apropos(Type:Query) or multiple words in Query
% to restrict your search.  For example:
%
%   ?- apropos(iso:open).
%   ?- apropos('open file').
```

For more detailed help on a specific predicate, enter `help(functor/arity).`
For example, `help(between/3).`

## Listing

To list all the clauses (facts and rules) known in the current session,
enter `listing.`.
The output will contain many built-in clauses in addition to those you loaded.

To list only the clauses for a given functor name,
enter `listing(functor-name).`
This will list all matching clauses regardless of arity.

For example, `listing(append).` shows the implementation of this functor name.

## Debugging

To see all the steps used to evaluate a predicate,
turn on trace mode by entering `trace.`

Enter a question and press the return key after
viewing the result of each step in the evaluation.

When finished debugging, enter `notrace.` to turn this mode off.

## Calling From Other Languages

SWI-Prolog can be called from C. See {% aTargetBlank
"https://www.swi-prolog.org/pldoc/man?section=calling-prolog-from-c",
"Calling Prolog from C" %}.

TODO: Which other programming languages can call SWI-Prolog?

## Efficiency

For information about the performance of Prolog, see {% aTargetBlank
"https://www.metalevel.at/prolog/efficiency", "Efficiency of Prolog" %}.

## Unit Tests

SCI-Prolog includes a unit testing framework called "Test Box".
See <a href="https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)"
target="_blank"> Prolog Unit Tests</a>.

Code for unit tests can be placed in the same source file
as the rules they test.
Alternatively, test code can be placed in a separate file
with an extension of `.plt`.

The following code demonstrates implementing unit tests
for the built-in `append` rule.

```prolog
% This line is only needed to load predicates from another file.
% :- consult({file-name}).

:- begin_tests(append).

test(append_assertions) :-
  append([], [], []),
  append([a], [], [a]),
  append([], [a], [a]),
  append([a, b], [c, d], [a, b, c, d]).

test(append_make_first) :-
  append(X, [c, d], [a, b, c, d]),
  assertion(X == [a, b]),
  !.

test(append_make_second) :-
  append([a, b], X, [a, b, c, d]),
  assertion(X == [c, d]).

test(append_make_third) :-
  append([a, b], [c, d], X),
  assertion(X == [a, b, c, d]).

:- end_tests(append).
:- run_tests.
:- halt.
```

If the code above is in a file named `append.plt`
then the tests can be run by entering `swipl append.plt`.
If the last two lines in the code above are omitted,
use the following instead:
`swipl -g run_tests -t halt your/file.pl`

The `test` rule takes a test name (atom or string)
and an optional list of options.
Supported options include:

- `setup`: takes a goal to execute before the test is run
- `cleanup`: takes a goal to execute after the test is run
- `forall`: takes a generator and runs the test for each generated value
- `throws`: takes an error and verifies that the test throws the error
- `error`: takes an error and verifies that the test throws `error(Error, _Context)`
- several other options that seem less valuable

The `assertion` rule prints assertions that fail.
When this is not used, the output will only provide
the name of the test that failed.

If a test ends with a choice point, a warning message will be output.
To prevent this, end the test with the cut operator (`, !.`)
or include the option `nondet`.

## Sudoku

Prolog can be used to solve puzzles such as {% aTargetBlank
"https://en.wikipedia.org/wiki/Sudoku", "Sudoku" %}.

The following code is based on code from Markus Triska in the SWI-Prolog manual.

```prolog
:- use_module(library(clpfd)).

sudoku(Rows) :-
  % Verify that Rows is a list with 9 elements.
  length(Rows, 9),

  % Verify that all elements are lists
  % with the same length as Rows which is 9.
  maplist(same_length(Rows), Rows),

  % Create a flattened list of all the values (Vs), and verify
  % that all elements in Vs are a number in the range 1 to 9.
  append(Rows, Vs), Vs ins 1..9,

  % Verify that all element values in all rows
  % are unique within their row.
  maplist(all_distinct, Rows),

  % Create a list of lists that represent the columns.
  transpose(Rows, Columns),

  % Verify that all element values in all columns
  % are unique within their column.
  maplist(all_distinct, Columns),

  % Assign a variable name to each of the 9 rows.
  [R1, R2, R3, R4, R5, R6, R7, R8, R9] = Rows,

  % Verify that the element values in every 3x3 block
  % are unique within their block.
  blocks(R1, R2, R3),
  blocks(R4, R5, R6),
  blocks(R7, R8, R9).

% When a block is empty, its element values (which are none)
% can be considered unique.
blocks([], [], []).

% When a block is not empty, get its 9 values
% and verify that they are unique.
blocks([R1C1,R1C2,R1C3|T1], [R2C1,R2C2,R2C3|T2], [R3C1,R3C2,R3C3|T3]) :-
  all_distinct([R1C1, R1C2, R1C3, R2C1, R2C2, R2C3, R3C1, R3C2, R3C3]),
  blocks(T1, T2, T3).

% When there a no more rows, stop printing.
print_rows([]).

% When there are more rows, print the first row.
print_rows([H|T]) :- print_row(H), print_rows(T).

% When the last element of a row has been printed, print a newline.
print_row([]) :- nl.

% When there are more row elements,
% print the first one followed by a space.
print_row([H|T]) :- format('~w ', H), print_row(T).

% Each puzzle must contain at least 17 clues.

problem(1, % can solve
  [[_,_,_, _,_,_, _,_,_],
   [_,_,_, _,_,3, _,8,5],
   [_,_,1, _,2,_, _,_,_],

   [_,_,_, 5,_,7, _,_,_],
   [_,_,4, _,_,_, 1,_,_],
   [_,9,_, _,_,_, _,_,_],

   [5,_,_, _,_,_, _,7,3],
   [_,_,2, _,1,_, _,_,_],
   [_,_,_, _,4,_, _,_,9]]).

:- problem(1, Rows), sudoku(Rows), print_rows(Rows).
```

This outputs the following solution:

```text
9 8 7 6 5 4 3 2 1
2 4 6 1 7 3 9 8 5
3 5 1 9 2 8 7 4 6
1 2 8 5 3 7 6 9 4
6 3 4 8 9 2 1 5 7
7 9 5 4 6 1 8 3 2
5 1 9 2 8 6 4 7 3
4 7 2 3 1 9 5 6 8
8 6 3 7 4 5 2 1 9
```

## Einstein's Riddle

Einstein's riddle, aka {% aTargetBlank
"https://en.wikipedia.org/wiki/Zebra_Puzzle", "Zebra Puzzle" %},
describes a set of known facts and relationships
and asks you to find some set of unknown values.

There are several examples of this type of puzzle.

One begins with "Three kids went to a superheroes dress birthday party."
The following code solves this puzzle.

```prolog
% The names of the three kids are Ethan, Ali and Anya.
kid(ethan).
kid(ali).
kid(anya).

% They dressed up as Spiderman, Captain America and Iron Man.
hero(spiderman).
hero(captain_america).
hero(iron_man).

% The kids are 6, 8 and 10 years old.
age(6).
age(8).
age(10).

% Anya was dressed up as Spiderman.
kid_hero_age(anya, spiderman, A) :- age(A).

% Ethan was not dressed up as Captain America.
kid_hero_age(ethan, H, A) :- hero(H), age(A), H\=captain_america.

% The youngest kid dressed up as Spiderman.
kid_hero_age(K, spiderman, 6):- kid(K).

% The kid who is 8 years old dressed up as Captain America.
kid_hero_age(K, captain_america, 8) :- kid(K).

% Three values are distinct if this holds.
different(A, B, C) :-
  A \= B, A \= C, B \= C. % use distinct list?

% Determine the missing information.
solve(K1, H1, A1, K2, H2, A2, K3, H3, A3) :-
  kid_hero_age(K1, H1, A1),
  kid_hero_age(K2, H2, A2),
  kid_hero_age(K3, H3, A3),
  different(K1, K2, K3),
  different(H1, H2, H3),
  different(A1, A2, A3),
  !.

:- solve(K1, H1, A1, K2, H2, A2, K3, H3, A3),
   S = '~w is ~w and dressed as ~w.~n',
   format(S, [K1, A1, H1]),
   format(S, [K2, A2, H2]),
   format(S, [K3, A3, H3]),
   halt.
```

The output is:

```
anya is 6 and dressed as spiderman.
ethan is 10 and dressed as iron_man.
ali is 8 and dressed as captain_america.
```

The classic Zebra puzzle is a bit more difficult.
It asks you to determine who owns a zebra.

There are five nationalities:
englishman, japanese, norwegian, spaniard, and ukrainian.

There are five houses colors:
blue, green, ivory, red, and yellow.

There are five drinks:
coffee, milk, orange_juice, tea, and water.

There are five smokes:
chesterfields, kools, lucky_strike, old_gold, and parliaments.

There are five pets:
dog, fox, horse, snails, and zebra.

The following code solves this puzzle.

```prolog
% The relation arguments are Nationality, Color, Drinks, Smokes, and Pet.

% List element A is on the left of list element B
% if appending $ something onto a list
% beginning with A,B results in a given list.
on_left(A, B, Ls) :- append(_, [A,B|_], Ls).

% List element A is on the right of list element B
% if B is on the left of A.
on_right(A, B, Ls) :- on_left(B, A, Ls).

% List elements A and B are adjacent
% if A is on the left or right side of B.
adjacent(A, B, Ls) :- on_left(A, B, Ls); on_right(A, B, Ls).

% This gets a list of all the houses contain all their details.
houses(Hs) :-
  % There are five houses.
  length(Hs, 5),

  % The Englishman lives in the red house.
  member(relation(englishman, red, _, _, _), Hs),

  % The Spaniard owns the dog.
  member(relation(spaniard, _, _, _, dog), Hs),

  % Coffee is drunk in the green house.
  member(relation(_, green, coffee, _, _), Hs),

  % The Ukrainian drinks tea.
  member(relation(ukrainian, _, tea, _, _), Hs),

  % The green house is immediately to the right of the ivory house.
  on_left(
    relation(_, ivory, _, _, _),
    relation(_, green, _, _, _),
    Hs),

  % The Old Gold smoker owns snails.
  member(relation(_, _, _, old_gold, snails), Hs),

  % Kools are smoked in the yellow house.
  member(relation(_, yellow, _, kools, _), Hs),

  % Milk is drunk in the middle house.
  Hs = [_, _, relation(_, _, milk, _, _), _, _],

  % The Norwegian lives in the first house.
  Hs = [relation(norwegian, _, _, _, _) | _],

  % The man who smokes Chesterfields lives in
  % the house next to the man with the fox.
  adjacent(
    relation(_, _, _, chesterfields, _),
    relation(_, _, _, _, fox),
    Hs),

  % Kools are smoked in the house next to the house where the horse is kept.
  adjacent(
    relation(_, _, _, kools, _),
    relation(_, _, _, _, horse),
    Hs),

  % The Lucky Strike smoker drinks orange juice.
  member(relation(_, _, orange_juice, lucky_strike, _), Hs),

  % The Japanese smokes Parliaments.
  member(relation(japanese, _, _, parliaments, _), Hs),

  % The Norwegian lives next to the blue house.
  adjacent(
    relation(norwegian, _, _, _, _),
    relation(_, blue, _, _, _),
    Hs),

  % Someone drinks water.
  member(relation(_, _, water, _, _), Hs),

  % Someone owns a zebra.
  member(relation(_, _, _, _, zebra), Hs).

zebra_owner(N) :-
	houses(Hs),
	member(relation(N, _, _, _, zebra), Hs),
	!.

water_drinker(N) :-
	houses(Hs),
	member(relation(N, _, water, _, _), Hs),
	!.

print_houses([]).

print_houses([H|T]) :-
  relation(N, C, D, S, P) = H,
  %S = 'The ~w lives in the ~w house, drinks ~w, smokes ~w, and owns a ~w.~n',
  %format(S, [N, C, D, S, P]),
  format(
    'The ~w lives in the ~w house, drinks ~w, smokes ~w, and owns a ~w.~n',
    [N, C, D, S, P]),
  print_houses(T).

:- houses(Hs), print_houses(Hs).
```

The output is:

```
The norwegian lives in the yellow house, drinks water, smokes kools, and owns a fox.
The ukrainian lives in the blue house, drinks tea, smokes chesterfields, and owns a horse.
The englishman lives in the red house, drinks milk, smokes old_gold, and owns a snails.
The spaniard lives in the ivory house, drinks orange_juice, smokes lucky_strike, and owns a dog.
The japanese lives in the green house, drinks coffee, smokes parliaments, and owns a zebra.
```

## Search Strategies

Prolog implementations can employ many search strategies.

A forward chaining search strategy starts from a starting state
and derives new states that can follow using rules describing valid changes
until a goal state is reached.

A backward chaining search strategy starts from a goal state
and derives new states that can precede it using rules describing valid changes
until the starting state is reached.

TODO: Add information about other search strategies
TODO: that Prolog implementations typically use.

## Reading from URLs

The following code prints all the tag names found in an HTML document.
They are indented to represent their position in the DOM hierarchy.
This was only tested in SWI-Prolog.

```prolog
:- use_module(library(http/http_open)). % for http_open

indent_write(Indent, V) :-
  format('~*|~t~w~n', [Indent, V]).

% This is used if 2nd argument is an element structure.
% element structure components are Tag, Attributes, and Children.
print_tag(Level, element(Tag, _, Children)) :-
  Indent is Level * 2,
  indent_write(Indent, Tag),
  NextLevel is Level + 1,
  maplist(print_tag(NextLevel), Children).

% This is used if 2nd argument is not an element structure.
print_tag(_, _). % ignore

% This provides a starting level of 0.
print_tag(E) :- print_tag(0, E).

process(In) :-
  % copy_stream_data(In, user_output). % for debugging
  load_html(In, DOM, []),
  maplist(print_tag, DOM).

:- setup_call_cleanup(
     % Must use single, not double quotes around URL!
     http_open('https://mvolkmann.github.io', In, []),
     process(In),
     close(In)).
```

## Definite Clause Grammars (DCGs)

A DCG defines a set of grammar rules (GR)
where each has the syntax `GRHead --> GRBody`.
These describe a sequence of allowed terminals, non-terminals, and goals.
Be careful to include two dashes in the arrow and not just one.

DCGs are not yet part of the ISO Prolog standard, but they are being considered.
Most Prolog implementations already support DCGs.

DCGs are enabled by default in SWI-Prolog,
but not in all Prolog implementations.
To enable DCGs, it may be necessary to include the `dcg` library
with `:- use_module(library(dcgs)).`
This can be added to the configuration file for a Prolog implementation
so the `dcg` library is always available.

Grammar rules have the syntax `GRHead -> GRBody.`

The name at the beginning of `GRHead` is used to refer to the rule.
Grammar rule names typically describe the kinds of sequences they allow
rather than describe their arguments as is common in Prolog predicates.

Each GRBody consists of terminals, non-terminals, and grammar body goals.
A terminal is an allowed value.
A non-terminal is written as Prolog goal.

The notation `F//N` refers to the non-terminal `F` with `N` arguments.

Predefined non-terminals include:

- `(,)//2`: concatenation; read as "and then" or "followed by"
- `(|)//2`: alternatives; read as "or"

The predicate `phrase(GRBody, L)` holds if
`L` is a list that matches `GRBody`.
This is the main way to find solutions to a DCG rule.
It can be used to test, complete, and generate solutions.
Since grammar rules can be used in all of these usage modes,
it is preferable to say that a grammar rule "describes" conforming sequences
rather than using words like "generates" and "consumes".

For example, the following grammar rules describe sequences
that contain any number of `x` characters.

Recall that when the `double_quotes` flag is set to `chars`
then `"abc"` is the same as `[a, b, c]`.

```prolog
xs --> "".
xs --> "x", xs.

% To test solutions ...
phrase(xs, "xyx"). % false
phrase(xs, "xxx"). % true

% To complete solutions ...
phrase(xs, [x, A, x, x, B, x]). % solution is A = B, B = x.

% To generate solutions ...
phrase(xs, Ls). % finds all possible solutions
```

All DCG rules can be translated to a standard Prolog rules
which typically require longer code.
Most DCG implementations do this behind the scenes
and then consider those rules at runtime instead of the DCG rules.

The predicate `seq(L)` describes a sequence of values.
For example, the following finds all combinations of `Xs` and `Ys` values
that can be concatenated to form `"abc"`:

```prolog
?- phrase((seq(Xs), seq(Ys)), "abc").
% output is:
% Xs = [], Ys = "abc"
% ;  Xs = "a", Ys = "bc"
% ;  Xs = "ab", Ys = "c"
% ;  Xs = "abc", Ys = []
```

The following code implements predicates that are often useful when using DCGs:

```prolog
% Definition of a sequence.
% Scryer Prolog provides this in its dcg library.
seq([]) --> [].
seq([H|T]) --> [H], seq(T).

% Concatenation of two lists.
append(Xs, Ys, Zs) :- phrase((seq(Xs), seq(Ys)), Zs).
% append('abc', "xyz", L), writeln(L). % output is [a,b,c,x,y,z]

% Concatenation of any number of lists.
% Scryer Prolog provides this in its dcg library.
seqq([]) --> [].
seqq([H|T]) --> seq(H), seqq(T).
% phrase(seqq(["ab", "cd", "ef"]), L), writeln(L). % output is [a,b,c,d,e,f].

% qes is the reverse of seq.
qes([]) --> [].
qes([H|T]) --> qes(T), [H].

palindrome(L) :- phrase(qes(L), L).
% palindrome('mother'). % false
% palindrome('mom'). % true

% This describes any sequence.
... --> [] | [_], ... .

% ... can be used to get the last element in a list.
% phrase((..., [Last]), "xyz"). % output is Last = z; false.

% ... can be used to determine if a given sublist
% occurs anywhere in a list.
% phrase((..., "y", ...), "xyz"). % output is true
% phrase((..., "ar", ...), "Mark"). % output is true

% ... can be used to determine if
% any element occurs twice in succession in a list.
% phrase((..., [X, X], ...), "Mississippi"). % finds s, s, and p
```

A DCG can be used to describe a tree structure.

```prolog
nodes(nil) --> [].
nodes(node(Node, L, R)) --> [Node], nodes(L), nodes(R).
/*
phrase(
  nodes(
    node(
      a,
      nil,
      node(
        b,
        node(c, nil, nil),
        nil
      )
    )
  ),
  L
). % output is a list of the leaf node values [a, b, c].
*/
```

DCGs can be used for parsing by describing a relationship
between lists of characters and syntax trees.

Grammar bodies can contain Prolog goals in curly braces.

To enable termination it is sometimes useful to include `{false}`.

Another way to achieve termination is to use a different execution strategy
such as "SLG resolution".
To enable this, execute the following:

```prolog
:- use_module(library(tabling)).
:- table expr//0.
```

The following DCG that describes sequences like `1+1+1`
doesn't naturally terminate due to its use of left recursion.
However, it does terminate when SLG resolution is enabled.

```prolog
expr --> "1".
expr --> expr, "+", expr. % uses left recursion
```

This can be rewritten as follows so it terminates without SLG resolution.

```prolog
expr --> "1", expr_rest.
expr_rest --> [].
expr_rest --> "+", expr.
```

Using SLG resolution with DCGs is called "Packrat Parsing".

Lexical analysis is a relationship between a string and sequence of tokens.
For example:

```prolog
% This uses the "eager consumer rule" which causes
% tokens to extend to their maximum length.
% Eager consumes check for the nil case ([]) last.
% Curly braces on needed here to evaluate a Prolog predicate in a DCG rule.
% The char_type predicate here asserts that H is an alphabetic character.
word([H|T]) --> [H], { char_type(H, alphabetic) }, word(T).
word([]) --> [].

words([]) --> [].
words([H|T]) --> ws, word(H), ws, words(T).

ws --> [W], { char_type(W, whitespace) }, ws | [].

% The once predicate wraps another predicate and gives only the first solution.
% once(phrase(words(Ws), "The quick brown fox jumps over the lazy dog")).
% This does not terminate if the string contains
% characters that are not alphabetic or whitespace such as a period.

integer(I) --> digits(Ds), { number_chars(I, Ds) }.

digits([H|T]) --> digit(H), digits_remaining(T).
digits_remaining([H|T]) --> digit(H), digits_remaining(T).
digits_remaining([]) --> [].

digit(D) --> [D], { char_type(D, decimal_digit) }.

% phrase((ws, integer(I), ws), "  1961 ").
% I = 1961.

assignment(V, I) --> ws, word(V), ws, ":=", ws, integer(I), ws.
% once(phrase(assignment(V, I), "  gretzky := 99 ")).
% V = "gretzky", I = 99.
```

### Parsing Sentences

The following code provides a basic example of using a DCG
to perform Natural Language Processing (NLP).
It is based on code in the video "Build Syntax Trees in Prolog with DCGs"
at https://youtu.be/QGXypIkV-GU.

```prolog
:- include(sentences). % This file is shown below.

% From Wikipedia, "English determiners are words such as
% the, a, each, some, which, this, and six
% that are most commonly used with nouns to specify their referents."
determiner --> [the] | [a].

noun --> [cat] | [dog].
noun_phrase --> determiner, noun.
verb --> [chased].
verb_phrase --> verb, noun_phrase.
sentence --> noun_phrase, verb_phrase.

% Enter `test.`
test :-
  phrase(sentence, [the,cat,chased,a,dog]), % matches
  \+ phrase(sentence, [the,cat,chased,a,mouse]), % does not match
  !.

% Enter `complete1.`
complete1 :-
  findall(X, phrase(sentence, [the,X,chased,the,dog]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `complete2.`
complete2 :-
  findall(Rest, phrase(sentence, [the,cat,chased | Rest]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `generate.`
generate :-
  findall(X, phrase(sentence, X), Solutions),
  maplist(atoms_sentence, Solutions, Sentences),
  maplist(writeln, Sentences).

/*
This is a basic example of using a DCG to perform
Natural Language Processing (NLP) in Prolog.
It is based on code in the video "Build Syntax Trees in Prolog with DCGs"
at https://youtu.be/QGXypIkV-GU.
*/

:- include(sentences).

% From Wikipedia, "English determiners are words such as
% the, a, each, some, which, this, and six
% that are most commonly used with nouns to specify their referents."
determiner --> [the] | [a].

noun --> [cat] | [dog].
noun_phrase --> determiner, noun.
verb --> [chased].
verb_phrase --> verb, noun_phrase.
sentence --> noun_phrase, verb_phrase.

% Enter `test.`
test :-
  phrase(sentence, [the,cat,chased,a,dog]), % matches
  \+ phrase(sentence, [the,cat,chased,a,mouse]), % does not match
  !.

% Enter `complete1.`
complete1 :-
  findall(X, phrase(sentence, [the,X,chased,the,dog]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `complete2.`
complete2 :-
  findall(Rest, phrase(sentence, [the,cat,chased | Rest]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `generate.`
generate :-
  findall(X, phrase(sentence, X), Solutions),
  maplist(atoms_sentence, Solutions, Sentences),
  maplist(writeln, Sentences).
```

The following code is from the file `sentences.pl`
which is included in the code above:

```prolog
% This relates a string to the same string,
% but with the first letter capitalized.
capitalize(S0, S1) :-
  string_chars(S0, [H|T]),
  string_upper(H, U),
  atomics_to_string([U|T], S1).

% This relates a list of atoms to a sentence.
atoms_sentence(Atoms, Sentence) :-
  % Convert the list atoms into a list of strings.
  maplist(atom_string, Atoms, Strings),
  % Get the first word and a list of the remaining words.
  [W | Ws] = Strings,
  % Capitalize the first word.
  capitalize(W, C),
  % Join the words back into a single string
  % with a space between each word.
  atomics_to_string([C | Ws], ' ', S),
  % Add period at end.
  string_concat(S, ".", Sentence).

% This converts a list of atoms to a sentence
% and writes it to the current output stream.
write_sentence(Atoms) :-
  atoms_sentence(Atoms, Sentence),
  writeln(Sentence).
```

### Generating Syntax Trees

The following code is an alternate version of the code in the previous section.
It uses grammar goal arguments to capture nested structures
that describe a syntax tree for a parsed sentence.

To capture the atoms that match each grammar rule,
we specify a structure as the argument of each rule.
These structures can contain arguments that are fixed atoms
or variables that are set in the grammar rule body.
The grammar rule arguments are "accumulators"
in that they accumulate the result of parsing in a syntax tree.

This enables each grammar rule to generate a tree of structures
that describe what was matched.

```prolog
:- include(sentences).

determiner(d(a)) --> [a].
determiner(d(the)) --> [the].

noun(n(cat)) --> [cat].
noun(n(dog)) --> [dog].
noun_phrase(np(D, N)) --> determiner(D), noun(N).
verb(v(chased)) --> [chased].
verb_phrase(vp(V, Np)) --> verb(V), noun_phrase(Np).
sentence(s(Np, Vp)) --> noun_phrase(Np), verb_phrase(Vp).

% To see a sample tree, enter
% `phrase(sentence(Tree), [a,cat,chased,the,dog]).`

% Enter `test.`
test :-
  phrase(sentence(Tree), [the,cat,chased,a,dog]), % matches
  % Tree = s(np(d(the), n(cat)), vp(v(chased), np(d(a), n(dog)))).
  \+ phrase(sentence(Tree), [the,cat,chased,a,mouse]), % does not match
  !.

% Enter `complete1.`
complete1 :-
  findall(X, phrase(sentence(Tree), [the,X,chased,the,dog]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `complete2.`
complete2 :-
  findall(Rest, phrase(sentence(Tree), [the,cat,chased | Rest]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `generate.`
generate :-
  findall(X, phrase(sentence(Tree), X), Solutions),
  maplist(atoms_sentence, Solutions, Sentences),
  maplist(writeln, Sentences).
```

## Calling From JavaScript

The npm package {% aTargetBlank "https://github.com/rla/node-swipl#readme",
"swipl" %} makes it easy to run Prolog questions from a Node.js application
which can be an Express server.
This requires installing Node.js and SWI-Prolog.

To create a Node.js application that does this:

1. Create a directory for the application and cd to it.
1. Enter `npm init` to create a `package.json` file.
1. Enter `npm install swipl` to install the package.
1. Edit `package.json` and add the script `"start": "node index.js"`.
1. Create the file `index.js` containing code similar to the following:

```js
const swipl = require('swipl');

function loadFile(directory, fileName) {
  swipl.call(`working_directory(_, '${directory}')`);
  swipl.call(`consult(${fileName})`);
}

function printSolution(goal, solution) {
  console.log('Solution for', goal);
  if (solution) {
    console.log(' ', solution.X);
  } else {
    console.error('No solution returned');
  }
}

function printSolutions(goal, question) {
  console.log('Solutions for', goal);
  while (question && (solution = question.next())) {
    console.log(' ', solution.X);
  }
  // Only one question can be open at a time.
  question.close();
}

// This only gets the first solution.
let goal = 'member(X, [1,2,3,4])';
printSolution(goal, swipl.call('member(X, [1,2,3,4])'));

// This gets all solutions.
let question = new swipl.Query(goal);
printSolutions(goal, question);

// This loads a Prolog source file and runs a question against it.
loadFile('..', 'exercise1_3');
goal = 'grandfather_of(richard, X)';
question = new swipl.Query(goal);
printSolutions(goal, question);
```

The Prolog code is not prevented from performing "unsafe" operations.
For example if it invokes the `halt` predicate
then no further questions will be processed. I

## Creating an HTTP Server

SWI-Prolog supports creating an HTTP server that
runs Prolog questions and servers HTML pages contain the results.
For example:

```prolog
:- use_module(library(http/http_server)).

:- initialization
  consult(exercise1_3),
  http_server([port(8081)]).

:- http_handler(
  root(.),
  http_redirect(moved, location_by_id(home_page)),
  []).

:- http_handler(
  root(home),
  home_page,
  []).

home_page(_Request) :-
  % findall gathers all the solutions from the 2nd argument question,
  % transforms them with the first argument,
  % and places the resulting list in the 3rd argument.
  findall(h2(P), grandfather_of(richard, P), L),

  Title = 'Grandchildren of Richard',
  reply_html_page(
    title(Title),
    [h1(Title) | L] % page body
  ).
```

To start the server, enter `swipl {filename}.pl`.
Then browse localhost:{port-number}.

Another way to run Prolog code in an HTTP server is to use {% aTargetBlank
"https://pengines.swi-prolog.org/docs/index.htm", "Pengines" %}.
However, the getting started page says
"We cannot at this time guarantee the safety of the Pengines platform.
We think it is safe, but only if you know what you are doing.
You run it at your own risk!"
This coupled with the fact that the code has not been modified
since November, 2020 leads me to think this may not be a good option.

## Language Server

TODO: How can you install a Prolog language server in Neovim?
TODO: See https://github.com/jamesnvc/lsp_server.

TODO: Can you run Prolog code inside Neovim?

## Miscellaneous Topics

For multithreading, see the {% aTargetBlank
"http://packs.ndrix.com/spawn/index.html", "spawn" %} library.

TODO: Can you create an HTTP server that returns results of a Prolog program?
