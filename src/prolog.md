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

   For example: `father(mark, amanda).`

1. Describe a relationship that is conditionally true ... a rule.

   For example, the following rule states that `G` is a grandfather of `C`
   if `G` is the father of `P` AND `P` is either the father or mother of `C`:

   ```prolog
   grandfather(G, C) :=
     father(G, P),
     (father(P, C); mother(P, C)).
   ```

1. Ask whether a specific relationship is true ... a query with no variables.

   For example, `?- grandfather(richard, amanda).` outputs `true`.

1. Ask for values for which a relationship is true ...
   a query with variables.

   For example, `?- grandfather(G, amanda).` sets `G` to `richard`.
   Sometimes there are multiple values for which a query holds.
   This would be the case if `amanda` had more than one grandfather.

Queries perform "unification" which basically means finding
values for variables that cause a relationship to hold.
This requires pattern matching search and backtracking.
Unification relies on the properties of {% aTargetBlank
"https://en.wikipedia.org/wiki/Horn_clause", "Horn clauses" %}.

The set of facts and rules supplied to the Prolog engine is called a database.
Prolog is highly optimized to handle searching large databases.

Prolog is a homoiconic language, which means its code can be treated as data.

Prolog has many uses including artificial intelligence,
abstract problem solving, symbolic equation solving, and more.

"IBM Watson is a question-answering computer system
capable of answering questions posed in natural language."
It is partially implemented in Prolog.

## Learning Curve

The primary concepts in Prolog such as facts, rules, and queries
are easy to understand after seeing a few examples.

However, writing programs that are entirely
based on relationships rather than imperative code
makes Prolog very different from other programming languages.
This makes learning Prolog somewhat challenging initially.

Also, Prolog supports a large number of operators and built-in predicates.
These take considerable time to learn and master.

## Resources

- {% aTargetBlank "https://www.covingtoninnovations.com/mc/plcoding.pdf",
  "Coding guidelines for Prolog" %}

- {% aTargetBlank "http://www.gprolog.org", "GNU Prolog" %}

- {% aTargetBlank "https://www.iso.org/standard/21413.html",
  "ISO Standard for Prolog" %}

- {% aTargetBlank "https://www.metalevel.at/prolog", "The Power of Prolog" %}
  free, online book by {% aTargetBlank "https://www.metalevel.at",
  "Markus Triska" %}

- {% aTargetBlank "https://link.springer.com/book/10.1007/978-3-642-55481-0",
  "Programming in Prolog" %} Fifth edition book by Clocksin and Mellish

- {% aTargetBlank "https://www.swi-prolog.org", "SWI-Prolog" %}

- {% aTargetBlank
  "https://www.linuxlinks.com/excellent-free-books-learn-prolog/",
  "13 Excellent Free Books to Learn Prolog" %}

- {% aTargetBlank "https://en.wikipedia.org/wiki/Prolog", "Wikipedia" %}

## Implementations

There are many implementations of the Prolog programming language.
The most popular seems to be {% aTargetBlank
"https://www.swi-prolog.org", "SWI-Prolog" %}.
Another option is {% aTargetBlank "http://www.gprolog.org", "GNU Prolog" %}.

Both SWI-Prolog and GNU Prolog conform to the ISO standard and
both were being actively maintained as of June 2023.

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

SWI-Prolog is implemented in a combination of C (48%) and Prolog (39%).

To install the terminal command `swipl` in macOS,
enter `brew install swi-prolog`

To run SWI-Prolog from a terminal, enter `swipl`.

### GNU Prolog

<img alt="GNU Prolog logo" style="width: 20%"
  src="/blog/assets/gnu-prolog-logo.png?v={{pkg.version}}"
  title="GNU Prolog logo">

GNU Prolog is implemented in a combination of C (84%) and Prolog (15%).

To install the terminal command `gprolog` in macOS,
enter `brew install gnu-prolog`.

To run GNU Prolog from a terminal, enter `gprolog`.

### Scryer Prolog

<img alt="Scryer Prolog logo" style="width: 20%"
  src="/blog/assets/scryer-prolog-logo.png?v={{pkg.version}}"
  title="Scryer Prolog logo">

{% aTargetBlank "https://www.scryer.pl", "Scryer Prolog" %}
"is a free software ISO Prolog system intended to be an
industrial strength production environment and
a testbed for bleeding edge research in logic and constraint programming."

Scryer Prolog is implemented in a combination of Rust (64%) and Prolog (36%).

To install, enter the following commands:

```bash
git clone https://github.com/mthom/scryer-prolog
cd scryer-prolog
curl https://sh.rustup.rs -sSf | sh # if cargo is not yet installed
cargo build --release
```

This creates the executable file `target/release/scryer-prolog`.
Define an alias to make this easier to run.

### Other Implementations

- {% aTargetBlank "https://ciao-lang.org", "Ciao" %}
  implemented in Prolog (72%) and C (23%)
- {% aTargetBlank "http://tau-prolog.org", "Tau" %}
  implemented in JavaScript (95%) and Prolog (5%)
- {% aTargetBlank "https://github.com/trealla-prolog/trealla", "Trealla" %}
  implemented in C (82%) and Prolog (18%)

## Online REPL

To enter and run Prolog code in a web browser, browse
{% aTargetBlank "https://swish.swi-prolog.org", "SWISH" %}.

Enter facts and rules in the left pane.
Enter a query in the lower-right pane.
Press the "Run!" button or ctrl-return to execute the query.

## Exiting

To exit from any Prolog interpreter, enter `halt.` or press ctrl-d.

## Terminology

| Term              | Meaning                                                         |
| ----------------- | --------------------------------------------------------------- |
| term              | the only datatype; has four subtypes listed below               |
| - number          | integer or floating point                                       |
| - atom            | identifier that represents a specific thing                     |
| - variable        | represents a value to be determined                             |
| - compound term   | specific combination of terms; more detail below                |
| structure         | another name for a compound term                                |
| fact              | description of something that is true                           |
| rule              | relationship involving one or more unknown things (variables)   |
| predicate         | collection of clauses with the same functor                     |
| clause            | a fact or rule                                                  |
| query             | asks if a clause is true or asks for satisfying variable values |
| database          | collection of predicates                                        |
| functor name      | name of a predicate                                             |
| arity             | number of predicate arguments                                   |
| functor           | function name and its arity; written with a slash between       |
| goal              | rule body expression to be satisfied                            |
| list notation     | comma-separated terms inside square brackets; ex. `[a, B, 7]`   |
| operator notation | terms separated by operators; ex. `Y = m\*X + b`                |
| unification       | process of searching for variable values that satisfy a goal    |
| choice point      | represents a choice in the search for a solution                |

A compound term is a functor name followed by an argument list.
Each argument can be an atom, a variable, a destructuring of variables,
or another compound term.
Nested compound terms can be represented by a tree structure.

A rule can be thought of as a special kind of fact
that depends on a set of other facts

Numbers can include underscores for readability.
For example, `1_234_567` makes it more clear
that this number is greater than one million.

The functor for `foo(bar, baz)` is written as `foo/2`.

For more, see <a href="https://swi-prolog.org/pldoc/man?section=glossary"
target="_blank">Glossary of Terms</a>.

## Syntax

Prolog programs are composed of facts, rules, and queries.
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

### Rules

Rules are written as a head and a body separated by
the "if" operator `:-` and terminated by a period.
They states that the head holds if all the goals in the body hold.
Rules do not return a value like functions in other programming languages.

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

% The previous two lines can be replaced with this.
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
% Output is F = 120.
```

Also see the `sum` example in the "Lists" section.

When a rule is not working as expected,
it may be too general (true for invalid values)
or too specific (false for valid values).
A common way to fix a rule that is too general is to add more goals.
A common way to fix a rule that is too specific
is to add more versions of the rule.

## Queries

Queries test whether a clause is true or
they find variable values for which the clause is true.
Queries are written after the characters `?-`.

For example:

```prolog
% This is a query that asks whether comet is fast.
?- fast(comet). % true

% This is a query that asks for something that is fast.
?- fast(X). % comet
```

Suppose the following facts are loaded:

```prolog
likes(mark, tacos).
likes(mark, books).
likes(mark, running).
```

The query `likes(X, running)` will find "mark".

The query `likes(mark, X)` will find
"tacos", "books", and "running" that in that order.
When a query has multiple matches, as in this example,
the interpreter will wait for further input.

To search for the next match, press the semicolon key.
(SWI-Prolog also supports pressing the n, r, space, or tab keys to do this.)

To stop searching for matches before the last one is found,
press the return key.
(SWI-Prolog also supports pressing the c or a keys to do this.)

After the last match is found, a prompt for the next query will appear.

Variables can be used for any argument of a predicate.
The unification process will find each set of variable values
that cause the predicate to succeed, one set at a time.

### Conjunctions

The comma operator is used to form rules and queries
where multiple goals must be met.
For example:

```prolog
% This rule says that mark likes females that like cycling.
likes(mark, x) := female(X), likes(X, cycling).

% This query asks if mark loves tami AND tami loves mark.
?- loves(mark, tami), loves(tami, mark)`

% This query searches for things that both mark and tami love.
% X stands for the same value in both goals.
?- loves(mark, X), loves(tami, X)
```

Variables retain their values across query conjunctions,
but their values are lost when a query ends.
This is a feature of {% aTargetBlank
"https://en.wikipedia.org/wiki/Static_single-assignment_form",
"static single-assignment" %} (SSA) that is used by Prolog.
In SSA,
"each variable to be assigned exactly once and defined before it is used."
and "every definition (Prolog fact, rule, or query) gets its own version."

For example:

```prolog
X is 6, Y is X * 2, Z is Y / 3.
% Output is X = 6, Y = 12, Z = 4.
% Subsequent queries cannot access these values.
```

## Typical Flow

To start a Prolog REPL, enter an implementation command
such as `swipl` or `gprolog`.

To evaluate a query in the REPL,
enter the query terminated with a period.
If the query does not contain any variables
then `true` or `false` will be output.
If the query does contain variables,
the first set of values that satisfy the query will be output.
To see the next set, press the semicolon key.
A period will be output after the last set.

To evaluate arithmetic operators that result in a numeric value,
assign the expression to a variable using the `=` operator.
For example, entering `X = 1 + 2.` will output `X = 3.`

The typical steps to run a Prolog program are:

1. Add facts and rules to a Prolog source file that has an extension of `.pl`
1. Load Prolog source files into the Prolog app.
1. Enter queries in the Prolog app.

Unfortunately Prolog and Perl use the same file extension
for their source files.

To load a `.pl` file in the REPL,
enter `['file-path'].` or `consult('file-path').`
If the file is in the current directory,
its name can be used without the `.pl` extension
and quotes are not needed inside the square brackets.
For example, to load the file `demo.pl` in the current directory,
enter `[demo].`

Alternatively, pass a source file to the REPL when starting it.
For example, `swipl demo.pl`

After modifying source files that have already been loaded,
enter `make.` to reload all of them.

To enter new facts and rules in a running session:

- Enter `[user].` to open stdin as a pseudo file.
- Enter facts and rules.
- Press ctrl-d to close and load the pseudo file.

It seems this can replace existing facts rather than add to them.

## Tree Representation

Every Prolog clause and query can be represented as a tree
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

main :-
  owns(tami, A),
  format('pet = ~w~n', A), % pet(dog,comet)

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

A list is commonly used to hold a collection of elements when
there can be any number of elements (even zero), their order matters,
and all the elements have the same type (ex. all numbers).

A list can be written as a comma-separated set of terms
surrounded by square brackets.
For example, `[red, green, blue]` is a list of atoms
and `[R, G, B]` is a list of variables that can be
unified with any list containing exactly three elements.

An empty list is written as `[]` which is called "nil".

There are other ways to construct a list.

The dot function is the list constructor.
It is passed the head and the tail of the list to be constructed.
For example, `.(E, Es)` where `E` is a single element that is the head
and `Es` is a list of elements in the tail.
By convention, variable names that end in "s" represent lists.

The head-tail separator `|` creates a list
if the term that follows it is a list.
For example, `[a | []]` is a list containing only `a`
and `[a | [b, c]]` is equivalent to `[a, b, c]`.
Use of the `|` operator can be nested.
For example, `[a | [b | [c]]]` is also equivalent to `[a, b, c]`.

If the `double_quotes` flag is set then
a double-quoted string (not single-quoted) provides a way to write
a list of atoms that correspond to the characters in the string.
For example:

```prolog
?- set_prolog_flag(double_quotes, chars).
L = "abc".
% L = [a, b, c].
```

The following are all equivalent ways to write the same list:

```prolog
[red, green, blue] % list notation

.(red, .(green, .(blue, []))) % functional notation
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
[A, B, C | T] % will be a list of at least three elements if T is a list
```

To get the sum of numbers in a list:

```prolog
L = [1, 2, 3], sum_list(L, Sum).
% output is Sum = 6.
```

The `sum` predicate could be implemented as follows:

```prolog
% This is an example of a recursive rule.
sum(List, Sum) :-
  % If the list is empty then the sum is zero.
  List = [] -> Sum = 0;

  % Otherwise ...
  List =
    % Get the first number and a list of the remaining numbers.
    [Head|Tail],
    % Compute the sum of the remaining numbers.
    sum(Tail, TailSum),
    % The result is the first number plus that sum.
    % Note the use of the "is" keyword to assign the
    % result of an arithmetic expression to the Sum argument.
    Sum is TailSum + Head.

?- sum([1, 2, 3], X).
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
print_list_parts(L) :-
  [H|T] = L,
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
   format("youngest is ~w at age ~w~n", [N1, A1]),
   % output is youngest is carl at age 19

   max_member(younger, Po, People),
   person(N2, A2) = Po,
   format("oldest is ~w at age ~w~n", [N2, A2]).
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
% Output is L4 = [a, b, c, d, e, f].
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

A list of pairs can be sorted using `keysort`.
For example:

```prolog
?- keysort([c-cow, b-bear, a-apple], Ps).
Ps = [a-apple, b-bear, c-cow].
```

TODO: Add detail here.

The following code implements rules to determine if a queen on a chess board
can attach another piece.

```prolog
queen_can_attack((R, _), (R, _)).
queen_can_attack((_, C), (_, C)).
queen_can_attack((R1, C1), (R2, C2)) :-
  abs(R1 - R2) =:= abs(C1 - C2).
```

See {% aTargetBlank "https://eu.swi-prolog.org/pldoc/man?section=pairs",
"library(pairs): Operations on key-value lists" %}.

### Dicts

A dictionary, or dict for short, is a hash map.
To create one, specify a tag followed by an open curly brace,
key/value pairs where there is a colon between each key and value,
and the pairs are separated by commas, and a closing curly brace.
The tag optionally begins with a module name and a colon.
Then it must specific an atom or variable.

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

TODO: Add more detail on working with dicts.

## Type Checking

Prolog provides many built-in predicates that can be used
to assert the type of arguments.

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
test(string) :- demo("hello", T), T == 'string'.
test(atom) :- demo(a, T), T == 'atom'.

test(atomic) :- demo3(true, T), T == 'atomic'.
test(atomic) :- demo3(5, T), T == 'atomic'.
test(atomic) :- demo3(1.2, T), T == 'atomic'.
test(atomic) :- demo3("Hello", T), T == 'atomic'.

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

By default predicates cannot be added or deleted in a session.
To enable this, run a `dynamic` query on a specific predicate.
Once this is done, a predicate of that type can be
added to the beginning with `asserta` or added to the end with `assertz`.
And predicates of that type can be removed
with the `retract` and `retractall` functions.

For example, suppose we have the file `likes.pl` containing the following:

```prolog
% Enable adding and removing "likes" predicates that take two arguments.
:- dynamic(likes/2).
likes(mark, books).
likes(mark, running).
```

A session can do the following:

```prolog
?- [likes].
?- assertz(likes(mark, reeces)). % adds after existing predicates
?- retract(likes(mark, books)). % removes
?- likes(mark, X). % outputs running and reeces

?- retractall(likes(mark, _)). % removes everything that mark likes
?- retractall(likes(_, _)). % removes everything that anybody likes
?- likes(mark, X). % outputs nothing
```

## Input

The `read` function reads a string from stdin.
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
use the `open`, `get_char`, and `close` functions.
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

TODO: Is there an open source library that provides a similar function
TODO: that places the file contents in a variable as a string?

## Output

The `write` predicate writes to the current output stream,
which defaults to stdout.
The atom `nl` is a built-in that writes a newline character
to the current output stream.

For example:

```prolog
write('Hello World!'), nl.

writeln('Hello World!'). % same as previous line
```

The {% aTargetBlank "https://www.swi-prolog.org/pldoc/man?predicate=format/2",
"format" %} predicate also writes to the current output stream.
It takes a format string and a list of values
to be substituted into the format string.

For example:

```prolog
format('~w likes ~w.', [mark, 'Prolog']).
% Outputs "mark likes Prolog."
```

Rules can write to the current output stream.
For example:

```prolog
greet(Name) :- format('Hello, ~w!', [Name]).

greet('Mark')
% Outputs "Hello, Mark!"
```

The following special sequences can be used in format strings:

- `~2f`: substitutes a float value and only outputs two decimal places
- `~n`: newline character
- `~s`: substitutes a literal string
- `~w`: substitutes a word derived from an atom name
- TODO: Add more!

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

## Special Characters

| Characters    | Meaning                     |
| ------------- | --------------------------- |
| `:-`          | if; used to define rules    |
| `,`           | logical and                 |
| `;`           | logical or                  |
| `not`         | logical not                 |
| `?-`          | begins a query              |
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
For example, infix operators like `+` that are typically
written like `a + b` can instead be written as `+(a, b)`.

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

Prolog supports the following relational operators for strings:

| Operator | Meaning                              |
| -------- | ------------------------------------ |
| `@<`     | alphabetically less than             |
| `@=<`    | alphabetically less than or equal    |
| `@>`     | alphabetically greater than          |
| `@>=`    | alphabetically greater than or equal |

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
  x(A, B) =@= x(C, D).

test(not_structurally_equivalent) :-
  x(A, B) \=@= x(C, D, E), % different arity
  x(A, B) \=@= y(C, D). % different functor name
```

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

Prolog supports the following bitwise operators:

Prolog supports the following additional operators:

| Operator | Meaning              |
| -------- | -------------------- |
| `/\`     | bitwise and          |
| `\/`     | bitwise or           |
| `xor`    | bitwise exclusive or |
| `\`      | bitwise not (prefix) |
| `<<`     | bitwise shift left   |
| `>>`     | bitwise shift right  |

TODO: Finish documenting the meaning of some of these operators.

| Operator | Meaning                                                                |
| -------- | ---------------------------------------------------------------------- |
| `-->`    | used in grammar rules for implementing parsers                         |
| `:-`     | appears between the head and body of every rule; read as "if"          |
| `?-`     | prefix operator that appears before every query                        |
| `\|`     | separates the head and tail of a list in `[H\| T]`                     |
| `;`      | separates clauses to be or'ed                                          |
| `,`      | separates clauses to be and'ed                                         |
| `->`     | similar to the ternary operator `?:` in other languages                |
| `\+`     | prefix operator that succeeds when the goal that follows does not hold |
| `=`      | attempts to unify LHS with RHS                                         |
| `\=`     | tests whether two terms cannot be unified                              |
| `=..`    | gets the functor and arguments of a clause; pronounced "univ"          |
| `is`     | attempts to unify LHS with RHS arithmetic expression result            |
| `>:<`    | partial unification between to dictionaries                            |
| `!`      | cut; prevents further backtracking                                     |
| `$`      | similar to `!` TODO How does it differ?                                |
| `*->`    | soft cut; rarely used                                                  |
| `:=`     | evaluates RHS as JavaScript (odd!)                                     |
| `:<`     | succeeds when LHS is a sub-dict of RHS dict                            |
| `:`      |                                                                        |
| `?`      |                                                                        |
| `\_`     |                                                                        |
| `/`      |                                                                        |
| `.`      |                                                                        |
| `as`     |                                                                        |
| `=>`     |                                                                        |

The `:-` prefix operator marks a directive to the Prolog system.
For example, `:- use_module(library(clpfd)).`

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

The `current_op` predicate queries operators.
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

## Strings

Literal strings can be delimited with single or double quotes.
SWI-Prolog prefers double quotes.
To escape a quote inside a literal string, precede it with a backslash.

A string is represented by a list of characters.

To get the length of a string, use the `atom_length` function.
For example:

```prolog
?- atom_length("Mark", X).
X = 4.
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

To get a single character from a string, convert it to a list of ASCII codes,
and use the `nth0` function.
For example:

```prolog
?- name("Mark", L), nth0(2, L, C), put(C). % 114 (ASCII code for 'r')
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
For example:

```prolog
sign_word(X, Y) :-
  (X = 0 -> Y = 'zero';
  (X > 0 -> Y = 'positive';
  Y = 'negative')).
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

Enter a query and press the return key after
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

## Constraint Logic Programming over Finite Domains: CLP(FD)

The library clpfd implmenents Constraint Logic Programming over Finite Domains.
This supports two primary use cases:

- declarative integer arithmetic
- combinatorial problems

This library supports a different, powerful way to write Prolog rules.

For example, the following rules describe the relationship
between a geometry shape and its area:

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

## Unit Tests

SCI-Prolog includes a unit testing framework called "Test Box".
See {% aTargetBlank
"https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)",
"Prolog Unit Tests" %}.

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

## Language Server

TODO: How can you install a Prolog language server in Neovim?
TODO: See https://github.com/jamesnvc/lsp_server.

TODO: Can you run Prolog code inside Neovim?

## Libraries

TODO: Is there a popular collection of open source Prolog libraries?
