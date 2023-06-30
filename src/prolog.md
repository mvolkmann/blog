---
eleventyNavigation:
  key: Prolog
layout: topic-layout.njk
---

## Overview

Prolog is a logic-based programming language.
The name is a contraction of "programming in logic".

Prolog uses a declarative syntax rather than a procedural one.
It is a homoiconic language which means its code can be treated as data.

A core feature of Prolog is pattern matching search with backtracking,
also referred to as "unification".
This is the process of searching a set of facts and rules (called a database)
to find values that match a given predicate.
Prolog is highly optimized to handle searching large databases.

Prolog has many uses including artificial intelligence,
abstract problem solving, symbolic equation solving, and more.

Prolog first appeared in 1972. It was designed by three computer scientists,
Alain Colmerauer (France), Phillipe Roussel (France), and
Robert Kowalski (USA/Britain).

"IBM Watson is a question-answering computer system
capable of answering questions posed in natural language."
It is partially implemented in Prolog.

The Prolog unification process relies on the properties of {% aTargetBlank
"https://en.wikipedia.org/wiki/Horn_clause", "Horn clauses" %}.

## Resources

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
| goal              | a question                                                      |
| list notation     | comma-separated terms inside square brackets; ex. `[a, B, 7]`   |
| operator notation | terms separated by operators; ex. `Y = m\*X + b`                |
| unification       | process of searching for variable values that satisfy a rule    |
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

A rule is written as a head and a body separated by
the "if" symbol `:-` and terminated by a period.

The head is a functor (atom) followed by
an argument list that is surrounded by parentheses.
The head syntax is similar to that of a fact,
but its argument list can contain variables.

The body is a comma (means "and") or semicolon (means "or")
separated list of goals.

The following rules state that if something is
a whippet or a cheetah then it is fast.

```prolog
fast(X) :- whippet(X).
fast(X) :- cheetah(X).

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
% This states that siblings must have the same father and the same mother.
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

Rules do not return values like a function,
but they can set the values of their arguments.

For example:

```prolog
area(circle, Radius, X) :- X is pi * Radius^2.
area(square, Side, X) :- X is Side^2.
area(rectangle, Width, Height, X) :- X is Width * Height.

?- area(circle, 2, X).
X = 12.566370614359172.
```

TODO: See the version of the area rule that uses clpr in geometry.pl.

Each of the `area` rules uses the `is` keyword
to assign a value to the X argument.

Rules can be recursive.
See the `sum` example above.

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

## Typical Flow

To evaluate a query in an interactive session,
enter a query terminated with a period.
If the query does not contain any variables
then `true` or `false` will be output.
If the query does contain variables,
the first set of values that satisfy the query will be output.
To see the next set, press the semicolon key.

To evaluate operators that result in value,
assign the expression to a variable.
For example, entering `X = 1 + 2.` will output `X = 3.`

The typical steps to run a Prolog program are:

1. Add facts and rules to a Prolog source file that has an extension of `.pl`
1. Load Prolog source files into the Prolog app.
1. Enter queries in the Prolog app.

Unfortunately Prolog and Perl use the same file extension
for their source files.

To load a `.pl` file, enter `[file-name].` or `consult('file-name').`
For example, to load the file `demo.pl`, `enter [demo].`
Alternatively, pass a source file to the interpreter when starting it.
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
In this example, whippet and comet are the components of the structured.
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

A list can be written as a comma-separated set of terms
surrounded by square brackets.
For example, `[red, green, blue]` is a list of atoms
and `[R, G, B]` is a list of variables that can be
unified with any list containing exactly three elements.

An empty list is written as `[]` which is called "nil".

There are also other ways to construct a list.
The dot function is the list constructor.
It is passed the head and the tail of the list to be constructed.
For example, `.(E, Es)` where `E` is a single element that is the head
and `Es` is a list of elements in the tail.
By convention, variable names that end in "s" represent lists.

A string is represented by a list of atoms
that correspond to the characters in the string.

The following are all equivalent ways to write the same list:

```prolog
[red, green, blue] % list notation
[red | [green | [blue | []]]]
.(red, .(green, .(blue, []))) % functional notation
```

The `|` operator can be used to get the head and tail of a list.
For example:

```prolog
print_list_parts(L) :-
  [H|T] = L,
  format('head is ~w, tail is ~w', [H, T]).

?- print_list_parts([red, green, blue]).
% head is red, tail is [green,blue]
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
% red
% green
% blue
```

The built-in predicate `append` appends two lists to create a new list.

If this were not built-in, it could be implemented as follows:

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
Each operator has left, right, or no associativity.

Operators can be used in function form.
For example, `a + b` can be written as `+(a, b)`.

Prolog supports the following relational operators:

| Operator | Meaning                              |
| -------- | ------------------------------------ |
| `=:=`    | equal value                          |
| `=\=`    | not equal value                      |
| `<`      | less than                            |
| `>`      | greater than                         |
| `=<`     | less than or equal                   |
| `>=`     | greater than or equal                |
| `@<`     | alphabetically less than             |
| `@=<`    | alphabetically less than or equal    |
| `@>`     | alphabetically greater than          |
| `@>=`    | alphabetically greater than or equal |
| `==`     | identical terms                      |
| `\==`    | not identical terms                  |
| `=@=`    | structurally equivalent terms        |
| `\=@=`   | not structurally equivalent terms    |

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

Prolog supports the following math operators:

| Operator | Meaning                       |
| -------- | ----------------------------- |
| `+`      | addition                      |
| `-`      | subtraction                   |
| `*`      | multiplication                |
| `/`      | floating point division       |
| `//`     | integer division              |
| `div`    | integer division              |
| `rem`    | remainder of integer division |
| `rdiv`   | rational number division      |
| `mod`    | modulo                        |
| `**`     | exponentiation                |
| `^`      | exponentiation                |

Prolog supports the following additional operators:

TODO: Finish documenting the meaning of some of these operators.

| Operator | Meaning                                                       |
| -------- | ------------------------------------------------------------- |
| `-->`    | used in grammar rules for implementing parsers                |
| `:-`     | appears between the head and body of every rule; read as "if" |
| `?-`     | appears before every query                                    |
| `\|`     | separates the head and tail of a list in `[H\| T]`            |
| `;`      | separates clauses to be or'ed                                 |
| `,`      | separates clauses to be and'ed                                |
| `->`     |                                                               |
| `*->`    | soft cut; rarely used                                         |
| `:=`     |                                                               |
| `\+`     | negates the value of the expression that follows              |
| `=`      | tests whether two terms can be unified                        |
| `\=`     | tests whether two terms cannot be unified                     |
| `=..`    | gets the functor and arguments of a clause; pronounced "univ" |
| `is`     | assigns right value to variable on left                       |
| `>:<`    | partial unification between to dictionaries                   |
| `:<`     |                                                               |
| `:`      |                                                               |
| `\\`     |                                                               |
| `xor`    | bitwise exclusive or                                          |
| `?`      |                                                               |
| `\_`     |                                                               |
| `/`      |                                                               |
| `<<`     | bitwise shift left                                            |
| `>>`     | bitwise shift right                                           |
| `.`      |                                                               |
| `!`      | cut; prevents further backtracking                            |
| `$`      |                                                               |
| `as`     |                                                               |
| `=>`     |                                                               |

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
There are two required parts, characteristics and implementation.

For example:

```prolog
% Priority is a number between 0 and 1200 where 0 is the highest.
% Type is prefix, infix, or postfix.
% Name is the characters to be used.
:- op( 0, fx, [ dbl ]).
dbl(N) :- N * 2.
```

TODO: The above does not work! Find out why.

## Strings

Literal strings can be delimited with single or double quotes.
Single quotes seem to be preferred.
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

TODO: Add this.

## Iteration

Iteration in Prolog is done with recursion.

To get all the integers starting from one integer and ending at another,
use the `between` function.
For example:

```prolog
% This sets V to 3, 4, 5, 6, and 7.
?- between(3, 7, V).
```

## Lists

Lists are sequential collections of values.
They are created by including a comma-separated list of values
in square brackets.

To create a new list that results from adding a value
to the beginning of an existing list, use the pipe operator.
For example:

```prolog
?- L is [2, 3], [1 | L].

```

The following rule computes the sum of numbers in a list:

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
    % Note the use of the "is" keyword to assign a value to the Sum argument.
    Sum is TailSum + Head.

?- sum([1, 2, 3], X).
X = 6.
```

To get the length of a list, use the `length` rule.
For example:

```prolog
?- length([2, 5, 7], L).
L = 3.
```

TODO: Show an example where a list is destructured in the argument list using `[H|T]`

Anonymous variables (`_`) can be used to destructure value from a list.
For example, the following gets the first and third values.
The `| _` syntax at the end of the list on the left side
indicates that we do not care about values in the tail of the list
which includes all values after the third.

```prolog
?- [V1, _, V3 | _] = [9, 8, 7, 6, 5].
V1 = 9,
V3 = 7.
```

To test whether a value is a member of a list, use the `member` function.
For example:

```prolog
TODO: WHY DOESN'T THIS WORK?
TODO: How can you set a variable and then use it in a subsequest predicate?
MyList = [3, 7, 9].
member(7, MyList). % true
member(4, MyList). % false
```

The `member` function can also be used to iterate over the values in a list.
For example, `member(X, [3, 7, 9])` will set `X`
to each value in the list one at a time.

The `reverse` function creates a new list containing
all the values in a given list in reverse order.
For example:

```prolog
?- reverse([1, 2, 3], X). % X = [3, 2, 1]
```

The `append` function creates a new list by appending two existing lists.
For example:

```prolog
?- append([1, 2, 3], [4, 5], X). % X = [1, 2, 3, 4, 5]
```

Lists can be nested. For example:

```prolog
[a, [b, c], d, e, [f, g, h]]
```

TODO: Can you append and insert values in list?
TODO: Can you remove values from a list?

For implementations of map, filter, and reduce, see {% aTargetBlank
"https://pbrown.me/blog/functional-prolog-map-filter-and-reduce/",
"Functional Prolog" %}.

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

## Listing

To list all the facts and rules known in the current session,
enter `listing.`. The output will contain many clauses created by the system in addition to those you loaded.

To list only the clauses (facts and rules) for a given predicate,
enter `listing(predicate-name).`
This will list all matching clauses regardless of arity.

## Debugging

To see all the steps used to evaluate a predicate,
turn on trace mode by entering `trace.`

Enter a query and press the return key after
viewing the result of each step in the evaluation.

When finished debugging, enter `notrace.` to turn this mode off.

## Structures

TODO: Add this detail.

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

For example, the following rule computes the area of various geometry shapes.

````prolog

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
````

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
