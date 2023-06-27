---
eleventyNavigation:
  key: Prolog
layout: topic-layout.njk
---

## Overview

Prolog is a logic-based programming language.
The name is a contraction of "programming in logic".
It uses a declarative syntax rather than a procedural one.

A core feature of Prolog is pattern matching search with backtracking,
also referred to as "unification".
This is the process of searching a set of facts and rules
to find values that match a given ???.

Prolog has many uses including artificial intelligence,
abstract problem solving, symbolic equation solving, and more.

Prolog first appeared in 1972. It was designed by three computer scientists,
Alain Colmerauer (France), Phillipe Roussel (France), and
Robert Kowalski (USA/Britain).

"IBM Watson is a question-answering computer system
capable of answering questions posed in natural language."
It is partially implemented in Prolog.

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

| Term            | Meaning                                                |
| --------------- | ------------------------------------------------------ |
| term            | the only datatype; has four subtypes listed below      |
| - number        | integer or floating point                              |
| - atom          | an identifier that represents a specific thing         |
| - variable      | represents a value to be determined                    |
| - compound term | specific combination of terms; more detail below       |
| fact            | description of something that is true                  |
| rule            | relationship involving one or more unknown thing       |
| predicate       | a fact or rule                                         |
| clause          | another name for a predicate                           |
| query           | asks if a clause is true or asks for satisfying values |
| database        | collection of predicates                               |
| functor         | name of a predicate                                    |
| unification     | process of searching for values that satisfy a rule    |

A compound term is a functor followed by an argument list.
Each argument is an atom, a variable, or a destructuring of variables.

A rule can be thought of as a special kind of fact
that depends on a set of other facts.

## Syntax

Prolog programs are composed of facts, rules, and queries.

When a query has multiple matches, the system will wait for further input.
To search for the next match, press the semicolon key.
After the last match is found, the prompt for another query will appear.

To stop searching for things that are fast, press the return key.

Suppose the following facts are loaded:

```prolog
likes(mark, tacos).
likes(mark, books).
likes(mark, running).
```

The query `likes(mark, X)` will find
tacos, books, and running that in that order.
The query `likes(X, running)` will find mark.

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

## Conjunctions

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

Prolog supports the following relational operators:

| Operator | Meaning               |
| -------- | --------------------- |
| `=:=`    | equal                 |
| `=\=`    | not equal             |
| `<`      | less than             |
| `>`      | greater than          |
| `=<`     | less than or equal    |
| `>=`     | greater than or equal |

The syntax for "equal" and "not equal" is quite odd!

The odd syntax for "less than or equal" was
chosen so it doesn't look like an arrow.

Prolog supports the following math operators:

| Operator | Meaning                 |
| -------- | ----------------------- |
| `+`      | addition                |
| `-`      | subtraction             |
| `*`      | multiplication          |
| `/`      | floating point division |
| `//`     | integer division        |
| `**`     | exponentiation          |
| `^`      | exponentiation          |
| `mod`    | modulo                  |

One way to evaluate a mathematical expression is to assign it to a variable.
For example, we can compute the angle in degrees
whose `sin` is `0.5` as follows:

```prolog
% The asin function returns an angle in radians.
?- Angle is asin(0.5) * 180 / pi.
Angle = 29.999999999999996.
```

After evaluating this, the variable `Angle` is no longer defined.

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
X = 'AB
```
