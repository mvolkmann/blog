---
eleventyNavigation:
  key: Prolog
layout: topic-layout.njk
---

## Overview

Prolog is a logic-based programming language.
The name is a contraction of "programming in logic".
Prolog has many uses including artificial intelligence,
abstract problem solving, symbolic equation solving, and more.
A core feature is search with backtracking.

Prolog first appeared in 1972. It was designed by three computer scientists,
Alain Colmerauer (France), Phillipe Roussel (France), and
Robert Kowalski (USA/Britan).

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

## Installing

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

SWI-Prolog is implemented in a combination of C (48%) and Prolog (39%).

To install the terminal command `swipl` in macOS,
enter `brew install swi-prolog`

To install a stable, binary version of SWI-Prolog app, browse {% aTargetBlank
"https://www.swi-prolog.org/download/stable",
"Download SWI-Prolog stable version" %}.

On macOS, double-click the downloaded `.dmg` file.
This opens a Finder window containing several files and directories.
Drag the file `SWI-Prolog.app` to the `Applications` directory.

To run SWI-Prolog from a terminal, enter `swipl`.

### GNU Prolog

<img alt="GNU Prolog logo" style="width: 20%"
  src="/blog/assets/gnu-prolog-logo.png?v={{pkg.version}}"
  title="GNU Prolog logo">

GNU Prolog is implemented in a combination of C (84%) and Prolog (15%).

To install the terminal command `gprolog` in macOS,
enter `brew install gnu-prolog`.

To run GNU Prolog from a terminal, enter `gprolog`.

## Online REPL

To enter and run Prolog code in a web browser, browse
{% aTargetBlank "https://swish.swi-prolog.org", "SWISH" %}.

Enter facts and rules in the left pane.
Enter a query in the lower-right pane.
Press the "Run!" button or ctrl-return to execute the query.

## Running

Double-click the SWI-Prolog app to open a window
where Prolog commands can be entered.
Initially macOS will not open the app because it is deemed untrusted.
To make it trusted, open the "System Settings" app, select "Privacy & Security",
and allowing opening `SWI-Prolog.app`.

## Exiting

To exit the SWI-Prolog interpreter, enter `halt.` or press ctrl-d.

## Terminology

| Term           | Meaning                                                        |
|----------------|----------------------------------------------------------------|
| atom           |                                                                |
| fact           | description of something that is true                          |
| functor        | Is this just another term for "predicate"?                     |
| predicate      | name of a rule; appears in head before left parenthesis        |
| rule           | description of a relationship about at least one unknown thing |
| question/query | asks if something is true or asks for a matching value         |
| database       | a collection of facts and rules                                |

A rule can be thought of as a special kind of fact
that depends on a set of facts.

## Basic Syntax

Prolog programs are composed of facts, rules, and queries.

For example:

```prolog
% These are facts that say comet is a whippet and spots is a cheetah.
whippet(comet).
cheetah(spots).

% This is a rule that says if something is a whippet then it is fast.
fast(X) :- whippet(X).
fast(X) :- cheetah(X).
% The previous two lines can be replaced with this.
% fast(X) :- cheetah(X); whippet(X).

% This is a query that asks whether comet is fast.
% It returns "true".
?- fast(comet).

% This is a query that asks for something that is fast.
% It returns "comet".
?- fast(X).

% This rule ... TODO
```

A fact is a degenerate case of a rule whose body only contains `true`.
For example, the following are equivalent:

```prolog
likes(mark, ice-cream).
likes(mark, ice-cream) :- true.
```

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

## Typical Flow

The typical steps to run a Prolog program are:

1. Add facts and rules to a Prolog source file that has an extension of `.pl`
1. Load Prolog source files into the Prolog app.
1. Enter queries in the Prolog app.

Unfortunately Prolog and Perl use the same file extension
for their source files.

To load a `.pl` file, enter `[file-name].` or `consult('file-name').`
For example, to load the file `demo.pl`, `enter [demo].`

After modifying source files that have already been loaded,
enter `make.` to reload all of them.

To enter new facts and rules in a running session:

- Enter `[user].` to open stdin as a pseudo file.
- Enter facts and rules.
- Press ctrl-d to close and load the pseudo file.

It seems this can replace existing facts rather than add to them.

## Dynamic Predicates

By default predicates cannot be added or deleted in a session.
To enable this, run a `dynamic` query.
Once this is done, the `assertz` function can be used to add a predicate
and the `retract` function can be used to remove one.

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
?- assertz(likes(mark, reeces)).
?- retract(likes(mark, books)).
?- likes(mark, X). % outputs running and reeces
```

## Input

The `read` function reads a string from stdin.
For example:

```prolog
greet :-
  write("Enter your name: "),
  read(Name),
  format("Hello, ~w!", [Name]).

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

readFile("demo.txt").
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
format('~w likes ~w.', [mark, "Prolog"]).
```

The following ? can be used in format strings:

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

writeFile("demo.txt", "first line\nsecond line").
```

## Special Characters

| Characters    | Meaning                     |
|---------------|-----------------------------|
| `:-`          | if; used to define rules    |
| `,`           | logical and                 |
| `;`           | logical or                  |
| `not`         | logical not                 |
| `?-`          | begins a query              |
| `.`           | terminates all commands     |
| `%`           | begins single-line comment  |
| `/*` and `*/` | delimits multi-line comment |

## Naming Conventions

Terms that begin with a lowercase letter represent objects or relationships.

Terms that begin with an uppercase letter represent variables.

TODO: Summarize valid names for predicates, atoms, and variables.

An underscore represents an anonymous variable.
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
To escape a quote inside a literal string, precede it with a backslash.

A string is represented by a list of characters.

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

TODO: Can you append and insert values in list? 
TODO: Can you remove values from a list? 

## Rules

Rules can write to the current output stream.
For example:

```prolog
greet(Name) :- format('Hello, ~w!', [Name]).

greet("Mark")
Hello, Mark!
```

Rules do not return values like a function,
but then can set the values of their arguments.

For example:

```prolog
area(circle, Radius, X) :- X is pi * Radius^2.
area(square, Side, X) :- X is Side^2.
area(rectangle, Width, Height, X) :- X is Width * Height.

?- area(circle, 2, X).
X = 12.566370614359172.
```

Each of the `area` rules uses the "is" keyword
to assign a value to the X argument.

Rules can be recursive.
See the `sum` example above.

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

## Listing

To list all the facts and rules known in the current session,
enter `listing.`.  The output will contain many clauses created by the system in addition to those you loaded.

To list only the facts and rules for a given predicate,
enter `listing(predicate-name).`

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
"https://www.metalevel.at/prolog/efficiency", "lEfficiency of Prolog" %}.

## Language Server

TODO: How can you install a Prolog language server in Neovim?
TODO: See https://github.com/jamesnvc/lsp_server.

TODO: Can you run Prolog code inside Neovim?

## Libraries

TODO: Is there a popular collection of open source Prolog libraries?

