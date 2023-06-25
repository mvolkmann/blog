---
eleventyNavigation:
  key: Prolog
layout: topic-layout.njk
---

## Overview

## Installing

There are many implementations of the Prolog programming language.
The most popular seems to be {% aTargetBlank
"https://www.swi-prolog.org", "SWI-Prolog" %}.

<img alt="SWI-Prolog" style="width: 20%"
    src="/blog/assets/swipl-logo.png?v={{pkg.version}}"
    title="SWI-Prolog">

> SWI-Prolog offers a comprehensive free Prolog environment.
> Since its start in 1987, SWI-Prolog development has been
> driven by the needs of real world applications.
> SWI-Prolog is widely used in research and education
> as well as commercial applications."

To install the terminal command `swipl` in macOS,
enter `brew install swi-prolog`

To install a stable, binary version of SWI-Prolog app, browse {% aTargetBlank
"https://www.swi-prolog.org/download/stable",
"Download SWI-Prolog stable version" %}.

On macOS, double-click the downloaded `.dmg` file.
This opens a Finder window containing several files and directories.
Drag the file `SWI-Prolog.app` to the `Applications` directory.

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
| fact           | description of something that is true                          |
| rule           | description of a relationship about at least one unknown thing |
| question/query | asks if something is true or asks for a matching value         |
| database       | a collection of facts and rules                                |

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
?: fast(comet).

% This is a query that asks for something that is fast.
% It returns "comet".
?: fast(X).
```

To stop searching for things that are fast, press the return key.

To search for something else that is fast,
enter a semicolon and press the return key.
After the last match is found, the prompt for another query will appear.

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

To load a `.pl` file, enter `[file-name].`
For example, to load the file `demo.pl`, `enter [demo].`

After modifying source files that have already been loaded,
enter `make.` to reload all of them.

To enter new facts and rules in a running session:

- Enter `[user].` to open stdin as a pseudo file.
- Enter facts and rules.
- Press ctrl-d to close and load the pseudo file.

It seems this can replace existing facts rather than add to them.

Another way to add a fact or rule is to enter
`assertz(fact-or-rule).`

To remove a fact or rule, enter `retract(fact-or-rule).`
TODO: I get "ERROR: No permission to modify static procedure".

## Special Characters

| Characters    | Meaning                     |
|---------------|-----------------------------|
| `:-`          | if; used to define rules    |
| `,`           | logical and                 |
| `;`           | logical or                  |
| `not`         | logical not                 |
| `?:`          | begins a query              |
| `.`           | terminates all commands     |
| `%`           | begins single-line comment  |
| `/*` and `*/` | delimits multi-line comment |

## Case-sensitive

Terms that begin with a lowercase letter represent objects or relationships.

Terms that begin with an uppercase letter represent variables.

## Calling From Other Languages

SWI-Prolog can be called from C. See {% aTargetBlank
"https://www.swi-prolog.org/pldoc/man?section=calling-prolog-from-c",
"Calling Prolog from C" %}.

TODO: Which other programming languages can call SWI-Prolog?

## Language Server

TODO: How can you install a Prolog language server in Neovim?
TODO: See https://github.com/jamesnvc/lsp_server.
