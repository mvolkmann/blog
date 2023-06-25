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

> SWI-Prolog offers a comprehensive free Prolog environment.
> Since its start in 1987, SWI-Prolog development has been
> driven by the needs of real world applications.
> SWI-Prolog is widely used in research and education
> as well as commercial applications."

To install a stable, binary version of SWI-Prolog, browse {% aTargetBlank
"https://www.swi-prolog.org/download/stable",
"Download SWI-Prolog stable version" %}.

On macOS, double-click the downloaded `.dmg` file.
This opens a Finder window containing several files and directories.
Drag the file `SWI-Prolog.app` to the `Applications` directory.

## Basics

Prolog programs are composed of facts, rules, and queries.

The following are examples of facts:

```prolog
whippet(comet).
fast(whippet).
```

The following is an example of a rule:

```prolog
fast(X) :- whippet(X).
```

The following is an example of a query:

```prolog
fast(comet).
```

## Running

Double-click the SWI-Prolog app to open a window
where Prolog commands can be entered.
Initially macOS will not open the app because it is deemed untrusted.
To make it trusted, open the "System Settings" app, select "Privacy & Security",
and allowing opening `SWI-Prolog.app`.

The steps to run a Prolog program are:

1. Add facts and rules to a Prolog source file that has an extension of `.pl`
1. Load Prolog source files into the Prolog app.
1. Enter queries in the Prolog app.

## Online REPL

To enter and run Prolog code in a web browser, browse
{% aTargetBlank "https://swish.swi-prolog.org", "SWISH" %}.

Enter facts and rules in the left pane.
Enter a query in the lower-right pane.
Press the "Run!" button or ctrl-return to execute the query.

## Calling From Other Languages

TODO: Which programming languages can invoke Prolog code?
