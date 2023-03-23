---
eleventyNavigation:
  key: Lua
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.lua.org/", "Lua" %} is a
dynamically typed scripting language created in 1993
by a team at {% aTargetBlank "http://www.puc-rio.br",
"Pontifical Catholic University of Rio de Janeiro" %} in Brazil.

Lua is considered to be the fastest scripting language.
Compared to Python, Lua is simpler (only 21 keywords) and faster.

The Lua programming language was named after the Moon.

One reason Lua is popular is that it is easy to embed in a C/C++ application.
It is easy to run C code from Lua and run Lua code from C.

Lua has a small standard library, but has a
large collection of packages that can be installed using the
{% aTargetBlank "https://luarocks.org", "LuaRocks" %} package manager.

## Installing

Lua can be installed in macOS using Homebrew.
Enter `brew install lua` to install it
and enter `lua -v` to verify that it worked and see the version.

For installing in other operating systems, see
{% aTargetBlank "http://www.lua.org/start.html", "Getting Started" %}.

To experiment with Lua on the web without installing anything,
see {% aTargetBlank "http://www.lua.org/demo.html", "Lua Demo" %}.

## Resources

- {% aTargetBlank "https://www.lua.org/manual/", "Lua Reference Manual" %}
- {% aTargetBlank "https://www.amazon.com/exec/obidos/ASIN/8590379868/lua-pilindex-20", "Programming in Lua" %} book

## Cheat Sheets

- {% aTargetBlank "https://www.codecademy.com/learn/learn-lua/modules/learn-lua-introduction/cheatsheet", "codecademy" %}
- {% aTargetBlank "https://devhints.io/lua", "DevHints.io" %}
- {% aTargetBlank "https://cheatography.com/srgmc/cheat-sheets/lua-scripting-5-1/", "Cheatography" %}

## Source Files

Source files have a `.lua` extension.
To execute a source file, enter `lua filename.lua`.

A source file can use the variables and functions
defined in another source file by using the `require` function.
For example:

```lua
require "other"
```

## Keywords

Lua only defines 21 keywords.
Some of the keywords such as `end`, `and`, `or`, and `then`
make the syntax similar to the Ruby programming language.

TODO: List all the keywords with a brief explanation of each.

## Comments

Single-line comments begin with two dashes.
Multiline comments are delimited by `--[[` and `--]]`.

## Types

Lua uses dynamic types.
The types of variables and function parameters
are never specified and are always inferred.

## Variables

Variables are global by default.
Use the `local` keyword to make scoped to their environment.

## Strings

Strings can be delimited with either single or double quotes.

## Functions

Functions are defined with the `function` keyword.

For example:

```lua
function add(n1, n2)
  return n1 + n2
end

print(add(2, 3)) -- 5
```

Functions in Lua are first-class.
This means they can take other functions as arguments
and can return functions.

## Data Structures

Lua only provides one data structure called a "table".
It can represent arrays, dictionaries, trees, and graphs.

Lua does not support defining classes.
Instead it uses a combination of tables and functions for everything.

### Tables

A table is an associative array.
Values for keys are defined integer values by default starting from 1.
Tables are indexed starting from 1 instead of 0.

To iterate over keys and values, use a `for` loop with the `pairs` function.

## Standard Library

- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.1", "basic" %}.
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.2", "coroutine" %}
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.10", "debug" %}
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.8", "io" %}
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.7", "math" %}
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.3", "modules/package" %}
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.4", "string" %}
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.6", "table" %}
- {% aTargetBlank "https://www.lua.org/manual/5.4/manual.html#6.5", "utf8" %}

## Multitasking

Lua supports collaborative multitasking with coroutines.

```lua
co = coroutine.create(function ()
…
coroutine.yield(“intermediate value”)
…
return “final value”
end)

v1 = coroutine.resume(co)
v2 = coroutine.resume(co)
v3 = coroutine.resume(co) — error
```
