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

Lua has a relatively small standard library, but has a
large collection of packages that can be installed using the
{% aTargetBlank "https://luarocks.org", "LuaRocks" %} package manager.

## Where Used

Notable uses of Lua include:

- {% aTargetBlank "https://www.minecraft.net/", "Minecraft" %} game
- {% aTargetBlank "https://www.roblox.com", "Roblox" %} game
- {% aTargetBlank "http://www.legoengineering.com/platform/nxt/", "Lego Mindstorms NXT" %}
- {% aTargetBlank "https://neovim.io", "Neovim" %} text editor
- {% aTargetBlank "https://redis.io", "Redis" %} database

For an extensive, see the {% aTargetBlank
"https://en.wikipedia.org/wiki/List_of_applications_using_Lua",
"List of applications using Lua" %} Wikipedia page.

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

Boolean literal values:

- true
- false

Conditional Logic:

- if
- then
- elseif
- else

Functions:

- function
- return

Iteration:

- for: loop over numbers or pairs
- in: specifies what to loop over
- while: top-tested loop
- repeat and until: bottom-tested loop
- break: exit loop early

Logical Operators:

- and
- or
- not

Variables:

- local

Other:

- do: begins a new lexical block
- end: marks the end of a lexical block
- nil: represents having no value

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

- <a href="https://www.lua.org/manual/5.4/manual.html#6.1" target="_blank">basic</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.2" target="_blank">coroutine</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.10" target="_blank">debug</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.8" target="_blank">io</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.7" target="_blank">math</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.3" target="_blank">modules/package</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.9" target="_blank">os</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.4" target="_blank">string</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.6" target="_blank">table</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.5" target="_blank">utf8</a>

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
