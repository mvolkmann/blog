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

The Lua programming language was named after the Moon.

Lua has an interpreter and a virtual machine.
The interpreter is written in C and
produces bytecode that runs in the virtual machine.
Compiling to bytecode can be done at runtime or ahead of time
(although I haven't found a recommended way to do it).

Lua has a relatively small standard library, but has a
large collection of packages that can be installed using the
{% aTargetBlank "https://luarocks.org", "LuaRocks" %} package manager.

Pros of Lua include:

- considered to be the fastest scripting language
- free and open sources under the MIT license
- simple syntax with only 21 keywords
- uses dynamic variables that do not require specifying types
- functions are first class and are closures
- implements tail call optimization
- easy to run C code from Lua and run Lua code from C
- easy to embed in C/C++ applications (interpreter is only 182K)
- highly portable (runs on all major OSes and most microcontrollers)
- native support for multitasking with coroutines
- provides automatic, incremental garbage collection

Cons of Lua include:

- limited Unicode support

  Strings can contain unicode characters,
  but indexing into such strings does not account for their size.
  See {% aTargetBlank "http://lua-users.org/wiki/LuaUnicode", "Lua Unicode" %}.

- variables and functions are global by default
- lack of type checking
- lack of support for object oriented programming (OOP),
  although it can be simulated with metatables and functions
- limited support for error handling
- limited support for regular expressions

TODO: Get more from http://notebook.kulchenko.com/programming/lua-good-different-bad-and-ugly-parts.

## Used By

Notable uses of Lua include:

- {% aTargetBlank "https://www.minecraft.net/", "Minecraft" %} game
- {% aTargetBlank "https://www.roblox.com", "Roblox" %} game
- {% aTargetBlank "http://www.legoengineering.com/platform/nxt/", "Lego Mindstorms NXT" %}
- {% aTargetBlank "https://neovim.io", "Neovim" %} text editor
- {% aTargetBlank "https://redis.io", "Redis" %} database

Roblox uses a variant of Lua called Luau.
TODO: How does that differ?

For an extensive, see the {% aTargetBlank
"https://en.wikipedia.org/wiki/List_of_applications_using_Lua",
"List of applications using Lua" %} Wikipedia page.

## Resources

- {% aTargetBlank "https://www.lua.org/manual/", "Lua Reference Manual" %}
- {% aTargetBlank "https://www.amazon.com/exec/obidos/ASIN/8590379868/lua-pilindex-20", "Programming in Lua" %} book

## Cheat Sheets

- {% aTargetBlank "https://www.codecademy.com/learn/learn-lua/modules/learn-lua-introduction/cheatsheet", "codecademy" %}
- {% aTargetBlank "https://devhints.io/lua", "DevHints.io" %}
- {% aTargetBlank "https://cheatography.com/srgmc/cheat-sheets/lua-scripting-5-1/", "Cheatography" %}

## Installing

Lua can be installed in macOS using Homebrew.
Enter `brew install lua` to install it.
Enter `lua -v` to verify that it worked and see the version.

For installing in other operating systems, see
{% aTargetBlank "http://www.lua.org/start.html", "Getting Started" %}.

To experiment with Lua on the web without installing anything,
see {% aTargetBlank "http://www.lua.org/demo.html", "Lua Demo" %}.

For faster performance, use {% aTargetBlank "https://luajit.org/", "LuaJIT" %}.
To install this:

- Download the source from {% aTargetBlank "", "" %}
  by entering `git clone https://luajit.org/git/luajit.git`.
- Enter `cd luajit`
- In macOS enter `export MACOSX_DEPLOYMENT_TARGET={version}`
  where `version` is a value like `13.2`.
- Enter `make && sudo make install`
- The previous command will ask you to create a symlink with a command like
  `ln -sf luajit-2.1.0-beta3 /usr/local/bin/luajit`.
  Enter that command.

To compile a Lua program to bytecode and save it,
enter a command like `luajit -b demo.lua demo.out`
To execute the saved bytecode, enter a command like `luajit demo.out`.
This will start faster than entering `luajit demo.lua`.

# VS Code Setup

VS Code has great support for Lua.

The {% aTargetBlank "https://github.com/LuaLS/lua-language-server", "Lua" %}
extension from sumneko is a Lua language server.
It provides code annotations, syntax checking, dynamic type checking,
code formatting, spell checking, and more.

The {% aTargetBlank "https://github.com/Koihik/vscode-lua-format",
"vscode-lua-format" %} extension from Koihik
is a popular Lua code formatting extension.
To configure this, open Settings, filter on "Lua", and enter the path
to a configuration file in "Vscode-lua-format: Config Path" such as
`/Users/{your-user}/Documents/dev/lang/lua/style.config`.
Then create the file `style.config` in that location
containing something like the following:

```text
# See all the Lua code formatting options at
# https://github.com/Koihik/LuaFormatter/blob/master/docs/Style-Config.md

column_limit: 80
continuation_indent_width: 2
indent_width: 2
keep_simple_control_block_one_line: true
keep_simple_function_one_line: true
single_quote_to_double_quote: true
use_tab: false
```

## Source Files

Source files have a `.lua` extension.
To execute a source file, enter `lua filename.lua`.

A source file can use the variables and functions
defined in another source file by using the `require` function.
For example:

```lua
require "other"
```

## Running Programs

To run a Lua program, enter `lua {filename}.lua` or `luajit {filename}.lua`.
This runs the source file through the Lua interpreter to produce
bytecode and uses the Lua virtual machine to execute the bytecode.

There are several tools that claim to
produce executable files from Lua programs.
But none seem to be popular or actively maintained.

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
Multiline comments begin with `--[[` and end with `--]]` or just `]]`.

## Types

Lua uses dynamic types.
The types of variables and function parameters
are never specified and are always inferred.

To convert a boolean or number value to a string,
pass it to the `tostring` function.

To convert a string value to a number,
pass it to the `tonumber` function.

To convert a string value to a boolean,
pass it to the `toboolean` function.

There are no builtin functions for converting
a boolean to a number or a number to a boolean.

## Input/Output

To write to stdout, use the `print` function or the`io.write` function.
Both take any number of arguments.

The `print` function adds a tab character between each value
and a newline at the end of its output

The `io.write` function does not add any characters between values
and does not add a newline at the end.
Boolean values must be converted to strings
before being passed to `io.write`.

For example:

```lua
print('Hello', true, 7) -- Hello   true    7
io.write('Hello', tostring(true), 7) -- Hellotrue7
```

To read from stdin, use the `io.read` function.

The following code prompts for two numbers and prints their sum.

```lua
io.write('First Number: ')
s1 = io.read()
n1 = tonumber(s1) -- returns nil if not a number

io.write('Second Number: ')
s2 = io.read()
n2 = tonumber(s2)

if n1 == nil then
  print('The first value "' .. s1 .. '" is not a number.')
elseif n2 == nil then
  print('The second value "' .. s2 .. '" is not a number.')
else
  sum = tonumber(n1) + tonumber(n2)
  print('Sum: ' .. sum)
end
```

## Variables

Variables are global by default.
Use the `local` keyword to make scoped to their environment.

Variables with no assigned value have the value `nil`.

New values of any type can be assigned to a variable at any time.

The `type(someVar)` function returns a string containing
the type name of the variable value.
This can be `nil`, `boolean`, `number`, `string`, `table`, or `function`.

Multi-variable assignment is supported.
For example:

````lua
a, b, c = 1, 2, 3
a, b = b, a -- swaps values

## Booleans

Boolean literal values are `true` and `false`.

## Numbers

TODO: Are all numbers represented by double-precision floating point?

## Strings

Strings can be delimited with either single or double quotes.
For example, `'Hello World!'` or `"Hello World!"`.

Multiline strings are delimiting with `[[` and `]]`.
For example:

```lua
haiku = [[
Out of memory.
We wish to hold the sky.
But we never will.
]]
````

Strings are indexed starting from 1 instead of 0.

Use the `..` operator to concatenate strings.
For example, `fullName = firstName .. ' ' .. lastName`.

String operations include:

- length: `string.len(var)` or `#var`
- substring: `string.sub(source, startIndex, endIndex)` or
  `source:sub(startIndex, endIndex)`
- global substitute: `string.gsub(source, oldValue, newValue)`
- find start and end index: `string.find(source, target)` or
  `source:find(target)`
- uppercase: `string.upper(var)` or `var:upper()`
- lowercase: `string.lower(var)` or `var:lower()`
- regular expression matches:
  `string.match(someString, 'regular-expression')` returns a table of matches?

Lua does not support string interpolation.
The closest Lua feature to this is the `string.format` function.
For example:

```lua
name = "Mark"
color = "yellow"
sentence = string.format("%s's favorite color is %s.", name, color)
```

## Operators

Lua supports the following mathematical operators:

- `+` addition
- `-` subtraction
- `*` multiplication
- `/` division
- `%` modulo

The ++ and — operators are not supported.
Lua supports the following relational operators:

- `==` equal
- `~=` not equal
- `<` less than
- `>` greater than
- `<=` less than or equal to
- `>=` greater than or equal to

Lua supports the following logical operators:

- `and`
- `or`
- `not`

Lua does not support shorthand assignment operators like +=.
Adding a number to a variable must be done with `myVar = myVar + n`.

## Conditional Logic

Parentheses are not required around conditions.

In an `if` statement, the `elseif` and `else` blocks shown below are optional.

All the parts of the `if` statement can be written on a single line if desired.

```lua
if condition then
  ...
elseif condition then
  ...
else
  ...
end
```

Lua does not have a `switch` statement or an equivalent.

Lua does not have a ternary operator, but the
same functionality can be achieved with the following:

```lua
let result = condition and trueValue or falseValue
```

## Iteration

```lua
for i = 1, 10, 2 do
  ...
end

for key, value in pairs(mytable) do
  ...
end

while condition do — top-tested
  ...
end


repeat — bottom-tested
  ...
until condition
```

Loops can use the `break` keyword,
but the `continue` keyword is not supported.

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

Functions can return multiple values.
For example:

````lua
function getStooges()
  return "Moe", "Larry", "Curly"
end

s1, s2, s3 = getStooges()
print(s1, s2, s3) -- Moe Larry Curly
```

Functions can take a variable number of arguments.
For example:

```lua
function myFn(...)
  for key, value in pairs(...) do
    ...
  end
end
````

Anonymous functions (unnamed) are closures and can be stored in variables.
For example:

```lua
product = function(n1, n2)
  return n1 * n2
end

print(product(2, 3)) -- 6
```

## Data Structures

Lua only provides one data structure called a "table".
It can represent arrays, dictionaries, trees, and graphs.

Lua does not support defining classes.
Instead it uses a combination of tables and functions for everything.

### Tables

A Lua table is an associative array.

Tables are indexed starting from 1 instead of 0.

Values for keys are defined integer values by default starting from 1.

The last element in all tables has the value `nil`.

To iterate over keys and values, use a `for` loop with the `pairs` function.

For example:

```lua
myTable = {}
myTable[index] = value -- indexes start at 1
print(myTable[index])
print('length =', #myTable)
table.insert(myTable, index, value)
table.concat(myTable, “, “) -- returns a string of concatenated table elements
table.remove(myTable, index)
my2D = []
my2D[i] = {}
my2D[i][j] = value
print(my2D[i][j])
```

The <a href="https://www.lua.org/manual/5.4/manual.html#6.6"
target="_blank">table</a> library does not include
functions such as `map`, `filter`, and `reduce`.
These can be found in the {% aTargetBlank "https://luafun.github.io/",
"Lua Functional Library" %}.

To download the file `fun.lua`,
enter `wget https://raw.github.com/luafun/luafun/master/fun.lua`.

To use this library,
To install this, enter `luarocks install luafun`.

The following code demonstrates uses this library:

```lua
fun = require('fun')
```

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

## math module

Random numbers:

- `math.randomseed(os.time())` seeds the random number generator
- `math.random()` returns a number in the range [0, 1)
- `math.random(10)` returns a number in the range [0, 10)
- `math.random(5, 10)` returns a nubmer in the range [5, 10)
- `print(string.format('pi is %.2f', math.pi)` outputs `pi is 3.14`

Trigonometry functions:

- `math.sin(angle)`
- `math.cos(angle)`
- `math.tan(angle)`

- TODO: Add more!

## Modules

A module is a collection of variables and functions

## Multitasking

Lua supports collaborative multitasking with coroutines.
Coroutines are like threads, but they do not run in parallel.

```lua
-- TODO: Can this function have parameters?
co = coroutine.create(function ()
  ...
  coroutine.yield("value #1")
  ...
  coroutine.yield("value #1")
  ...
  return "value #3"
end)

v1 = coroutine.resume(co) -- "value #1"
print(coroutine.status(co)) -- "running"
v2 = coroutine.resume(co) -- "value #2"
v3 = coroutine.resume(co) -- "value #3"
v4 = coroutine.resume(co) — error
print(coroutine.status(co)) -- "suspended"
```

## Unorganized Content

when calling functions of a module, can you separate the module name from the function name with either a period or a colon?

are there linters in code formatters for Lua?

learn about metamethods and metatables.
