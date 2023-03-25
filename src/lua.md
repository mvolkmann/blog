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

- {% aTargetBlank "http://www.legoengineering.com/platform/nxt/", "Lego Mindstorms NXT" %}
- {% aTargetBlank "https://www.minecraft.net/", "Minecraft" %} game
- {% aTargetBlank "https://neovim.io", "Neovim" %} text editor
- {% aTargetBlank "https://redis.io", "Redis" %} database
- {% aTargetBlank "https://www.roblox.com", "Roblox" %} game
- {% aTargetBlank "https://worldofwarcraft.blizzard.com/", "World of Warcraft" %}

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

## Compiling

Lua programs can be compiled to bytecode before runtime.
This enables faster program startup because
runtime bytecode generation is no longer needed.

There are multiple ways to produce and execute Lua bytecode.

### luac

{% aTargetBlank "https://www.lua.org/manual/5.1/luac.html", "luac" %}
is the standard Lua compiler.

By default `luac` creates the file `luac.out`
Use the `-o` option to change this.

To generate a bytecode file, enter a command like
`luac demo.lua -o demo.luac`.

To execute a bytecode file, enter a command like
`lua demo.luac`.

### LuaJIT

{% aTargetBlank "https://luajit.org/", "LuaJIT" %} is an alternative
to `luac` that produces smaller bytecode files.
LuaJIT also executes bytecode files and provides runtime optimizations
that typically result in better performance.

From {% aTargetBlank "https://api7.ai/learning-center/openresty/luajit-vs-lua",
"api7.ai" %}:

> The LuaJIT runtime environment ... has a JIT compiler
> that can generate machine code directly.
> The LuaJIT interpreter records some runtime statistics while executing the
> bytecode, such as the actual number of times each Lua function call entry
> is run and the actual number of times each Lua loop is executed.
> When these counts exceed a random threshold,
> the corresponding Lua function entry or Lua loop is considered hot enough
> to trigger the JIT compiler to start working.
> The JIT compiler tries to compile the corresponding Lua code path,
> starting from the hot function's entry or the hot loop's location.
> The compilation process converts the LuaJIT bytecode into LuaJIT's own
> defined Intermediate Representation (IR) and
> then generates machine code for the target architecture.

To install LuaJIT:

- Download the source from {% aTargetBlank "", "" %}
  by entering `git clone https://luajit.org/git/luajit.git`.
- Enter `cd luajit`
- In macOS, enter `export MACOSX_DEPLOYMENT_TARGET={version}`
  where `version` is a value like `13.2`.
- Enter `make && sudo make install`
- The previous command will ask you to create a symlink with a command like
  `ln -sf luajit-2.1.0-beta3 /usr/local/bin/luajit`.
  Enter that command.

To generate a bytecode file from a Lua program,
enter a command like `luajit -b demo.lua demo.out`
To execute a bytecode file, enter a command like `luajit demo.out`.
This will start faster than entering `luajit demo.lua`.

## VS Code Setup

VS Code has great support for Lua.

The {% aTargetBlank "https://github.com/LuaLS/lua-language-server", "Lua" %}
extension from sumneko is a Lua language server.
It provides code annotations, syntax checking, dynamic type checking,
code formatting, spell checking, and more.

To disable specific diagnostics,
open Settings, filter on "Lua", scroll down to "Lua > Diagnostics: Disable",
click the "Add Item" button, select a diagnostic name from the dropdown,
and click the "OK" button.
For example, you may wish to disable "lowercase-global".

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

The {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=actboy168.lua-debug",
"Lua Debug" %} VS Code extension supports
debugging Lua code with breakpoints and watches.

## Source Files

Source files have a `.lua` extension.
To execute a source file, enter `lua filename.lua`.

A source file can use the variables and functions
defined in another source file by using the `require` function.
For example:

```lua
require "other"
```

## Running Code

To start a Lua REPL, enter `lua` and then enter Lua statements.
To exit the REPL, press ctrl-c or ctrl-d.

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

Primitive type conversions are supported by
the `tonumber` and `tostring` functions.
There are no builtin functions for converting
a boolean to a number, a number to a boolean, or a string to a boolean.

The following table shows how to convert each primitive type
to the other primitive types.

| From        | To Boolean    | To Number      | To String     |
| ----------- | ------------- | -------------- | ------------- |
| boolean `b` | not needed    | `b and 1 or 0` | `tostring(b)` |
| number `n`  | `n == 0`      | not needed     | `tostring(n)` |
| string `s`  | `s == "true"` | `tonumber(s)`  | not needed    |

The conversion from a number to a boolean above
assumes that all numbers except zero should be treated as false.

The `tonumber` function returns `nil` if the conversion is not possible.

## Input/Output

To write to stdout, use the `print` function or the`io.write` function.
Both take any number of arguments.

The `print` function adds a tab character between each value
and a newline at the end of its output.
It can be called with no arguments to only write a newline character.

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
io.write("First Number: ")
-- This form of `io.read` returns a number or nil if a non-number is entered.
n1 = io.read("*number")

io.write("Second Number: ")
n2 = io.read("*number")

if n1 and n2 then
  sum = n1 + n2
  print("Sum: " .. sum)
else
  print("An invalid number was entered.")
end
```

## Variables

Variable names can contain letters, digits, and underscores,
but cannot start with a digit. Hyphens are not allowed.
In multi-word names the words can be separated by underscores (preferred)
or written in camel-case.
For example, `one_long_name` or `oneLongName`.

To assign a value to a variable, use the `=` operator.

Variables are global by default.
Use the `local` keyword to make them only exist in their scope.

For example:

```lua
a = 1 -- global, even if assigned inside a function
local b = 2 -- local to the current scope
```

TODO: What does `_G.name = value` do?

Variables with no assigned value have the value `nil`.

New values of any type can be assigned to a variable at any time.

The `type(someVar)` function returns a string containing
the type name of the variable value.
This can be `nil`, `boolean`, `number`, `string`,
`table`, `function`, or `thread`.

Multi-variable assignment is supported.
For example:

```lua
local a, b, c = 1, 2, 3
a, b = b, a -- swaps values
```

## Booleans

Boolean literal values are `true` and `false`.
In conditions the only values treated as false are `false` and `nil`.
The number zero, an empty string, and an empty table are all treated as true.

## Numbers

TODO: Are all numbers represented by double-precision floating point?

## Strings

The string type is used single and multiple character text.
Lua does not have a dedicated type for single characters.

Strings can be delimited with either single or double quotes.
For example, `'Hello World!'` or `"Hello World!"`.

Escaping certain characters by preceding them with a backslash
changes how they are interpreted.
For example, `\n` produces a newline character,
`\t` produces a tab character,
`\"` produces a double quote inside a string delimited by double quotes, and
`\'` produces a single quote inside a string delimited by single quotes, and
`\\` produces a backslash character.

Multiline strings are delimiting with `[[` and `]]`.
For example:

```lua
haiku = [[
Out of memory.
We wish to hold the sky.
But we never will.
]]
```

Newlines and indentation inside the square brackets are retained.

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
- `-` subtraction or negation
- `*` multiplication
- `/` division
- `%` modulo
- `^` exponentiation

The `++` and `--` operators found in many other programming languages
are not supported.

Lua supports the following relational operators:

- `==` equal
- `~=` not equal (differs from other languages that use `!=`)
- `<` less than
- `>` greater than
- `<=` less than or equal to
- `>=` greater than or equal to

Lua supports the following logical operators:

- `and`
- `or`
- `not`

Lua only supports one assignment operator which is `=`.
It does not support shorthand assignment operators like `+=`.
Adding a number to a variable must be done with `myVar = myVar + n`.

The operator `..` is used to concatenate strings.

Lua operators have the following precedence from highest to lowest:

- `^`
- `not` and unary `-`
- `+` and binary `-`
- `..`
- `<`, `>`, `<=`, `>=`, `==`, and `~=`
- `and`
- `or`

Parentheses can be used to change the
evaluation order of parts of a long expression.
For example, `(a + b) / (c - d)` is
evaluated differently than `a + b / c - d`.

All operators are left associative except for
`^` and `..` which are right associative.
For example, `2 ^ 2 ^ 3` is the same as `2 ^ 8` and not `4 ^ 3`.

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
-- The loop variable, `i` in this case, is local to the loop
-- and cannot be accessed outside it.
for i = 1, 10, 2 do
  ...
end

for key, value in pairs(mytable) do
  ...
end

for index, value in ipairs(mytable) do
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
This is longest keyword in Lua.
It's too bad the designers didn't choose a shorter keyword
like `func`, `fun`, or `fn`.

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

There are two ways to call functions defined in a module/library.

```lua
-- Approach #1
library_name.function_name(target_value, arg1, arg2)

-- Approach #2
-- This implicitly passes `target_value` as the first argument.
-- Does this only work when the type of `target_value`
-- matches `library_name`?
target_value:function_name(arg1, arg2)
```

## Utility Functions

The following functions are helpful for debugging.
They can be defined in a file like `utility.lua`
and required where needed.

```lua
-- Returns a string description of the keys and values in a table.
-- Values can be nested tables.
function dump(value)
  if type(value) ~= "table" then
    return tostring(value)
  end

  local s = "{ "
  for k, v in pairs(value) do
    if type(k) ~= "number" then
      k = "\"" .. k .. "\""
    end
    s = s .. k .. "=" .. dump(v) .. ", " -- recursive
  end
  return s .. "} "
end

-- Returns a string containing all the values in a table,
-- each separated by a comma and a space.
-- Values cannot be nested tables.
function valuesString(obj)
  if type(obj) ~= "table" then
    return ""
  end

  s = ""
  for index, v in ipairs(obj) do
    s = s .. v .. ", "
  end
  return s:sub(1, -3)
end
```

## Data Structures

Lua only provides one data structure called a "table".
It is used to represent arrays, dictionaries, trees, and graphs.

Lua does not support defining classes.
Instead it uses a combination of tables and functions for everything.

### Tables

A Lua table is an associative array.

To create a table, surround key/value pairs in curly braces.
Each key and value are separated by `=`.
Non-string keys must be encloses in square brackets.

For example:

```lua
fill_later = {} -- creates an empty table

scores = {Mark = 7, Tami = 9} -- uses string keys

-- For string keys that include special characters such as spaces,
-- enclose them in square brackets and quotes.
full_scores = {["Mark Volkmann"] = 7, Tami = 9} -- uses string keys

days_in_month = {[1]=31, [2]=28, [3]=31, [4]=30} -- uses number keys

-- This has the same result as the previous line,
-- but values are specified without keys.
-- Number keys are provided starting from 1.
days_in_month = {31, 28, 31, 30}

words = {[true]="yes", [false]="no"} -- uses boolean keys
```

To get the value corresponding to a table key, use square brackets
as follows:

```lua
print("Tami's score is " .. scores["Tami"]) -- 9

-- When a key is not found, the value returned is `nil`.
print("Mary's score is " .. (scores["Mary"] or "unknown")) -- unknown

print("April has " .. days_in_month[4] .. " days.") -- 30
```

To iterate over keys and values, use a `for` loop
with the `pairs` or `ipairs` function.
For example:

```lua
for name, score in pairs(scores) do
  print(name, score) -- Mark 7 and Tami 9 in random order
end

for index, score in ipairs(scores) do
  print(index, score) -- 1 7 and 2 9
end
```

To add a key/value pair to a table:

To insert a key/value pair into a table that uses
consecutive integer keys starting from one
so the keys past the insertion point move up by one:

```lua
names = {"Mark", "Tami"}
table.insert(names, 2, "Comet") -- expensive for large tables
table.insert(names, "Bob") -- pushes onto end
for k, v in pairs(names) do print(v) end -- Mark Comet Tami Bob
```

To remove a key/value pair from a table:

```lua
-- For tables with consecutive integer keys starting from one
-- where you want the remaining pairs to move down ...
table.remove(my_table, index) -- expensive for large tables
last_value = table.remove(my_table) -- pops the last value and returns it

-- For all other cases ...
scores["Mark"] = nil
```

To create a string from a table by concatenating all of its values
using a given delimiter, use the `concat` function.
This only works for tables that have consecutive integer keys starting from one
and values that are strings or numbers.
For example:

```lua
t = {"Mark", 7, "Tami", 9}
print(table.concat(t, ", ")) -- "Mark, 7, Tami, 9"
```

Tables do not store the number of key/value pairs that they contain.
To get the count it is necessary to iterate over the table and count the pairs.
In tables that contain consecutive integer keys starting from one,
`#table_var` returns the length.
The "Lua Functional" library described later defines a `length` function
for computing the size of a table, but it has O(n) complexity.

Table values can be other tables.
This supports creating the equivalent of multi-dimensional arrays.
For example:

```lua
my2d = {}
my2d[i] = {}
my2D[i][j] = "some-value"
print(my2D[i][j]) -- "some-value"
```

## Lua Functional (luafun)

The <a href="https://www.lua.org/manual/5.4/manual.html#6.6"
target="_blank">table</a> library does not include
functions such as `map`, `filter`, and `reduce`.
These can be found in the {% aTargetBlank "https://luafun.github.io/",
"Lua Functional Library" %}.

This library is distributed in the single file `fun.lua`.
To download the file, enter
`wget https://raw.github.com/luafun/luafun/master/fun.lua`.

The following code demonstrates basic use of this library:

```lua
fun = require("fun")

scores = {7, 4, 13}

-- The `map` and `filter` methods returns an iterator.
-- Calling `:totable()` on an iterator returns a table.

-- Using `map` to double numbers in a table.
function double(n) return n * 2 end
doubled_iter = fun.map(double, scores)
print("Doubled Scores")
fun.each(print, doubled_iter) -- 14 8 26

-- Using `filter` to get odd numbers from a table.
function odd(n) return n % 2 == 1 end
odd_iter = fun.filter(odd, scores)
print("Odd Scores")
fun.each(print, odd_iter) -- 7 3

-- Using `reduce` to sum numbers in a table.
function add(a, b) return a + b end
total = fun.reduce(add, 0, scores)
print("Total is " .. total) -- 24

-- Doing the same with the `sum` function.
print("Total is " .. fun.sum(scores)) -- 24

-- There are MANY more functions in the luafun library!

-- Can you use a for loop to iterate over values in an interator?
-- for k, v in pairs(doubled_scores) do
for k, v in doubled_iter:unwrap() do
  print(v) -- 14 8 26
end
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

## File I/O

Files can be opened in one of the following modes
where the columns indicate the capabilities of each mode:

| Mode | New | Overwrite | Read | Write | Append |
| ---- | --- | --------- | ---- | ----- | ------ |
| `r`  |     |           | X    |       |        |
| `w`  | X   | X         |      | X     |        |
| `a`  | X   |           |      |       | X      |
| `r+` |     |           | X    | X     |        |
| `w+` | X   | X         | X    | X     |        |
| `a+` | X   |           | X    |       | X      |

To open a file:

```lua
file = io.open(file_path, mode) -- mode defaults to "r"
```

To write to a file:

```lua
file:write(data)
```

To read the entire contents of a file:

```lua
data = file:read("*a")
```

To seek to a specific byte offset:

```lua
file:seek("set", offset)
```

TODO: How do you read a given number of bytes from the current offset?

To close a file:

```lua
file:close()
```

## Modules

A module is a collection of variables and functions.

To define a module named `my_module`, create a file
named `my_module.lua` containing code like the following:

```lua
local my_module = {} -- a table

my_module.some_variable = "some value"

function my_module.some_function(p1, p2)
  print("some_function was passed " .. p1 .. " and " .. p2)
end

return my_module
```

To use this module:

```lua
mm = require("my_module")
mm.some_function(1, 2)
print(mm.some_variable)
```

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

learn about metamethods and metatables.

summarize the constants and functions in the math library and all The Standard, Libraries in markdown tables.

how does the math random function know whether you want a floating point or an integer value?

can a function that takes an arbitrary number of arguments specify names for the initial arguments?

do the math library trigonometry functions expect degrees or radians?

does the: syntax for calling a function only work when the variable has a type that maps to a Standard Library like String and number and table and function?

what are all the formatting types that string.format supports? Like %s, %i, and %f.

string.find returns the start and end indexes inclusive

string.gsub returns a new string, and the number of occurrences that were replaced.

Add a table of Lua operators that list them in precedence order from highest to lowest.
can you set a variable to a condition to get a Boolean value?
while variable and function names can use camel case, the Lua community seems to prefer using underscores.
