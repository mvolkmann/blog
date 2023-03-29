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
- limited support for error handling (see the `error` and `pcall` functions)
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
TODO: Learn more about Roblox programming.

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

Variables can be set to a condition to get a boolean value.
For example, `is_bigger = my_score > your_score`
sets `is_bigger` to `true` or `false`.

## Numbers

Numbers in Lua are all double precision floating point numbers.

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

  This returns a new string and the number of occurrences that were replaced.

- find start and end index inclusive:
  `string.find(source, target)` or `source:find(target)`
- uppercase: `string.upper(var)` or `var:upper()`
- lowercase: `string.lower(var)` or `var:lower()`
- regular expression matches:
  `string.match(someString, 'regular-expression')` returns a table of matches?

Lua does not support string interpolation.
The closest Lua feature to this is the `string.format` function.
It takes a format string as its first argument.
This can contain literal text to be output and
formatting directives that begin with a percent sign.
The supported formatting directives are:

- `%o` for octal numbers
- `%x` for hexadecimal numbers
- `%d` for decimal numbers
- `%f` for floating point numbers
- `%s` for strings

The `%f` directive can specify the number of decimal places to output.
For example, to output two decimal places use `%.2f`.

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
-- Use a `for` loop to iterate over a range of numbers with a given step size.
-- The loop variable, `i` in this case, is local to the loop
-- and cannot be accessed outside it.
for i = 1, 10, 2 do
  ...
end

-- Use a `for` loop to iterate over the key/value pairs in a table.
for key, value in pairs(mytable) do
  ...
end

-- Use a `for` loop to iterate over the indexes and values pairs
-- in a table whose keys are consecutive integers starting at 1.
for index, value in ipairs(mytable) do
  ...
end

-- Use a `while` loop for a top-tested loop
-- that stops when a given condition is no longer true.
while condition do
  ...
end

-- Use a `repeat`/`until` loop for a bottom-tested loop
-- that stops when a given condition becomes true.
-- The body is guaranteed to run at least once.
repeat — bottom-tested
  ...
until condition
```

Loops can use the `break` keyword to exit, but the `continue` keyword
for advancing to the next iteration is not supported.

## Functions

Functions are defined with the `function` keyword.
This is longest keyword in Lua.
It's too bad the designers didn't choose a shorter keyword
like `func`, `fun`, or `fn`.

Parameters are specified in parentheses after the function name
and are separated by commas.

For example:

```lua
function add(n1, n2)
  return n1 + n2
end

print(add(2, 3)) -- 5
```

Primitive parameters are passed by value
and tables are passed by reference.
For example:

```lua
function foo(b, n, s, t)
  b = true
  n = 2
  s = "yes"
  t.x = 2
end

b = false
n = 1
s = "no"
t = {x = 1}
foo(b, n, s, t)
print(b, n, s, t.x) -- false 1 no 2
```

Functions in Lua are first-class.
This means they can take other functions as arguments
and can return functions.

Functions can return multiple values.
For example:

```lua
function getStooges()
  return "Moe", "Larry", "Curly"
end

s1, s2, s3 = getStooges()
print(s1, s2, s3) -- Moe Larry Curly
```

Functions can take a variable number of arguments.
For example:

```lua
function add(...)
  local sum = 0
  local args = {...} -- creates a table containing the arguments
  for _, n in ipairs(args) do
    sum = sum + n
  end
  return sum
end

print("The sum is " .. add(1, 2, 3) .. ".") -- 6

function report(name, age, ...)
  local count = select("#", ...)
  local text = "%s is %d years old and likes %d things.\nThey are"
  local s = string.format(text, name, age, count)
  local things = {...}
  for index, thing in ipairs(things) do
    local prefix = " "
    if index == 1 then
      prefix = " "
    elseif index > 1 and index < count then
      prefix = ", "
    else
      prefix = ", and "
    end
    s = s .. prefix .. thing
  end
  return s .. "."
end

print(report("Mark", 61, "running", "biking", "programming"))
-- Mark is 61 years old and likes 3 things.
-- They are running, biking, and programming.
```

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
TODO: I'm still confused about this usage!
target_value:function_name(arg1, arg2)
```

## Default Parameter Values

Lua does not provide an explicit way to define
default values for function parameters.
A workaround is to implement a function to have table parameter and
specify default values in that table using the `setmetatable` function.

For example:

```lua
function volume(t)
  -- This supplies default values for missing keys in the table t.
  setmetatable(t, {__index={width=1, height=1, depth=1}})
  return t.width * t.height * t.depth
end

print(volume({width=2, height=3, depth=4})) -- 24
print(volume({width=2, height=3})) -- 6
print(volume({width=2, depth=4})) -- 8
print(volume({})) -- 1
```

It's debatable whether this is better than the following approach
that achieves the same result without using a metatable:

```lua
function volume(t)
  local w = t.width or 1
  local h = t.height or 1
  local d = t.depth or 1
  return w * h * d
end
```

The `volume` function could also be implemented as follows:

```lua
function volume(t)
  return (t.width or 1) * (t.height or 1) * (t.depth or 1)
end
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
Their two primary purposes are be either array-like or dictionary-like,
although those uses can be combined.

To create a table that is array-like,
provide a comma-separated values inside curly braces.
For example:

```lua
scores = {5, 2, 7}

names = {"Mark", "Tami"}
```

To create a table that is dictionary-like,
provide keys and values inside curly braces.
String keys only require quotes if
they contain special characters such as spaces.
String keys in quotes and non-string keys must be enclosed in square brackets.
String values must be enclosed in quotes.
Each key is followed by `=` and its value.
For example:

```lua
scores = {Mark=5, Tami=7, ["Unknown Player"]=3} -- uses string keys

words = {[true]="yes", [false]="no"} -- uses boolean keys

months = {[1]="January", [2]="February"} -- uses integer keys

-- This has the same result as the previous line,
-- but values are specified without keys.
-- Integer keys are provided starting from 1.
months = {"January", "February"}
```

The keys and values in tables can be any kind of value including other tables.

An empty table can be created and filled later.
For example:

```lua
fill_later = {} -- creates an empty table

fill_later[1] = "Hello"
fill_later["condition"] = "sunny"
```

To get the value corresponding to a table key, use square brackets
as follows:

```lua
print("Tami's score is " .. scores["Tami"]) -- 7

-- When a key is not found, the value returned is `nil`.
print("Mary's score is " .. (scores["Mary"] or "unknown")) -- unknown

print("The second month is " .. months[2] .. ".") -- February
```

To iterate over keys and values, use a `for` loop
with the `pairs` or `ipairs` function.

The `pairs` function visits all the key/value pairs, but not
necessarily in the order in which they were added to the table.

The `ipairs` function is intended for iterating over array-like tables.
It only visits the consecutive integer keys starting with `1`.
As soon as it fails to find the next integer key, it stops.

The code below demonstrates these functions and
also shows that a table can be both array-like AND dictionary-like.

```lua
-- The entries `5` and `3` are array-like.
-- `5` is assigned the key `1` because it is the first array-like entry.
-- `3` is assigned the key `2` because it is the second array-like entry.
-- The entries `foo=2` and `bar=4` are dictionary-like.
t = {5, foo=2, 3, bar=4}

print("keys and values")
for k, v in pairs(t) do
  print(k, v)
end
-- 1       5
-- 2       3
-- bar     4
-- foo     2

print("indexes and values")
for i, v in ipairs(t) do
  print(i, v)
end
-- 1       5
-- 2       3
```

The `table.insert` function inserts an array-like entry so the
array-like entries past the insertion point move up by one.
This can be expensive if a large number of entries need to be moved.
For example:

```lua
names = {"Mark", "Tami"}
table.insert(names, 2, "Comet") -- Mark Comet Tami
table.insert(names, "Bob") -- pushes onto end; Mark Comet Tami Bob
```

The `table.remove` function removes an array-like entry
so the array-like entries past the removal point move down by one.
This can be expensive if a large number of entries need to be moved.
For example:

```lua
names = {"Mark", "Comet", "Tami", "Bob"}
table.remove(names, 2) -- changes to Mark Tami Bob and returns Comet
-- Omitting the index pops the last entry.
last_name = table.remove(names) -- changes to Mark Tami and returns Bob
```

To remove a dictionary-like entry, set its value to `nil`.
For example:

```lua
scores["Mark"] = nil
```

The `table.concat` function returns a string created by
concatenating all of its array-like values using a given delimiter.
This only includes values for consecutive integer keys starting from one.
For example:

```lua
t = {"Mark", 7, "Tami", 9}
print(table.concat(t, " and ")) -- "Mark and 7 and Tami and 9"
t = {5, foo=2, 3, bar=4}
print(table.concat(t, " and ")) -- 5 and 3
```

The `table.sort` function sorts an array-like table in place.
For example:

```lua
names = {"Tami", "Mark", "Amanda", "Jeremy"}

-- When no comparator function is specified,
-- the values are compared using the `<` operator.
table.sort(names)
print(table.concat(names, ", ")) -- Amanda, Jeremy, Mark, Tami

-- This specifies a comparator function.
-- Values with the same length are sorted using the `<` operator.
-- When values lengths differ, shorter lengths come first.
table.sort(names, function (a, b)
  if #a == #b then
    return a < b
  else
    return #a < #b
  end
end)
print(table.concat(names, ", ")) -- Mark, Tami, Amanda, Jeremy
```

To get the length of an array-like table, use `#my_table`.
The returns the highest consecutive integer index starting from `1`.

Dictionary-like tables do not store their number of entries.
To get the count it is necessary to iterate over the table and count them.
The "Lua Functional" library described later defines a `length` function
for computing the size of a table, but it has O(n) complexity.

The `table.pack` function provides another way to create an array-like table
that adds the key `n` which holds the number of entries.
The value of the `n` is not updated when entries are inserted or removed.
For example:

```lua
t = table.pack("Mark", 7, "Tami", 9)
print(t.n) -- 4
```

The `table.unpack` function returns consecutive array-like entries
with keys starting from `1`.
For example:

```lua
names = {"Mark", "Tami", "Comet"}
father, mother, dog = table.unpack(names)
print(father, mother, dog) -- Mark    Tami    Comet
```

Table values can be other tables.
This supports creating the equivalent of multi-dimensional arrays.
For example:

```lua
my2d = {}
my2d[i] = {}
my2D[i][j] = "some-value"
print(my2D[i][j]) -- "some-value"

points = {
  {x=2, y=3},
  {x=5, y=1},
  {x=3, y=7}
}
print(points[2].x) -- 5
```

When passing a single table to a function,
it is not necessary to include parentheses.
For example:

```lua
function dump_table(t)
  for k, v in pairs(t) do
    print(k, v)
  end
end

dump_table({foo=1, bar=2})
dump_table{foo=1, bar=2} -- same as previous line
```

The `table.move` function copies entries from one array-like table to another.
It does not remove entries from the source table.
Existing destination table entries at target indexes are replaced.

For example:

```lua
function dump(t)
  for _, v in ipairs(t) do
    print("  " .. v)
  end
end

t1 = {1, 2, 3}
t2 = {4, 5, 6}
-- Copy table t1 elements 1 to 3 into table t2 starting at 4.
-- The arguments are source table, start index, end index,
-- destination index, and destination table.
table.move(t1, 1, 3, 3, t2)
dump(t1) -- 1 2 3
dump(t2) -- 4 5 1 2 3; existing value 6 was replaced with 1
```

## self Parameter

Tables can have entries where the key is a function name
and the value is a function.
These can be defined inside a table literal or outside.

There are two syntaxes for defining table functions outside a table literal:

- `MyTable.my_function(p1, p2)` referred to below as a "dot function"
- `MyTable:my_function(p1, p2)` referred to below as a "colon function"

Dot functions are somewhat like object-oriented class methods.
They should always be called with a dot, not a colon.
For example, `MyTable.my_function(v1, v2)`.

Colon functions are like object-oriented instance methods.
They are supplied with a hidden first parameter named `self`,
whereas dot functions are not.

Colon functions should always be called with a colon, not a dot.
For example, `MyTable:my_function(v1, v2)`.

The following code demonstrates defining and calling
dot functions and colon functions.

```lua
MyTable = {
  dotInner = function(p1, p2)
    print("dotInner p1 =", p1)
    print("dotInner p2 =", p2)
  end
}

MyTable.dotInner(1, 2) -- p1 = 1, p2 = 2

-- This is an alternate way to write the dotInner function,
-- defining it outside the table literal.
-- The `self` variable is not defined in dot functions.
function MyTable.dotOuter(p1, p2)
  print("dotInner p1 =", p1)
  print("dotInner p2 =", p2)
end

MyTable.dotOuter(1, 2) -- p1 = 1, p2 = 2

-- Here dotOuter is called with a colon.
-- Calling any function using a colon causes the variable
-- before the colon to be passed as the first argument.
-- This makes 1 the second argument and 2 the third.
-- Dot function should never be called like this.
MyTable:dotOuter(1, 2) -- p1 = MyTable, p2 = 1, last argument 2 is ignored

-- Colon functions have an invisible first parameter named "self".
-- So p1 here is actually the second parameter and p2 is the third.
-- When a colon function is called using a colon,
-- the variable before the colon is passed as the first argument.
-- It seems colon functions cannot be defined inside a table literal.
function MyTable:colonOuter(p1, p2)
  print("colonOuter self =", self)
  print("colonOuterFn p1 =", p1)
  print("colorOuterFn p2 =", p2)
end

-- Here colonOuter is called without a colon,
-- so MyTable is not passed as the first argument.
-- Colon functions should never be called like this.
MyTable.colonOuter(1, 2, 3) -- self = 1, p1 = 2, p2 = 3

-- Here colonOuter is called with a colon,
-- so MyTable is passed as the first argument.
-- Colon functions should always be called like this.
MyTable:colonOuter(1, 2) -- self = MyTable, p1 = 1, p2 = 2

-- This is equivalent to the previous line.
-- While this works, it is overly verbose.
MyTable.colonOuter(MyTable, 1, 2) -- self = MyTable, p1 = 1, p2 = 2
```

### Metatables

A metatable is a table that defines metamethods.
All metamethods have names that begin with two underscores.
Examples include `__add` which defines how the `+` operator works and
`__index` which defines the value that should be returned
when an attempt is made to access a missing key in a table.

By default tables do not have a metatable.
A metatable only becomes useful when it is assigned to a table.
This is done with the `setmetatable(table, metatable)` function.
This returns its first argument which is useful when a literal table is passed.
The same metatable can be assigned to multiple tables.
The `getmetatable(table)` function returns the metatable that has been
assigned to a given table or `nil` if one has not be assigned.

An important use of metatables is simulating the
object-oriented programming concepts of classes and inheritance.

The `__index` method can be implemented in two ways.
It can be a table that supplies default values for missing properties
or it can be a function that is passed the ? table and a key.

TODO: Get examples from metatables.lua!

TODO: Add much more here!

## Metamethods

TODO: See your metamethods.lua file.

Metamethods enable defining the functionality of Lua operators.
TODO: Verify that all of these work. Maybe \_\_le isn't supported.

| Metamethod | Operator    |
| ---------- | ----------- |
| `__add`    | `+`         |
| `__sub`    | `-`         |
| `__mul`    | `*`         |
| `__div`    | `/`         |
| `__mod`    | `%`         |
| `__pow`    | `^`         |
| `__concat` | `..`        |
| `__len`    | `#`         |
| `__eq`     | `==`        |
| `__ne`     | `~=`        |
| `__lt`     | `<`         |
| `__le`     | `<=`        |
| `__gt`     | `>`         |
| `__ge`     | `>=`        |
| `__unm`    | `-` (unary) |

Metamethods also support implementing functions
that are called when specific things occur.

| Metamethod    | Operator                                        |
| ------------- | ----------------------------------------------- |
| `__call`      | called when the table is called like a function |
| `__gc`        | called after garbage collection runs            |
| `__index`     | called if a key is not found in the table       |
| `__metatable` | prevents changes to metatable; see below        |
| `__mode`      | returns a string; see below                     |
| `__newindex`  | called when an entry is added to the table      |
| `__tostring`  | returns a string representation                 |

When the `__metatable` function is defined,
the metatable cannot be modified and this returns an error message.

When the `__mode` function returns a string that contains `k`,
keys are weak. When it contains `v`, values are weak.
Both can be true.

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

### math module

Constants defined by this library include:

- `math.huge` - floating point value greater than any other number
- `math.maxinteger` - maximum integer value
- `math.mininteger` - minimum integer value
- `math.pi` - value of π

Trigonometry functions defined by this library include
`sin`, `cos`, `tan`, `asin`, `acos`, and `atan`.
All of these take and return angles in radians.
To convert an angle from degrees to radians, use the `math.rad(x)` function.
To convert an angle from radians to degrees, use the `math.deg(x)` function.

To generate random numbers, use the
`math.randomseed` and `math.random` functions.

- `math.randomseed(os.time())` seeds the random number generator
- `math.random()` returns a floating point number in the range [0, 1)
- `math.random(10)` returns an integer in the range [1, 10]
- `math.random(5, 10)` returns an integer in the range [5, 10]

Other functions defined in this library include:

- `math.abs(x)` for absolute value
- `math.ceil(x)` for ceiling (rounds up)
- `math.exp(x)` for e raised to the x power
- `math.floor(x)` for floor (rounds down)
- `math.fmod(x, y)` for floating point remainder of x / y
- `math.log(x, [base])` for logarithm of x with specified base or e
- `math.max(...)` for maximum of a set of numbers
- `math.min(...)` for minimum of a set of numbers
- `math.modf(x)` for integral and fractional parts of a floating point number

  For example, `math.modf(3.14)` returns `3` and `0.14`

- `math.sqrt(x)` for square root
- `math.tointeger(x)` returns an integer value if x
  can be converted to an integer; otherwise `nil`

  The value passed can be a number or string.
  For example, `math.tointeger(3.0)` and `math.tointeger("3.0")` returns `3`,
  and `math.tointeger(3.1)` returns `nil`.

- `math.type(x)` returns `"integer"`, `"float"`, or `nil`
- `math.ult(m, n)` returns `true` if m < n when
  compared as unsigned integers; otherwise `false`

## File I/O

The following code shows the most basic way to write to a new file.
If the file already exists, it is overwritten.

```lua
-- TODO: What does this do if file_path is omitted?
io.output(file_path)
io.write(some_string)
io.close()
```

The following code shows the most basic way to read from a file.

```lua
-- TODO: What does this do if file_path is omitted?
io.input(file_path)
-- TODO: What arguments does this accept and what do they do?
local data = io.read()
io.close()
```

Files can be opened in one of the following modes
where the columns indicate the capabilities of each mode:

| Mode | New | Overwrite | Read | Write | Append |
| ---- | :-: | :-------: | :--: | :---: | :----: |
| `r`  |     |           |  X   |       |        |
| `w`  |  X  |     X     |      |   X   |        |
| `a`  |  X  |           |      |       |   X    |
| `r+` |     |           |  X   |   X   |        |
| `w+` |  X  |     X     |  X   |   X   |        |
| `a+` |  X  |           |  X   |       |   X    |

To open a file:

```lua
file = io.open(file_path, mode) -- mode defaults to "r"
```

To write to a file:

```lua
file:write(data)
```

To read from a file:

```lua
contents = file:read("*all") -- reads the entire contents
line = file:read("*line") -- reads the next line
number = file:read("*number") -- reads a number
n1, n2 = file:read("*number", "*number") -- reads two numbers
text = file:read(n) -- reads a string of up to "n" characters
end_test = file:read(0) -- returns nil if at end of file; otherwise returns ""
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

## Object Oriented Programming (OOP)

While Lua does not support defining classes and creating instances,
it can simulate those using metatables.
The following code demonstrates defining
classes, properties, constructors, methods, and subclasses.

```lua
-- Define Animal class with properties `name` and `says`.
Animal = {name = "", says = ""}

-- Animal constructor
function Animal:new(kind, name, says)
  -- TODO: This must return a table, but it's not
  -- TODO: clear that it needs to have a metatable.
  -- `self` is a special variable name, not a keyword.
  setmetatable({}, self)

  -- Set properties.
  self.kind = kind
  self.name = name
  self.says = says

  -- Define methods.
  -- TODO: It seems each instance will have its own copy of every method.
  self.someMethod = function (p1, p2)
    -- Implement the method.
  end

  return self
end

-- Animal method
function Animal:toString()
  local text = "%s is a %s and says %s."
  return string.format(text, self.name, self.kind, self.says)
end

-- Create an Animal instance.
dog = Animal:new("dog", "Comet", "bark")

print(dog.says) -- access property; bark
print(dog:toString()) -- call method; Comet is a dog and says bark.

-- Define Giraffe class inheriting from the Animal class.
Giraffe = Animal:new()

-- Giraffe constuctor
function Giraffe:new(name, height)
  setmetatable({}, self)

  -- Set superclass properties.
  self.kind = "giraffe"
  self.name = name
  self.says = "nothing"

  -- Set properties unique to this class.
  self.height = height

  return self

  -- TODO: Maybe all the code above can be replaced by the following:
  --[[
  instance = Animal("giraffe", name, "nothing")
  instance.height = height
  return instance
  ]]
end

-- Giraffe method
function Giraffe:report()
  local text = "%s %s is %d feet tall."
  return string.format(text, self.name, self.kind, self.height)
end

-- Create Giraffe instance.
g = Giraffe:new("Geoffrey", 18) -- creates an instance

print(g.height) -- access property; 18
print(g:toString()) -- call method; Geoffrey is a giraffe and says nothing.
print(g:report()) -- call method; Geoffrey giraffe is 18 feet tall.
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

## Error Handling

Lua does not have a mechanism for throwing and catching exceptions.
In many cases errors result in a function returning the value `nil`
and it left to developers to check for `nil` values.
Failing to do so often results in programs crashing
and outputting a stack trace.

The only error handling mechanism Lua provides is the `pcall` function
(short for "protected call"), which can be compared
to a try block in other programming languages.
The `pcall` function is passed a function to execute and
optionally arguments to be passed to it.
It returns a boolean indicating whether the call completed without error
and an error message if one did occur.

The `error` function is the Lua equivalent of
a `throw` statement in other programming languages.
It is passed a message and an optional integer error level.
The message can be any type, but is typically a string or a table.

The following code demonstrates using the `pcall` function.
It repeated prompts for a dividend and a divisor
and displays their quotient.
When invalid values are entered, error messages are output,
but the program does not crash.

```lua
function read_number(prompt)
  io.write(prompt .. ": ")
  local number = io.read("*number")
  -- The previous line does not consume the newline character.
  -- Unless that is done, the next attempt to read a number will return `nil`.
  -- The following line consumes the newline character.
  local _ = io.read()
  return number
end

function process()
  local dividend = read_number("Enter a dividend")
  if not dividend then
    -- Error messages here are tables containing a message and a code.
    error({message = "dividend is invalid", code = 1})
  end

  local divisor = read_number("Enter a divisor")
  if not divisor then
    error({message = "divisor is invalid", code = 2})
  end
  if divisor == 0 then
    error({message = "cannot divide by zero", code = 3})
  end

  local quotient = dividend / divisor
  io.write(string.format("The quotient is %.3f\n\n", quotient))
end

while true do
  local success, err = pcall(process)
  if not success then
    if err then
      print(string.format("%s (code %d)", err.message, err.code))
    end
    -- print(debug.traceback()) -- prints a stack trace
    print() -- extra newline
  end
end
```

## Games

{% aTargetBlank "https://love2d.org", "LÖVE" %}
is a Lua framework for building 2D games.
It can be downloaded from the previous link.

For macOS, click the "64-bit zipped" link under "macOS" to download `love.app`.
Drag this file into the "Applications" directory.
Double-click `love.app` to launch the app.
This will fail the first time with the message
"love.app cannot be opened because the developer cannot be verified".
To fix this, open the Settings app, select "Privacy & Security",
scroll down to "love.app was blocked ..." and click the "Open Anyway" button.

Create a directory for a new game.
Create a file in this directory named "main.lua".

If using VS Code:

- Install the extension "Love2D Support" from Pixelbyte Studios.
- Click the gear icon and select "Extension Settings".
- Change "Pixelbyte > love2d: Path" to
  "/Applications/love.app/Contents/MacOS/love".
- Open a "main.lua" file in an editor tab.
- Press cmd-l to run the game.

TODO: See lua/love-game/main.lua.

## Unorganized Content

summarize the constants and functions in all the standard libraries

does the: syntax for calling a function only work when the variable has a type that maps to a Standard Library like String and number and table and function?

How do you work with dates and times in Lua?
Are there functions for formatting them?

Can precede function with local keyword to make it local to the current source file. Correct?

The ternary trick probably doesn’t work if the desired true value is false or nil.

Can assign a default value to a function parameter like this:

function foo(p1)
p1 = p1 or default_value
…
end

Functions don’t specify the type they return or even if they do return a value.
Functions must use the return keyword to return a value.

Are there recommended Lua linters and code formatters that run outside of VS Code?

What is the difference between these?
my_var = 1 — Is this only global within the current source file?
\_G.my_var = 1 — Does this make it accessible outside the current source file?

When getting multiple return values from a function, you can use underscore to act as a placeholder for values you don’t want. ex. pairs function when you don’t want the key or ipairs function when you don’t want the index.

Functions can call themselves recursively.

Did you describe the fact that there is no compact syntax for defining them?

Coroutine status values include running, suspended, normal, and dead (computed or stopped due to error).
coroutine.resume(my_coroutine) — starts or resumes a coroutine

Can a Lua program create multiple threads that run concurrently?
What happens if you call coroutine.resume on a coroutine that has a status of dead?

The things in the standard library are “modules”, not “libraries”.
io.read(…) returns nil if there is no more file content to read.
There are addition file mode values that include “b” to work with binary files.

Cover the os library.
os.time() returns ms since 1970 or.a given date/time
os.difftime() returns ms difference between two times
os.date()
os.getenv(“environment-variable-name”)
os.rename(current_file_path, new_file_path) ?
os.remove(current_file_path) — deletes the file
os.execute(shell_command)

To get the time it takes to run some code:
local start = os.clock()
— some code here
print(os.clock() - start)

os.exit(code) — code can be true (exits with EXIT_SUCCESS; default), false (exits with EXIT_FAILURE), or a number

A module source file returns a table containing variables and functions.

There are probably many variable assignments in the
examples here that should be preceded by `local`.

A Lua “package” is a collection of modules.

Modules in the standard library like io and table do not need to be imported to use them.

Cover the `self` special variable.

OO-like objects hold their properties and methods in a table.
Does each object get its own copy of every method?
