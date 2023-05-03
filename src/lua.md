---
eleventyNavigation:
  key: Lua
layout: topic-layout.njk
---

## Overview

<img alt="Lua logo" style="width: 40%"
    src="/blog/assets/lua-logo.svg?v={{pkg.version}}"
    title="Lua logo">

{% aTargetBlank "https://www.lua.org/", "Lua" %} is a
dynamically typed scripting language created in 1993
by a team at {% aTargetBlank "http://www.puc-rio.br",
"Pontifical Catholic University of Rio de Janeiro" %} in Brazil.
The team members include {% aTargetBlank
"https://en.wikipedia.org/wiki/Roberto_Ierusalimschy",
"Roberto Ierusalimschy" %},
{% aTargetBlank "https://web.tecgraf.puc-rio.br/~celes/", "Walemar Celes" %},
and {% aTargetBlank "https://lhf.impa.br/", "Luiz Henrique de Figueiredo" %}.

The Portuguese word "Lua" means "Moon".
The Lua logo depicts the Moon orbiting around the Earth
and its shadow being cast onto the Earth.

The goals of Lua are:

- simplicity: easy to use by non-professional programmers
- small size: supports embedding in non-Lua applications
- scripting: easy to invoke from system languages such as C
- portability: runs on any OS targeted by the ANSI C compiler

Examples of Lua simplicity include having a small number of keywords (22)
and indexing from 1 instead 0 (like matrices in math).
The Lua reference manual that describes every language feature
is only around 100 pages.

Standard Lua has an interpreter and a virtual machine.
The interpreter is written in C and
produces bytecode that runs in the virtual machine.
Compiling to bytecode can be done at runtime or ahead of time

Lua has a relatively small standard library, but a
large collection of optional packages can be installed using the
{% aTargetBlank "https://luarocks.org", "LuaRocks" %} package manager.

Pros of Lua include:

- simple syntax with only 22 keywords
- easy to embed in C/C++ applications
  (interpreter is ~250K and standard libraries are ~ 500K)
- easy to run C code from Lua and run Lua code from C
- highly portable; runs on all major OSes and most microcontrollers
- free and open source under the MIT license
- considered fastest scripting language when compiled
- uses dynamic variables that do not require specifying types
- provides automatic, incremental garbage collection
- functions are first class and are closures
- implements tail call optimization
- supports collaborative multitasking with coroutines

Cons of Lua include:

- lack of type checking
- lack of direct support for object-oriented programming (OOP),
  although it can be simulated with metatables and functions
- limited support for error handling;
  see `pcall`, `xpcall`, and `error` functions
- uses string "patterns" which are a simplified version of regular expressions
- limited Unicode support

  Strings can contain Unicode characters,
  but indexing into such strings does not account for their size.
  See {% aTargetBlank "http://lua-users.org/wiki/LuaUnicode", "Lua Unicode" %}.

- variables and functions are global by default,s
  but can be declared `local`

For a more extensive list of pros and cons, see {% aTargetBlank
"http://notebook.kulchenko.com/programming/lua-good-different-bad-and-ugly-parts",
"Lua: Good, bad, and ugly parts" %}.

## Used By

Notable uses of Lua include:

- {% aTargetBlank "https://www.angrybirds.com", "Angry Birds" %} game
- {% aTargetBlank "https://codea.io", "Codea" %}
  creates games and simulations on an iPad using Lua
- {% aTargetBlank "http://www.legoengineering.com/platform/nxt/",
  "Lego Mindstorms NXT" %} robotics platform
- {% aTargetBlank "https://www.minecraft.net/", "Minecraft" %} game
- {% aTargetBlank "https://neovim.io", "Neovim" %} text editor
- {% aTargetBlank "https://redis.io", "Redis" %} database
- {% aTargetBlank "https://www.roblox.com", "Roblox" %} game
- {% aTargetBlank "https://worldofwarcraft.blizzard.com/", "World of Warcraft" %} game
- Texas Instruments {% aTargetBlank
  "https://education.ti.com/en/resources/lua-scripting",
  "TI-Nspire" %} graphing calculators

Roblox uses a variant of Lua called Luau. From {% aTargetBlank
"https://devforum.roblox.com/t/what-is-the-difference-between-lua-and-luau/2181620/2",
"veilict" %}, "Luau's main focus was for much needed optimizations
to the Lua VM and to add more complex type checking,
which allows you to see more in-depth errors before runtime.
You can specify the type of a given variable yourself,
or you can have Luau infer the type for you."

For a more extensive list of Lua use, see the {% aTargetBlank
"https://en.wikipedia.org/wiki/List_of_applications_using_Lua",
"List of applications using Lua" %} Wikipedia page.

## Resources

- {% aTargetBlank "https://www.lua.org/manual/", "Lua Reference Manual" %}
- {% aTargetBlank "https://www.lua.org/pil/", "Programming in Lua" %} official book
- {% aTargetBlank "https://www.lua.org/gems/", "Lua Programming Gems" %} -
  "a collection of articles recording some of the wisdom and practice
  on how to program well in Lua"
- {% aTargetBlank "https://www.youtube.com/watch?v=XxcSvnEIUq4",
  "Why (and why not) Lua" %} talk by Roberto Ierusalimschy (2019)
- {% aTargetBlank "https://github.com/pallene-lang/pallene", "Pallene" %},
  "a statically typed and ahead-of-time compiled sister language to Lua,
  with a focus on performance"
- {% aTargetBlank "https://www.youtube.com/watch?v=H3inzGGFefg",
  "Lua and Pallene" %} talk by Roberto Ierusalimschy (2022)
- {% aTargetBlank "https://www.youtube.com/@teej_dv", "TJ DeVries" %}
  YouTube channel; TJ is a Neovim core maintainer and
  is the creator of several Neovim plugins including:

  - {% aTargetBlank "https://github.com/nvim-telescope/telescope.nvim", "telescope.nvim" %} "highly extendable fuzzy finder"
  - {% aTargetBlank "https://github.com/nvim-lua/plenary.nvim", "plenary.nvim" %} collection of Lua functions used to write Neovim plugins
  - {% aTargetBlank "https://github.com/tjdevries/tree-sitter-lua", "tree-sitter-lua" %} Tree-sitter grammar for Lua
  - {% aTargetBlank "https://github.com/tjdevries/colorbuddy.nvim", "colorbuddy.nvim" %} for creating Neovim color schemes

- {% aTargetBlank "https://github.com/nanotee/nvim-lua-guide",
  "Getting started using Lua in Neovim" %}
- {% aTargetBlank "https://github.com/LewisJEllis/awesome-lua",
  "Awesome Lua" %} - "a curated list of quality Lua packages and resources."

## Cheat Sheets

- {% aTargetBlank "https://devhints.io/lua", "DevHints.io" %}
- {% aTargetBlank "https://cheatography.com/srgmc/cheat-sheets/lua-scripting-5-1/",
  "Cheatography" %}
- {% aTargetBlank "https://www.codecademy.com/learn/learn-lua/modules/learn-lua-introduction/cheatsheet",
  "codecademy" %}

## Installing

Lua can be installed in macOS using Homebrew.
Enter `brew install lua` to install it.
Enter `lua -v` to verify that it worked and see the version.

To install in other operating systems, see
{% aTargetBlank "http://www.lua.org/start.html", "Getting Started" %}.

To experiment with Lua on the web without installing anything,
see {% aTargetBlank "http://www.lua.org/demo.html", "Lua Demo" %}.

## Compiling

Lua programs can be compiled to bytecode before runtime.
This enables faster program startup because
runtime bytecode generation is no longer needed.

The performance of standard Lua is worse than many scripting languages,
including Python, unless it is compiled
ahead of time (AOT) or just in time (JIT).

There are multiple ways to produce and execute Lua bytecode.

For a comparison of the performance of each approach, see {% aTargetBlank
"https://eklausmeier.goip.de/blog/2020/05-14-performance-comparison-pallene-vs-lua-5-1-5-2-5-3-5-4-vs-c/",
"Performance Comparison Pallene vs. Lua vs. C" %}.

{% aTargetBlank "https://www.lua.org/manual/5.1/luac.html", "luac" %}
is the standard Lua compiler.
It reads Lua source code and outputs Lua bytecode.

To generate a bytecode file, enter a command like `luac demo.lua`.
This creates the bytecode file `luac.out`.
To change the file name, use the `-o` option.
For example, `luac demo.lua -o demo.luac`.

To execute a bytecode file, pass it to the Lua interpreter.
For example, `lua demo.luac`.

## Flavors of Lua

### LuaJIT

{% aTargetBlank "https://luajit.org/", "LuaJIT" %} is an alternative
to `luac` that produces smaller bytecode files.
LuaJIT also executes bytecode files and provides runtime optimizations
that typically result in better performance.

LuaJIT is implemented by a separate team from the one that maintains Lua.

LuaJIT is based on Lua 5.1, so it is missing features of Lua added since then.

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

In some cases when using LuaJIT it is necessary to write code differently
than you would in vanilla Lua in order to get the best performance.

The LuaJIT foreign function interface (FFI) makes it easier to call
functions implemented in other languages than vanilla Lua.
Using the Lua C API in LuaJIT is slower than using it in vanilla Lua.

To install LuaJIT:

- Download the source by entering
  `git clone https://luajit.org/git/luajit.git`.
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
This will start faster than entering `luajit demo.lua`
which performs just-in-time compilation at runtime.

### Pallene

"{% aTargetBlank "https://github.com/pallene-lang/pallene", "Pallene" %}
is a statically typed and ahead-of-time compiled sister language
to Lua with a focus on performance. It is intended for
writing performance sensitive code that interacts with Lua,
a space that is currently filled by C modules and by LuaJIT.
Compared to C, Pallene should offer better
support for interacting with Lua data types,
bypassing the unfriendly syntax and performance overhead of the Lua-C API.
Compared to LuaJIT, Pallene aims to offer
more predictable run-time performance."

Pallene is the name of one of the moons of Saturn.
The name of the moon is pronounced "puh lee nee",
but the language designer pronounces it "pah lean".

One use case is to write performance-critical modules in Pallene,
compile them, and require them in Lua code.

To install Pallene, see the detailed instructions at the Pallene link above.

### Teal

{% aTargetBlank "https://github.com/teal-language/tl", "Teal" %} is a
typed dialect of Lua.

The supported types are `any`, `nil`, `boolean`, `integer`, `number`,
`string`, `function`, `enum`, `record`, `thread`, and
table types described by their allowed key and value types.

To install Teal, enter `luarocks install tl`.

To perform type checking of a Teal script without running it,
enter `tl check {file-name}.tl`.

To perform type checking and generate a `.lua` file
with type annotations removed, enter `tl gen {file-name}.tl`.

To run a Teal script, enter `tl run {file-name}.tl`.

To add support for Teal in Neovim, create the file
`~/.config/nvim/lua/user/plugins/vim-teal.lua` containing the following.
This may require also using ALE to get syntax highlighting.

```lua
return {
  "teal-language/vim-teal"
}
```

The following code demonstrates using Teal to implement {% aTargetBlank
"https://en.wikipedia.org/wiki/Fizz_buzz", "Fizz buzz" %}.
This code will make more sense after reading the sections that follow.

```lua
-- This type describes a number and the text that should
-- be printed if a given number is divisible by it.
local type Rule = {number, string}

local function fizzBuzz(limit: number, rules: {Rule})
  for n = 1, limit do
    local matched = false
    for _, rule in ipairs(rules) do
      local number = rule[1] as number
      local text = rule[2] as string
      if n % number == 0 then
        print(text)
        matched = true
      end
    end
    if not matched then print(n) end
  end
end

fizzBuzz(30, {
  {3, "fizz"},
  {5, "buzz"}
})
```

## Languages Based on Lua

{% aTargetBlank "http://ravilang.github.io/", "Ravi" %} is
a "dialect of Lua with limited optional static typing and JIT/AOT compilers".
The name comes from Sanskrit word for "Sun".

{% aTargetBlank "https://moonscript.org/", "MoonScript" %} is
"programmer friendly language that compiles into Lua".
It "gives you the power of the fastest scripting language
combined with a rich set of features".

{% aTargetBlank "https://terralang.org/", "Terra" %} is a
"low-level system programming language that is designed to
interoperate seamlessly with the Lua programming language".
It "shares Lua's syntax and control-flow constructs".

{% aTargetBlank "http://squirrel-lang.org/", "Squirrel" %} is a
"high level imperative, object-oriented programming language,
designed to be a light-weight scripting language"."
It was "inspired by languages like Python,Javascript and especially Lua".

## VS Code Support

VS Code has great support for Lua.

The {% aTargetBlank "https://github.com/LuaLS/lua-language-server", "Lua" %}
extension from sumneko is a Lua language server.
It provides code annotations, syntax checking, dynamic type checking,
code formatting, spell checking, and more.

To disable specific diagnostics,
open Settings, filter on "Lua", scroll down to "Lua > Diagnostics: Disable",
click the "Add Item" button, select a diagnostic name from the drop-down,
and click the "OK" button.

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

## Grammar

Lua is a very simple language.
The entire grammar, except comment syntax, is described below.

- "binop" is short for "binary operator".
- "exp" is short for "expression".
- "fieldsep" is short for "field separator".
- "stat" is short for "statement".
- "unop" is short for "unary operator".
- "var" is short for "variable".

{% raw %}

<code>
chunk ::= {stat [';']} [laststat[';']]

block ::= chunk

stat ::=  
 varlist1 '=' explist1 |  
 local namelist ['=' explist1] |  
 if exp then block {elseif exp then block} [else block] end |  
 do block end |  
 while exp do block end |  
 repeat block until exp |  
 for Name '=' exp ',' exp [',' exp] do block end |  
 for namelist in explist1 do block end |  
 function funcname funcbody |  
 local function Name funcbody |  
 functioncall

laststat ::= return [explist1] | break

funcname ::= Name {'.' Name} [':' Name]

varlist1 ::= var {',' var}

var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name

namelist ::= Name {',' Name}

explist1 ::= {exp ','} exp

exp ::=  
 nil | false | true | Number | String | '...' |  
 function | prefixexp | tableconstructor | exp binop exp | unop exp

prefixexp ::= var | functioncall | '(' exp ')'

functioncall ::= prefixexp args | prefixexp ':' Name args

args ::= '(' [explist1] ')' | tableconstructor | String

function ::= function funcbody

funcbody ::= '(' [parlist1] ')' block end

parlist1 ::= namelist [',' '...'] | '...'

tableconstructor ::= '{' [fieldlist] '}'

fieldlist ::= field {fieldsep field} [fieldsep]

field ::= '[' exp ']' '=' exp | Name '=' exp | exp

fieldsep ::= ',' | ';'

binop ::=  
 '+' | '-' | '\*' | '/' | '^' | '%' | '..' |  
 '<' | '<=' | '>' | '>=' | '==' | '~=' |  
 and | or

unop ::= '-' | not | '#'
</code>

{% endraw %}

## Source Files

Lua source files have a `.lua` extension.

A source file can use variables and functions defined in
another source file by using the `require` function.
The following statement loads the file `other.lua`:

```lua
require "other"
```

For more information on using `require`, see the "Modules" section.

## Running Code

To use the Lua {% aTargetBlank
"https://en.wikipedia.org/wiki/Read–eval–print_loop", "REPL" %},
enter `lua` and then enter Lua statements.
To exit the REPL, press ctrl-c or ctrl-d.

To run a Lua program, enter `lua {filename}.lua`.
This runs the Lua interpreter on the source file to produce bytecode
and uses the Lua virtual machine to execute the bytecode.
Alternatively, consider using `luajit`.

There are several tools that claim to
produce executable files from Lua programs.
However, none of them seem to be popular or actively maintained.

## Keywords

The Lua programming language only defines 22 keywords.
Some of the keywords such as `then`, `and`, `or`, and `end`
make the syntax similar to the Ruby programming language.

Each of the 22 keywords are summarized below.

Boolean literal values:

- `true`
- `false`

Conditional Logic:

- `if`
- `then`
- `elseif`
- `else`

Functions:

- `function`
- `return`

Iteration:

- `for`: loop over numbers or pairs
- `in`: used with `for` to specify what to loop over
- `while`: top-tested loop
- `repeat` and `until`: bottom-tested loo
- `break`: exit loop early

Logical Operators:

- `and`
- `or`
- `not`

Variables:

- `local`: default to global without this

Other:

- `do`: begins a new lexical block
- `end`: marks the end of a lexical block
- `goto`: jumps to a label
- `nil`: represents having no value

## Comments

A single-line comment begins with two dashes.
For example, `-- This is a comment.`

A multi-line comment is a single-line comment followed by a multi-line string.
They begin with `--[[` and end with `]]`.
No characters are allowed between the beginning `--` and `[[`.
Sometimes multi-line comments end with `--]]` to make the end easier to see,
but the `--` is really just part of the multi-line string.
For example:

```lua
--[[
This is a multi-line comment
that could contain Lua code
which is temporarily disabled.
--]]
```

To create a comment around lines that already contain multi-line comments,
include any number of equal sizes between the square brackets.
This is useful for commented out a section of code
that contains a commented out section.
For example:

```lua
--[=[
  print("one")
  --[[
  print("two")
  print("three")
  --]]
  print("four")
--]=]
```

## Types

Lua uses dynamic types.
The types of variables and function parameters
are never specified and are always inferred.

Primitive type conversions are supported by
the `tonumber` and `tostring` functions.
There are no builtin functions for converting
a boolean to a number, a number to a boolean, or a string to a boolean.

The only values treated as `false` in conditions are `false` and `nil`.
The number zero and empty strings are not treated as `false`.

The following table shows how to convert each primitive type
to the other primitive types.

| From        | To Boolean    | To Number      | To String     |
| ----------- | ------------- | -------------- | ------------- |
| boolean `b` | not needed    | `b and 1 or 0` | `tostring(b)` |
| number `n`  | `n == 0`      | not needed     | `tostring(n)` |
| string `s`  | `s == "true"` | `tonumber(s)`  | not needed    |

The conversion from a number to a boolean above
assumes that all numbers except zero should be treated as `false`.

The `tonumber` function returns `nil` if the conversion is not possible.

While it is not commonly done, functions can validate the
types of arguments passed to them using the `assert` function.
This takes a condition and
an optional message that defaults to "assertion failed!".
If the condition evaluates to `false`,
it prints the message and terminates the program.
For example:

```lua
function demo(s)
  assert(type(s) == "string", "demo requires a string")
  print("success")
end

demo("yes") -- success
demo(7) -- demo requires a string
```

## Input/Output

To write to stdout, use the `print` function or the`io.write` function.
Both take any number of arguments.

The `print` function adds a tab character between each value
and a newline at the end of its output.
The `print` function can be called with no arguments
to only write a newline character.

I prefer for the `print` function to separate values
with a single space instead of a tab.
To override it to do that, add the following function.

```lua
function print(...)
  local t = { ... }
  local len = #t
  for i, v in ipairs(t) do
    io.write(tostring(v))
    if i < len then io.write(' ') end
  end
  io.write('\n')
end
```

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
By default this returns a string, but it can also be asked to return a number.

The following code prompts for two numbers and prints their sum.

```lua
io.write("First Number: ")
-- This form of `io.read` returns a number
-- or nil if a non-number is entered.
n1 = io.read("n")

io.write("Second Number: ")
n2 = io.read("n")

if n1 and n2 then
  sum = n1 + n2
  print("Sum: " .. sum) -- `..` operator performs string concatenation
else
  print("An invalid number was entered.")
end
```

To print to the terminal using colors, see:

- {% aTargetBlank "https://github.com/kikito/ansicolors.lua", "ansicolors.lua" %}

  To install, enter `luarocks install ansicolors`

  ```lua
  local term = require 'term'
  local colors = term.colors

  print(colors("%{bright red}red"))
  print(colors("%{bright yellow}yellow"))
  print(colors("%{bright blue}blue"))
  print('white')
  ```

- {% aTargetBlank "https://github.com/hoelzro/lua-term", "lua-term" %}

  To install, enter `luarocks install lua-term`

  ```lua
  local term = require 'term'
  local colors = term.colors

  print(colors.bright .. colors.red 'red')
  print(colors.bright .. colors.yellow 'yellow')
  print(colors.bright .. colors.blue .. 'blue')
  print(colors.reset 'white')
  ```

For information on reading and writing files,
see the "Standard Library" "io Module" section below.

## Names

Variable and function names can contain letters, digits, and underscores,
but no other symbols. They cannot start with a digit.

In multi-word names, the words can be separated by underscores (preferred)
or written in camel-case.
For example, `one_long_name` or `oneLongName`.

Variable names, function names, and table keys are case-sensitive.

## Variables

To assign a value to a variable, use the `=` operator.

Variables with no assigned value have the value `nil`.

New values of any type can be assigned to a variable at any time.

The `type(some_variable)` function returns a string containing
the type name of the variable value.
This can be `nil`, `boolean`, `number`, `string`,
`table`, `function`, `thread`, or `userdata`.

The `thread` type represents a coroutine
which is described in the "Coroutines" section below.

The `userdata` type represents raw data provided through the C API.

Multi-variable assignment is supported.
For example:

```lua
local a, b, c = 1, 2, 3
a, b = b, a -- swaps values
```

Variables are global by default, even when defined in a different source file.
Use the `local` keyword to confine their use to the current scope.
It is common in Lua configuration files to expose values as global variables.
This is likely the reason that variables are
global, rather than local, by default.

For example:

```lua
a = 1 -- global, even if assigned inside a function
local b = 2 -- local to the current scope
```

It is an unenforced convention for
global variables to begin with a capital letter and
for the names of constants to be all uppercase.

All variables assigned without the `local` keyword
are added to the global table `_G`.
For example:

```
MyGlobal = "demo"
_G.MyGlobal = "demo" -- same as previous line

print(MyGlobal) -- demo
print(_G["MyGlobal"]) -- demo
print(_G.MyGlobal) -- demo
```

## Booleans

The boolean literal values are `true` and `false`.

In conditions, the only values treated as false are `false` and `nil`.
The number zero, an empty string, and an empty table are all treated as true.

Variables can be set to a condition to obtain a boolean value.
For example, `is_higher = my_score > your_score`
sets `is_higher` to `true` or `false`.

## Numbers

The `number` type typically represents numbers with
64-bit integers or double precision floats.
However, implementations are free to use other sizes.

Lua automatic converts numbers between integer and float representations
as needed.

The `math.type` function returns a string indicating
the specific type of a number. For example:

```lua
local i = 19
print(type(i), math.type(i)) -- number  integer

local f = 3.14
print(type(f), math.type(f)) -- number   float
```

## Strings

The `string` type is used for single and multiple character text.
There is no dedicated type for single characters.

Strings can be delimited with either single or double quotes.
For example, `'Hello World!'` or `"Hello World!"`.
This syntax is referred to as a "short literal string".

Escaping certain characters by preceding them with a backslash
changes how they are interpreted.
For example:

- `\n` produces a newline character
- `\t` produces a tab character
- `\"` produces a double quote inside a string delimited by double quotes
- `\'` produces a single quote inside a string delimited by single quotes
- `\\` produces a backslash character

Multi-line strings are delimited by `[[` and `]]`.
This syntax is referred to as a "long strings".
A newline after `[[` is ignored and a newline before `]]` is not ignored.
Otherwise newlines and indentation inside the square brackets are retained.
For example:

```lua
haiku = [[
Out of memory.
We wish to hold the sky.
But we never will.]]
```

There can be any number of `=` characters between the
opening and closing square brackets as long as the count matches.
For example, `[==[some text]==]`.
This enables creating strings that contain `]]` or
closing square brackets with a different number of equal signs between them.

Strings are indexed starting from 1 instead of 0.

To concatenate strings, use the `..` operator.
This creates a new string and does not modifier either operand.
For example, `fullName = firstName .. ' ' .. lastName`.

Strings are immutable.
Operations on them create new strings.

String operations are supported by the `string` standard library
which is described later.

## Patterns

A pattern is a string containing magic characters and character classes.
Patterns are similar to regular expressions,
but are bit simpler and not quite as powerful.

Regular expressions are not directly supported in Lua.
The reason for this is that
the code to support patterns is much smaller than
the code required to support regular expressions.
Opting out of regular expression support
helps achieve the Lua goal of being small.

Patterns are used in the string library functions
`find`, `gmatch`, `gsub`, and `match`.

Characters referred to as "magic characters" have special meaning in patterns.
To include them as literal characters without their special meaning,
precede them with a `%` character.
The magic characters include:

| Magic Character | Meaning                                             |
| --------------- | --------------------------------------------------- |
| `^`             | start anchor or negates a character class           |
| `$`             | end anchor                                          |
| `.`             | matches any single character                        |
| `?`             | zero or one                                         |
| `*`             | zero or more                                        |
| `+`             | one or more                                         |
| `[`             | begins a character class                            |
| `]`             | ends a character class                              |
| `-`             | forms a range in a custom character class           |
| `(`             | begins a capture group                              |
| `)`             | ends a capture group                                |
| `%`             | escapes a magic character (ex. `%$` represents `$`) |

Character classes are used to match sets of characters.
The provided character classes begin with a `%` character
rather than a backslash as they do in regular expression.
This was chosen because Lua patterns are strings and
strings use backslash to escape characters (ex. `\n`).
If character classes also began with a backslash,
they would need to be escaped (ex. `\\d` instead of `%d`).

The provided character classes include:

| Character Class | Meaning                            |
| --------------- | ---------------------------------- |
| `%a`            | letters                            |
| `%c`            | control characters                 |
| `%d`            | digits                             |
| `%g`            | printable characters except spaces |
| `%l`            | lowercase letters                  |
| `%p`            | punctuation characters             |
| `%s`            | space characters                   |
| `%u`            | uppercase letters                  |
| `%w`            | alphanumeric characters            |
| `%x`            | hexadecimal digits                 |

Custom character classes can be defined by listing individual characters
and/or ranges of characters inside square brackets.
For example:

- `[13579]` describes odd, single-digit numbers
- `[abcA-C]` allows the characters `a`, `b`, `c`, `A`, `B`, and `C`
- `[%da-fA-F]` is the equivalent of `%x` to describe a hexadecimal digit

TODO: Add content from https://www.lua.org/pil/20.2.html.

TODO: See examples in dev/lua/patterns.lua.

The `string.match` function takes a source string and a pattern.
When the pattern contains capture groups, `string.match` returns
the text matched by each capture group.
For example:

```lua
local s = "The score was 19 to 7."
local pattern = "(%d+).+(%d+)"
local score1, score2 = string.match(s, pattern)
print(score1, score2) -- 19      7
```

The `string.gsub` function performs global substitution.
It takes source string, a pattern for finding text to replace,
the replacement (a string, table, or function),
and an option limit on the number of replacements that should be made.
It returns a new version of the source string with the replacements made.
For example:

```lua
local s =
  "The 2nd time was easier than the 1st, and the 4th was a piece of cake."
local pattern = "%d%l%l"

local replTable = {
  ["1st"] = "first",
  ["2nd"] = "second",
  ["3rd"] = "third"
}
-- If a key for the match is not found in `replTable`, it keeps the match.
local s2 = string.gsub(s, pattern, replTable)
print(s2)
-- The second time was easier than the first, and the 4th was a piece of cake.

-- This function produces the same result as using `replTable`.
local function replFn(match)
  if match == "1st" then return "first" end
  if match == "2nd" then return "second" end
  if match == "3rd" then return "third" end
  return match
end

local s3 = string.gsub(s, pattern, replFn)
print(s3)
-- The second time was easier than the first, and the 4th was a piece of cake.
```

## Operators

Lua supports the following mathematical operators:

- `+` addition (not string concatenation)
- `-` subtraction (binary) or negation (unary)
- `*` multiplication
- `/` division; ex. `7 / 4` gives `1.75`
- `//` floor division; ex. `7 // 4` gives `1`
- `%` modulo; ex. `7 % 4` gives `3`
- `^` exponentiation; ex. `2 ^ 3` gives `8`

The `++` and `--` operators found in many other programming languages
are not supported.

Lua supports the following relational operators:

- `==` equal
- `~=` not equal (differs from other languages that use `!=`)
- `<` less than
- `>` greater than
- `<=` less than or equal to
- `>=` greater than or equal to

Tables are compared by their memory addresses,
not by their contents.

Lua supports the following logical operators:

- `and`
- `or`
- `not`

Lua supports the following bitwise operators:

- `&` bitwise and
- `|` bitwise or
- `~` bitwise exclusive or (binary) or bitwise not (unary)
- `<<` bit shift left
- `>>` bit shift right

The only supported assignment operator `=`.
Shorthand assignment operators like `+=` that are supported
by many other programming languages are not supported in Lua.
To add a number to a variable, use the form `myVar = myVar + n`.

The operator `..` is used to concatenate strings.
Both operands must be either a string or number.
Numbers are automatically converted to strings.

The `#` operator is applied on the left side of a string or table
to get its length.
For a string, this returns the number of bytes which can differ
from the number of characters when Unicode characters are included.
For a table, this returns the last consecutive integer index starting from `1`.
In a table with the keys `1`, `2`, `3`, and `7`, the length is reported as `3`.

Lua operators have the following precedence from highest to lowest:

- `^`
- unary `-`, `~`, `#`, and `not`
- `*`, `/`, `//`, `%`
- `+` and binary `-`
- `..`
- `<<` and `>>`
- `&`
- binary `~`
- `|`
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

All operators and special characters in Lua have a single meaning.
For example:

- `+` always performs addition, not string concatenation which uses `..`
- `{' and `}' always construct a table and do not delimit a code block
- `:` always define or calls a function that can be used in the "method" style

## Conditional Logic

Lua supports an `if` statement, but not a `switch` statement.

In an `if` statement, the `elseif` and `else` blocks shown below are optional.

Parentheses are not required around conditions.

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

Lua does not have a ternary operator, but the
same functionality can be achieved with the following:

```lua
local result = condition and trueValue or falseValue
```

For the condition, recall that `false` and `nil` are treated as `false`
and all other values are treated as `true`.

## Iteration

Lua supports `for`, `while` (top tested), and `repeat` (bottom tested) loops.
The `for` loop has two variations,
one that iterates over a range of numbers and
one that iterates over table entries.

```lua
-- Use a `for` loop to iterate over a range of numbers
   with a given step size.
-- The loop variable, `i` in this case, is local to the loop
-- and cannot be accessed outside it.
for i = 1, 10, 2 do
  ...
end

-- Use a `for` loop to iterate over the key/value pairs in a table.
for key, value in pairs(mytable) do
  ...
end

-- Use a `for` loop to iterate over the index/value pairs
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

Loops can use the `break` keyword to exit early.
The `continue` keyword for advancing to the next iteration
is not currently supported.

## Iterators

The `pairs` and `ipairs` functions that are used to iterate over tables
return three values needed by a `for` loop.
Understanding the purpose of these three values isn't necessary
unless there is a reason to implement custom iterators.

The following code exposes the three values returned by the `pairs` function
and provides insights on how a custom iterator might be implemented.
A `for` loop that uses the `pairs` function could be replaced
by code like the following:

```lua
local t = {'a', 'b', 'c'}

local iterator, state, control = pairs(t)
-- `iterator` is a function that can be called
-- to get the next control and value.
-- `state` returned by pairs is just a reference to `t`.
-- `control` returned by `pairs` is `nil`.

while true do
  -- Get the next control (table key) and its value.
  control, value = iterator(state, control)
  if not control then break end -- reached end
  print(control, value)
end
```

The output of the code above is:

```lua
1       a
2       b
3       c
```

The following custom function can be used to
iterate over only the odd numbers in an array-like table.
Note how it returns an iterator function, a state, and a control value.

```lua
function odds(t)
  local function iterator(state, control)
    control = control or 0
    repeat
      control = control + 1
      value = t[control]
    until control == nil or (value ~= nil and value % 2 == 1)
    return control, value
  end
  return iterator, state, nil
end

local t = {2, 3, 7, 8, 10, 13}
for i, v in odds(t) do
  print(v) -- 3, 7, and 13
end
```

Other custom implementations could build and return
a state based on the table passed in and
could start with a control value other than `nil`.

## Functions

Functions are defined with the `function` keyword.
This is longest keyword in Lua.
It's too bad the designers didn't choose a shorter keyword
like `func`, `fun`, or `fn`.

Parameters are specified in parentheses after the function name
and are separated by commas.
The parentheses are required even if there are no parameters.

The `return` keyword is used to exit a function
and return zero or more values to the caller.

For example:

```lua
local function add(n1, n2)
  return n1 + n2
end

print(add(2, 3)) -- 5
```

All functions are anonymous.
The function definition above is just syntactic sugar for the following:

```lua
local add = function (n1, n2)
  return n1 + n2
end
```

All functions are closures. This means they capture in-scope variables
and can use them later when called.

When a function call passes a single literal string or literal table,
the parentheses can be omitted.
For example, the following are equivalent ways
to pass a literal string to a function:

```lua
takeString("some text")
takeString "some text"
takeString [[some text]]
```

Likewise, the following are equivalent ways to pass a table to a function.

```lua
takeTable({1, 2, 3})
takeTable {1, 2, 3}
```

To avoid writing functions that take a large number of arguments,
use a parameter that expects a table.
This simulates having named arguments and
allows values to be specified in any order.
Table entries with no key (array-like)
can be thought of as positional parameters and
table entries with a key can be thought of as named parameters.

Primitive arguments are passed by value
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
foo(b, n, s, t) -- only t.x is modified in this scope
print(b, n, s, t.x) -- false 1 no 2
```

Functions in Lua are first-class.
This means they can accept other functions as arguments
and they can return other functions.
For example:

```lua
function pow(n1, n2) return n1 ^ n2 end

-- This function accepts another function as an argument.
function combine(n1, n2, operation)
  return operation(n1, n2)
end

print(combine(2, 3, pow)) -- 8.0

-- This function returns another function.
function makeGreetFn(greeting)
  return function (name)
    print(greeting .. " " .. name .. "!")
  end
end

local greet = makeGreetFn("Hola");

greet("Mark") -- Hola Mark!
```

Functions can return multiple values.
For example:

```lua
function getStooges()
  return "Moe", "Larry", "Curly"
end

s1, s2, s3 = getStooges()
print(s1, s2, s3) -- Moe Larry Curly
```

An underscore can be used as a placeholder for unneeded return values.
For example:

```lua
_, s2 = getStooges()
print(s2) -- Larry

_, _, s3 = getStooges()
print(s3) -- Curly
```

When getting multiple return values from a function,
underscores can be used as a placeholder for unneeded values.
For example, when calling the `ipairs` function,
if the index isn't needed then it can be used as follows:

```lua
for _, v in ipairs(t) do
  ...
end
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

print(add(1, 2, 3)) -- 6

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

print(report("Mark", 62, "running", "biking", "programming"))
-- Mark is 62 years old and likes 3 things.
-- They are running, biking, and programming.
```

## Default Parameter Values

Lua does not provide an explicit way to define
default values for function parameters.
There are approaches two implement this.

The first approach assigns default values using the `or` operator.
For example:

```lua
function greet(greeting, name)
  greeting = greeting or "Hello"
  name = name or "World"
  print(string.format("%s, %s!", greeting, name))
end

greet() -- Hello, World!
greet("Hola", "Mark") -- Hola, Mark!

function volume(t)
  local w = t.width or 1
  local h = t.height or 1
  local d = t.depth or 1
  return w * h * d
end

print(volume({width=2, height=3, depth=4})) -- 24
print(volume({width=2, height=3})) -- 6
print(volume({width=2, depth=4})) -- 8
print(volume({})) -- 1
```

The `volume` function can also be implemented as follows:

```lua
function volume(t)
  return (t.width or 1) * (t.height or 1) * (t.depth or 1)
end
```

The second approach is to accept a table argument and
specify default values in that table using the `setmetatable` function.
Metatables are discussed in more detail later.
Basically they can provide an alternate place to look
when a given key is not found in a table.
For example:

```lua
function volume(t)
  -- This supplies default values for missing keys in the table t.
  setmetatable(t, {__index={width=1, height=1, depth=1}})
  return t.width * t.height * t.depth
end
```

## Tables

Lua only provides one data structure called a "table"
which is an associative array.
It is used to represent arrays, dictionaries, trees, and graphs.

A single table can be array-like, dictionary-like, or both.

Assigning a table to a variable assigns the reference,
not a copy of the table.

_Implementation Detail from {% aTargetBlank "https://www.lua.org/gems/", "Lua Programming Gems" %}_

> "Every table in Lua has two parts: the array part and the hash part.
> The array part stores entries with integer keys in the range 1 to n,
> for some particular n.
> All other entries (including integer keys outside that range)
> go to the hash part."

To create a table that is array-like,
provide a comma-separated values inside curly braces.
Curly braces are only used to construct tables.
They are not used to delimit blocks of code.

For example:

```lua
local scores = {5, 2, 7}

local names = {"Mark", "Tami"}
```

To create a table that is dictionary-like,
provide keys and values inside curly braces.
Each key is followed by `=` and its value.

Keys can be any value except `nil`, including other tables.
String keys only require quotes if
they contain special characters such as spaces.
Non-string keys and string keys in quotes must be enclosed in square brackets.

Values can be of any type, including other tables.
String values must be enclosed in quotes.

For example:

```lua
local scores = {Mark=5, Tami=7, ["Unknown Player"]=3} -- uses string keys

local words = {[true]="yes", [false]="no"} -- uses boolean keys

local months = {[1]="January", [2]="February"} -- uses integer keys

-- This has the same result as the previous line,
-- but values are specified without keys.
-- Integer keys are provided starting from 1.
local months = {"January", "February"}
```

To create a table that is both array-like and dictionary-like,
provide some values with keys and some values without.
Each value without a key is assigned a consecutive index key starting from 1.
For example:

```lua
local mixed = { "apple", month="April", "banana", season="Spring", "cherry" }
for k, v in pairs(mixed) do
  print(k, v)
end
-- Output is:
-- 1	apple
-- 2	banana
-- 3	cherry
-- month	April
-- season	Spring
```

There are two ways to assign a key/value pair to a table.
The square bracket syntax works with any key value.
The dot syntax works with keys that are value variable names.
For example:

```lua
local point = {x = 0, y = 0}
point["x"] = 1
point.x = 1 -- same as previous line
```

An empty table can be created and filled later.
For example:

```lua
local later = {} -- creates an empty table
later[1] = "Hello"
later["condition"] = "sunny"
later.condition = "sunny" -- same
```

To get the corresponding value of a table key,
use the square bracket or dot syntax.
For example:

```lua
print(scores.Tami) -- 7
print(scores["Tami"]) -- 7

-- When a key is not found, the value returned is `nil`.
-- The `or` operator can be used to provide a default value.
print(scores["Mary"] or "unknown") -- unknown

print(months[2]) -- February
```

To iterate over keys and values, use a `for` loop
with the `pairs` or `ipairs` function.

The `pairs` function visits all the key/value pairs, but not
necessarily in the order in which they were added to the table.
Entries where the value is set to `nil` are not visited.

The `ipairs` function iterates over array-like entries,
only visiting consecutive integer keys starting with `1`.
It stops as soon as it fails to find the next integer key.

The code below demonstrates these functions and
also shows that a table can be both array-like AND dictionary-like.

```lua
-- The entries `5` and `3` are array-like.
-- `5` is assigned the key `1` because it is the first array-like entry.
-- `3` is assigned the key `2` because it is the second array-like entry.
-- The entries `foo=2` and `bar=4` are dictionary-like.
local t = {5, foo=2, 3, bar=4}

for k, v in pairs(t) do -- keys and values
  print(k, v)
end
-- 1       5
-- 2       3
-- bar     4
-- foo     2

for i, v in ipairs(t) do -- indexes and values
  print(i, v)
end
-- 1       5
-- 2       3
```

The `table.insert` function inserts an array-like entry.
The array-like entries past the insertion point move up by one.
This can be expensive if a large number of entries need to be moved.
For example:

```lua
local names = {"Mark", "Tami"}
table.insert(names, 2, "Comet") -- Mark Comet Tami
table.insert(names, "Bob") -- pushes onto end; Mark Comet Tami Bob
```

The `table.remove` function removes an array-like entry.
The array-like entries past the removal point move down by one.
This can be expensive if a large number of entries need to be moved.
For example:

```lua
local names = {"Mark", "Comet", "Tami", "Bob"}
table.remove(names, 2) -- changes to Mark Tami Bob and returns Comet
-- Omitting the index pops the last entry.
table.remove(names) -- changes to Mark Tami and returns Bob
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
local t = {"Mark", 7, "Tami", 9}
print(table.concat(t, " and ")) -- "Mark and 7 and Tami and 9"
local t = {5, foo=2, 3, bar=4}
print(table.concat(t, " and ")) -- 5 and 3; dictonary-like entries ignored
```

The `table.sort` function sorts an array-like table in place.
For example:

```lua
local names = {"Tami", "Mark", "Amanda", "Jeremy"}

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
To get the count, it is necessary to iterate over the table and count them.
The "Lua Functional" library described later defines a `length` function
for computing the size of a table, but it has O(n) complexity.

The `table.pack` function provides another way to create an array-like table
that adds the key `n` which holds the number of entries.
The value of the `n` is not updated when entries are inserted or removed.
For example:

```lua
local t = table.pack("Mark", 7, "Tami", 9)
print(t.n) -- 4
```

The `table.unpack` function returns consecutive array-like entries
with keys starting from `1`.
It does not work with dictionary-like tables.
For example:

```lua
local names = {"Mark", "Tami", "Comet"}
local father, mother, dog = table.unpack(names)
print(father, mother, dog) -- Mark    Tami    Comet
print(table.unpack(names)) -- same
```

Table values can be other tables.
This supports creating the equivalent of multi-dimensional arrays.
For example:

```lua
local my2d = {}
my2d[i] = {}
my2D[i][j] = "some-value"
print(my2D[i][j]) -- some-value

local points = {
  {x=2, y=3},
  {x=5, y=1},
  {x=3, y=7}
}
print(points[2].x) -- 5
```

The `table.move` function copies entries from one array-like table to another.
It does not remove entries from the source table.
Existing destination table entries at target indexes are replaced.

For example:

```lua
local function dump(t)
  for _, v in ipairs(t) do
    print("  " .. v)
  end
end

local t1 = {1, 2, 3}
local t2 = {4, 5, 6}
-- Copy table t1 elements 1 to 3 into table t2 starting at index 3.
-- The arguments are source table, start index, end index,
-- destination index, and destination table.
table.move(t1, 1, 3, 3, t2)
dump(t1) -- 1 2 3
dump(t2) -- 4 5 1 2 3; existing value 6 was replaced with 1
```

## Modules

A module is defined by a table whose entries are variables and functions.
Typically each module is defined in its own source file
and made available in other source files using the `require` function.

To define a module named `my_module`, create a file
named `my_module.lua` containing code like the following:

```lua
-- It is a convention to name this "M".
local M = {} -- a table

M.some_variable = "some value"

-- Function names containing a dot cannot be declared `local`.
function M.some_function(p1, p2)
  print("some_function was passed " .. p1 .. " and " .. p2)
end

return M
```

The following code shows an equivalent way to write the code above:

```lua
return {
  some_variable = "some value",
  some_function = function(p1, p2)
    print("some_function was passed " .. p1 .. " and " .. p2)
  end
}
```

The following code demonstrates using the module defined above.
Note that the string passed to the `require` function
does not include the `.lua` file extension.

```lua
local mm = require("my_module")
print(mm.some_variable) -- some value
mm.some_function(1, 2) -- some_function was passed 1 and 2
```

A Lua "package" is a collection of modules.

## Metatables

A metatable is a table that defines metamethods.
All metamethods have names that begin with two underscores.
Examples include:

- `__add` - defines how the `+` operator adds values
  represented by specific kinds of tables
- `__tostring` - defines the string representation of a value
- `__index` - determines the value that should be returned
  when an attempt is made to access a missing key

The `getmetatable(table)` function returns the metatable
associated with a given value or `nil` if one has not been assigned.

Any value can have a metatable.
Of the eight types defined by Lua,
only instances of the `string` type have a metatable by default.
The following code demonstrates this:

```lua
assert(getmetatable(nil) == nil) -- nil type
assert(getmetatable(true) == nil) -- boolean type
assert(getmetatable(123) == nil) -- number type

assert(getmetatable("test") ~= nil) -- string type
assert(getmetatable("test").__index == string)
-- This shows that the alternate place to look
-- for string functions is the `string` module.

local t = {}
assert(getmetatable(t) == nil) -- table tuype

local function fn() end
assert(getmetatable(fn) == nil) -- function type

local thread = coroutine.create(fn)
assert(getmetatable(thread) == nil) -- thread type
```

The colon operator provides syntactic sugar for an alternate way
to call a function that is defined as a table entry.
For example, the metatable of all `string` instances
contains all the string functions.
There are two ways to get the uppercase version of a string.

```lua
local s = "test"
print(string.upper(s)) -- uses the dot operator and returns "TEST"
print(s:upper()) -- uses the colon operator and returns "TEST"
```

In the last call to `upper` above
Lua attempts to find an `upper` function in the value of `s`.
But `s` refers to a string rather than a table,
so the `upper` function is not found there.
Next Lua gets the metatable of `s` and looks for `upper`
in the table that is the value of its `__index` entry.
It finds `upper` defined there and calls it,
passing it the value before the colon which is `s`.

The same approach does not work with table instances.
Table instances do not have a metatable,
much less one whose `__index` key is set to `table`.
It is possible to configure this for a specific table instance.
But doing so only adds the ability to use the colon operator
with that specific table instance.
For example:

```lua
local t = {"apple", "banana", "cherry"}
print(table.concat(t, " and ")) -- apple and banana and cherry
setmetatable(t, {__index = table})
print(t:concat(" and ")) -- apple and banana and cherry
```

Another issue with this approach is that assigning keys in the table `t`
that have the same name as a table function
makes those functions inaccessible using the colon operator.
For example:

```lua
t.concat = "broken"
print(t:concat(" and ")) -- attempt to call a string value (method 'concat')
```

### Table Delegation

Custom metatables can be added to tables, but not to any other Lua types.
This is done with the `setmetatable(table, metatable)` function.

The `setmetatable` function returns its first argument which is
useful for getting a reference when a table constructor is passed.

The `__index` entry can be implemented in two ways.
It can be a table that supplies default values for missing properties
or it can be a function that is passed a table and a key.
Both approaches are shown below.

The same metatable can be assigned to multiple tables.
Doing so allows them to share both functionality and data.

The simplest use of metatables is to associated one with a single table.
The following code demonstrates this.

```lua
local t = {alpha = 7}
print(getmetatable(t)) -- nil; no metatable assigned yet

-- Create a metatable containing one metamethod named `__index`.
-- Its value is a table holding default key/value pairs.
local my_metatable = {__index = {alpha = 1, beta = 2}}

-- Associate the metatable with the table.
setmetatable(t, my_metatable)

print(t.alpha, t.beta, t.gamma) -- 7 2 nil
```

There is no need to hold the metatable in a variable.
It can be assigned directly to the table as follows:

```lua
setmetatable(t, {__index = {alpha = 1, beta = 2}})
```

Instead of defining a separate table for the metatable,
we can make the table serve as its own metatable as follows:

```lua
t.__index = {alpha = 1, beta = 2}
setmetatable(t, t)
```

The value of `__index` can be a function instead of a table.
For example:

```lua
t.__index = function (table, key)
  if key == "beta" then return 2 end
  return nil
end
setmetatable(t, t)
print(t.alpha, t.beta, t.gamma) -- 7 2 nil
```

All these variations produce the same results.
You may encounter all of these approaches in code that others write,
so it is necessary to understand all of them.

Adding a metatable to a single table instance is not nearly as useful as
adding a metatable to all instances of a given kind of table
when they are created.
This is the topic of the next section.

### Classes

Lua does not directly support defining classes and creating instances.
However, these can be simulated using a combination of tables,
metatables, and functions.

A class can be represented by a table.
A `new` function can be added to the table and used to
create instances which are represented by new tables.
There is nothing special about the name "new",
but many other programming languages use that name for creating instances
of a class, so it's a good idea to stick with that convention.

The `new` function should:

- Create a new table to represent the instance OR
  use a table that is passed in.
- Associate a metatable with the new table
  that holds default property values and
  functions that act as instance methods of the class.
- Return the table that represents the instance.

There are a few common approaches for implementing all of this.

Approach #1:

The metatable can be a separate table defined
outside the `new` function in a variable.

```lua
-- Define a Shape class.
Shape = {name = "unknown", sides = 0}
local metatable = {__index = Shape}

-- Constructor
function Shape.new(name, sides)
  local instance = setmetatable({}, metatable)
  instance.name = name
  instance.sides = sides
  return instance
end

-- Method - note the colon
function Shape:report()
  print(string.format("%s has %d sides", self.name, self.sides))
end

my_shape = Shape.new("triangle", 3)
my_shape:report() -- triangle has 3 sides
```

Approach #2:

The metatable can be a property of the class table.

This only requires the following minor changes to the code in Approach #1.

```lua
-- Replace the line starting with `local metatable` with this.
Shape.metatable = {__index = Shape}

-- Replace the line starting with `local instance` with this.
local instance = setmetatable({}, Shape.metatable)
```

Approach #3:

The metatable can be the class table itself.
This approach requires defining the `new` function with a colon
so it can access the class table using the `self` variable.

This only requires the following minor changes to the code in Approach #2.

```lua
-- Replace the line starting with `Shape.metatable =` with this.
Shape.__index = Shape

-- Replace the line starting with `local instance` with this.
local instance = setmetatable({}, Shape)
```

The following code demonstrates defining and using a `Person` "class".

```lua
local mt = {
  __index = {
    -- Taking a "self" argument allows calling with the method syntax.
    -- There is no way to define a method with a colon inside a table.
    haveBirthday = function (self)
      self.age = self.age + 1
    end,

    -- Taking a "self" argument allows calling with the method syntax.
    report = function (self)
      print(string.format("%s is %d years old.", self.name, self.age))
    end
  }
}

local Person = {
  new = function (p)
    p = p or {name = "unknown", age = 0}
    setmetatable(p, mt)
    return p
  end
}

local me = Person.new({name = "Mark", age = 61})
me:haveBirthday()
me:report() -- Mark is 62 years old.
```

The following code demonstrates defining a `Point` class
and three ways to define a function that
computes the distance from the origin to a point.

```lua
local Point = {}

local mt = {
  __index = {
    distance1 = function(point)
      return math.sqrt(point.x ^ 2 + point.y ^ 2)
    end
  }
}

function Point.distance2(point)
  return math.sqrt(point.x ^ 2 + point.y ^ 2)
end

function Point.new(p)
  p = p or { x = 0, y = 0 }

  function p:distance3()
    return math.sqrt(self.x ^ 2 + self.y ^ 2)
  end

  setmetatable(p, mt)
  return p
end

local pt = Point.new({ x = 3, y = 4 })

-- Call function defined on Point instance as a method.
-- This is bad because a new function is attached to every instance.
print(pt:distance3()) -- 5.0

-- Call function defined in Point table as a function.
-- This is the approach used by the `table` type for functions like `concat`,
-- but it is not as concise as the last approach below.
print(Point.distance2(pt)) -- 5.0

-- Call function defined in metatable \_\_index as a function.
-- This is not a nice as the previous call because `pt` must be repeated.
print(pt.distance1(pt)) -- 5.0

-- Call function defined in metatable \_\_index as a method.
-- This is the preferred approach because only one copy of the function
-- is defined (in the shared metatable) and calls to it are concise.
print(pt:distance1()) -- 5.0
```

### Method Syntax

Tables can have entries where the key is a function name
and the value is a function.
While this can be done in any table, it makes the most sense to do this
in tables that are the values the `__index` property in a metatable.
The reason for this is that those functions can be shared
by multiple other tables that use the metatable.

These functions can be referred to as "methods" because
they are intended to be invoked on a specific table instance.

Methods can be defined inside or outside of a table constructor.
There is one syntax for defining them inside a table constructor
and two syntaxes for defining them outside.

The following code demonstrates all three approaches:

```lua
-- File point.lua
local M = {}

local mt = {
  __index = {
    distance1 = function (point)
      return math.sqrt(point.x^2 + point.y^2)
    end
  }
}

function mt.distance2(point)
  return math.sqrt(point.x^2 + point.y^2)
end

function mt:distance3()
  return math.sqrt(self.x^2 + self.y^2)
end

function M.new(p)
  p = p or {x = 0, y = 0}
  setmetatable(p, mt)
  return p
end

return M
```

The following code demonstrates using the module defined above:

```lua
local Point = require("point")

local p = Point.new({x = 3, y = 4})
print(Point.distance1(p)) -- 5.0
print(p:distance1()) -- 5.0
print(Point.distance2(p)) -- 5.0
print(p:distance2()) -- 5.0
print(Point.distance3(p)) -- 5.0
print(p:distance3()) -- 5.0
```

- `MyTable.my_function(p1, p2)` referred to below as a "dot function"
- `MyTable:my_function(p1, p2)` referred to below as a "colon function"

Dot functions are somewhat like object-oriented class methods and
colon functions are somewhat like object-oriented instance methods.
The only difference between these is that colon functions
are supplied with a hidden first parameter named self
whose value is the value on which the function was called.

Functions defined on table can be called using either a dot or a colon,
regardless of whether the function was defined with a dot or a colon.
The only difference between these is that
calling a table function using a colon
automatically passes the value on which it is called as the first argument,
moving all the explicit arguments to the right.

Let's clear up the confusion with some examples.

```lua
local point = { x = 3, y = 4 }
function Point.dot_distance(self)
  return math.sqrt(self.x^2 + self.y^2)
end
function Point:colon_distance()
  return math.sqrt(self.x^2 + self.y^2)
end
print()

Point = class({
  -- Properties
  x = 0,
  y = 0,

  -- Methods
  distanceFromOrigin = function(p)
    return math.sqrt(p.x ^ 2 + p.y ^ 2)
  end,
  print = function (p)
    print(p) -- uses __tostring below
  end,

  -- Metamethods
  __add = function (p1, p2)
    return Point.new({x = p1.x + p2.x, y = p1.y + p2.y})
  end,
  __tostring = function (p)
    return string.format("(%.2f, %.2f)", p.x, p.y)
  end
})

local p1 = Point.new {x = 3, y = 4}
print(p1) -- (3.00, 4.00)
print(p1:distanceFromOrigin()) -- 5.0

local p2 = Point.new {x = 5, y = 1}
local p3 = p1 + p2 -- uses the __add metamethod
print(p3) -- (8.0, 5.0)

local p4 = Point.new {y = 7}
print(p4) -- (0.00, 7.00)

MyType = {
  operation = function(p1, p2)
    print(p1, p2)
  end
}

MyTable.dotInner(1, 2) -- 1, 2

-- This is an alternate way to write the dotInner function,
-- defining it outside the table constructor.
function MyTable.dotOuter(p1, p2)
  print(p1, p2)
end

MyTable.dotOuter(1, 2) -- 1, 2

-- Here dotOuter is called with a colon.
-- Calling any function using a colon causes the variable
-- before the colon to be passed as the first argument.
-- This makes 1 the second argument and 2 the third.
-- Dot function should never be called like this.
MyTable:dotOuter(1, 2) -- MyTable address, 1 (last argument 2 is ignored)

-- Colon functions cannot be defined inside a table constructor.
-- They have an invisible first parameter named `self`.
function MyTable:colonOuter(p1, p2)
  print(self, p1, p2)
end

-- Here `colonOuter` is called without a colon,
-- so MyTable is not passed as the first argument.
-- Colon functions should never be called like this.
MyTable.colonOuter(1, 2, 3) -- 1, 2, 3

-- Here colonOuter is called with a colon,
-- so MyTable is passed as the first argument.
-- Colon functions should always be called like this.
MyTable:colonOuter(1, 2) -- MyTable address, 1, 2

-- This is equivalent to the previous line.
-- While this works, it is overly verbose.
MyTable.colonOuter(MyTable, 1, 2) -- MyTable address, 1, 2
```

TODO: See my question about the colon syntax at https://www.reddit.com/r/lua/comments/12tc33n/colon_syntax/
TODO: The answer is provided in the example code below!

```lua
s = "test"
print(s:upper()) -- TEST
-- The previous line works because all string instances have a metatable
-- with `__index` set to the table `string` that defines all the string functions.
print(getmetatable(s).__index.upper)

-- Tables do not do the same thing.
t = {"a", "b", "c"}
print(table.concat(t, " ")) -- a b c
-- print(t:concat(" ")) -- attempt to call a nil value (method 'concat')
print(getmetatable(t)) -- nil

-- We can fix this for individual tables, not for all of them at once.
setmetatable(t, {__index = table})
print(t:concat(" ")) -- a b c
```

### Inheritance

We can simulate class inheritance by setting the metatable
of a class to its superclass.
The following code demonstrates this.

The `Shape` class is a abstract class,
meaning we cannot create instances from it.
Its purpose is to serve as a base class
for other classes that wish to inherit from it.
The lack of a `new` function is what makes it abstract.

The `Triangle` and `Rectangle` classes both inherit from the `Shape` class.
The `Square` class inherits from the `Rectangle` class.

```lua
-- Define a Shape class.
Shape = {name = "unknown", sides = 0}
Shape.__index = Shape

-- Method
function Shape:report()
  print(string.format(
    "%s has %d sides and area %.1f.",
    self.name,
    self.sides,
    self:area()
  ))
end

-- Define a Triangle class.
Triangle = {name = "triangle", sides = 3}
Triangle.__index = Triangle
setmetatable(Triangle, Shape) -- Triangle inherits from Shape

-- Constructor
function Triangle.new(base, height)
  local instance = setmetatable({}, Triangle)
  instance.base = base
  instance.height = height
  return instance
end

-- Instance Method
function Triangle:area()
  return self.base * self.height / 2
end

triangle = Triangle.new(4, 6)
triangle:report() -- triangle has 3 sides and area 12.0

-- Define a Rectangle class.
Rectangle = {name = "rectangle", sides = 4}
Rectangle.__index = Rectangle
setmetatable(Rectangle, Shape) -- Rectangle inherits from Shape

-- Constructor
function Rectangle.new(width, height)
  local instance = setmetatable({}, Rectangle)
  instance.width = width
  instance.height = height
  return instance
end

-- Instance Method
function Rectangle:area()
  return self.width * self.height
end

rectangle = Rectangle.new(4, 6)
rectangle:report() -- rectangle has 4 sides and area 24.0

-- Define a Square class.
-- Note that we didn't specify the number of sides.
-- It will get that from its superclass Rectangle.
Square = {name = "square"}
Square.__index = Square
setmetatable(Square, Rectangle) -- Square inherits from Rectangle

-- Constructor
function Square.new(side)
  local instance = setmetatable({}, Square)
  instance.side = side
  return instance
end

-- Instance Method
function Square:area()
  -- TODO: Is it possible to compute this using the superclass method?
  return self.side ^ 2
end

square = Square.new(5)
square:report() -- square has 4 sides and area 25.0
```

TODO: Can you simplify inheritance similar to how you simplified defining a class?
TODO: Maybe you should show this example again using your `class` function.

### Multiple Inheritance

TODO: Add this based on https://www.youtube.com/watch?v=1BFoprD30dE&t=97s.

### Simplifying Classes

There is a fair amount boilerplate code in all the examples above
and plenty of opportunity to make mistakes.
We can address this by creating an "oo" module that defines the
functions `class` and `subclass` that do all the work for us.

The `oo` module can be defined as follows:

```lua
local M = {}

-- This adds a function to the builtin `string` module.
function string.startsWith(source, target)
  -- 1 is the index at which to start the search.
  -- true turns off use of patterns which improves performance.
  return source:find(target, 1, true) == 1
end

local function move_metamethods(source, target)
  for k, v in pairs(source) do
    if k:startsWith("__") then
      target[k] = v
      source[k] = nil
    end
  end
end

-- This creates a table that simulates an OO class.
-- The `defaults` parameter is a table that holds default property values
-- and optional metamethods like `__tostring`.
function M.class(defaults)
  assert(type(defaults) == "table", "defaults must be a table")

  local metatable = { __index = defaults }
  move_metamethods(defaults, metatable)

  return {
    meta = metatable, -- used by subclass function
    new = function(initial)
      assert(not defaults.abstract, "cannot create instance of abstract class")
      local instance = initial or {}
      setmetatable(instance, metatable)
      return instance
    end
  }
end

-- This creates a table that simulates an OO subclass.
-- The `defaults` parameter is a table that holds default property values
-- and optional metamethods like `__tostring`.
function M.subclass(baseClass, defaults)
  assert(type(baseClass) == "table", "base class must be a table")
  assert(type(defaults) == "table", "defaults must be a table")

  local metatable = { __index = defaults }
  move_metamethods(defaults, metatable)
  setmetatable(metatable.__index, baseClass.meta)

  return {
    meta = metatable, -- used by subclass function
    new = function(initial)
      local instance = initial or {}
      setmetatable(instance, metatable)
      return instance
    end
  }
end

return M
```

The following code demonstrates using the
`class` and `subclass` functions defined in the "oo" module.

```lua
local oo = require "oo"

Point = oo.class({
  -- Properties
  x = 0,
  y = 0,

  -- Methods
  distanceFromOrigin = function(p)
    return math.sqrt(p.x ^ 2 + p.y ^ 2)
  end,

  -- Metamethods
  __add = function(p1, p2)
    return Point.new({ x = p1.x + p2.x, y = p1.y + p2.y })
  end,
  __tostring = function(p)
    return string.format("(%.2f, %.2f)", p.x, p.y)
  end
})

local p1 = Point.new { x = 3, y = 4 }
print(p1) -- (3.00, 4.00)
print(p1:distanceFromOrigin()) -- 5.0

local p2 = Point.new { x = 5, y = 1 }
local p3 = p1 + p2
print(p3) -- (8.0, 5.0)

local p4 = Point.new({ y = 7 })
print(p4) -- (0.00, 7.00)

Shape = oo.class({
  abstract = true,
  report = function(self)
    print(string.format("%s has %d sides and area %f", self.name, self.sides,
      self:area()))
  end
})

Triangle = oo.subclass(Shape, {
  name = "triangle",
  sides = 3,
  area = function(self) return 0.5 * self.base * self.height end
})
local triangle = Triangle.new({ base = 4, height = 6 })
print("triangle area =", triangle:area()) -- 12.0
triangle:report() -- triangle has 3 sides and area 12.000000

Rectangle = oo.subclass(Shape, {
  name = "rectangle",
  sides = 4,
  area = function(self) return self.width * self.height end
})
local rectangle = Rectangle.new({ width = 4, height = 6 })
rectangle:report() -- rectangle has 4 sides
print("rectangle area = " .. rectangle:area()) -- 24

Square = oo.subclass(Rectangle, {
  name = "square",
  area = function(self) return self.side ^ 2 end
})
local square = Square.new({ side = 5 })
square:report() -- square has 4 sides
print("square area = " .. square:area()) -- 25.0
```

## Metamethods

Metamethods are special methods from a predefined list
that can be defined in a metatable.
Many of metamethods define the functionality of Lua operators
on tables associated with the metatable.

The following metamethods define the operation of mathematical operators.

| Metamethod | Operator |
| ---------- | -------- |
| `__add`    | `+`      |
| `__sub`    | `-`      |
| `__mul`    | `*`      |
| `__div`    | `/`      |
| `__idiv`   | `//`     |
| `__mod`    | `%`      |
| `__pow`    | `^`      |

The following metamethods define the operation of logical operators.

| Metamethod | Operator |
| ---------- | -------- |
| `__eq`     | `==`     |
| `__lt`     | `<`      |
| `__le`     | `<=`     |

There are no metamethods that correspond to the logical operators
`~=`, `>`, and `>=`.
Those are derived from the negation of the other logical operators.

There are no metamethods that correspond to the logical operators
`and`, `or`, and `not`.

The following metamethods define the operation of bitwise operators.

| Metamethod | Operator |
| ---------- | -------- |
| `__band`   | `&`      |
| `__bor`    | `\|`     |
| `__bxor`   | `~`      |
| `__bnot`   | `!`      |
| `__shl`    | `<<`     |
| `__shr`    | `>>`     |

The following metamethods define the operation other operators.

| Metamethod | Operator    |
| ---------- | ----------- |
| `__concat` | `..`        |
| `__len`    | `#`         |
| `__unm`    | `-` (unary) |

Metamethods also support implementing functions
that are called when specific things occur
that are unrelated to operators.

| Metamethod    | Operation                                       |
| ------------- | ----------------------------------------------- |
| `__call`      | called when the table is called like a function |
| `__gc`        | called after garbage collection runs            |
| `__index`     | called if a key is not found in the table       |
| `__metatable` | prevents changes to metatable; see below        |
| `__mode`      | returns a string; see below                     |
| `__newindex`  | called when an entry is added to the table      |
| `__pairs`     | `pairs` function                                |
| `__tostring`  | returns a string representation                 |

The `__index` metamethod is perhaps the most important one
in terms of the breadth of functionality it unlocks.
This was covered in previous sections on "Metatables" and "Classes".

When the `__metatable` function is defined,
the metatable cannot be modified and this returns an error message.

When the `__mode` function returns a string.
When it contains `k`, the keys are weak.
When it contains `v`, the values are weak.
Both can be true.

The `__pairs` method method enables iterating over metatable entries.

TODO: See your metamethods.lua file.

## Evaluting Code at Runtime

The `load` function takes a string or a function.
It returns a function and message, either of which can be `nil`.

When a string is passed it parses it as Lua code and
returns a function that executes the code and `nil` for the message.
If there are errors in the code, it returns
`nil` for the function and a string for an error message.

When a function is passed, the function must return a string of code.
It is called repeatedly until it returns an empty string or `nil`.
The returned strings are concatenated to form the entire code to be compiled.

Here are two contrived examples.

```lua
local name = "World"
local code = "print('Hello, " .. name .. "!')"
local fn = load(code)
if fn then print(fn()) end -- Hello, World!

local n = 0
local last = 4
local function getCode()
  -- local code
  if n == 0 then
    code = "print("
  elseif n < last then
    code = n .. ","
  elseif n == last then
    code = n .. ")"
  elseif n > last then
    code = nil
  end

  n = n + 1
  return code
end

fn = load(getCode)
if fn then print(fn()) end -- 1       2       3       4
```

The `loadfile` function is similar to the `load` function,
but it takes a file path from which code is read.
If no file path is provided, it reads code from `stdin`.

The `dofile` function reads code from a file or `stdin`,
executes it, and returns all the values returned by the code
rather than returning a function to be executed later.

## Lua Functional (luafun)

The <a href="https://www.lua.org/manual/5.4/manual.html#6.6"
target="_blank">table</a> library does not include
functions such as `map`, `filter`, `reduce`, `some`, and `every`.
It's not difficult to write these though.
The following code implements and demonstrates using each of them.

```lua
function map(fn, t)
  local result = {}
  for i, v in ipairs(t) do
    result[i] = fn(v)
  end
  return result
end

local numbers = { 1, 2, 3, 4, 5 }
local function square(n) return n * n end
local squares = map(square, numbers)
print(table.concat(squares, ", ")) -- 1, 4, 9, 16, 25

function filter(fn, t)
  local result = {}
  for _, v in ipairs(t) do
    if fn(v) then
      table.insert(result, v)
    end
  end
  return result
end

local function isEven(n) return n % 2 == 0 end
local evens = filter(isEven, numbers)
print(table.concat(evens, ", ")) -- 2, 4

function reduce(fn, t, initial)
  local acc = initial
  for _, v in ipairs(t) do
    acc = fn(acc, v)
  end
  return acc
end

local function sum(n1, n2) return n1 + n2 end
print(reduce(sum, numbers, 0)) -- 15

function some(fn, t)
  for _, v in ipairs(t) do
    if fn(v) then return true end
  end
  return false
end

function every(fn, t)
  for i, v in ipairs(t) do
    if not fn(v) then return false end
  end
  return true
end

print(some(function(n) return n > 3 end, numbers))  -- true
print(every(function(n) return n < 7 end, numbers)) -- true
```

The functions `map`, `filter`, and `reduce` (and many more)
can be found in the {% aTargetBlank "https://luafun.github.io/",
"Lua Functional Library" %}.

This library is distributed in the single file `fun.lua`.
To download the file, enter
`wget https://raw.github.com/luafun/luafun/master/fun.lua`.

The following code demonstrates basic use of this library:

```lua
local fun = require("fun")

local scores = {7, 4, 13}

-- The `map` and `filter` methods returns an iterator.
-- Calling `:totable()` on an iterator returns a table.

-- Use `map` to double the numbers in a table.
local function double(n) return n * 2 end
doubled_iter = fun.map(double, scores)
print("Doubled Scores")
fun.each(print, doubled_iter) -- 14 8 26

-- Use `filter` to get odd numbers from a table.
local function odd(n) return n % 2 == 1 end
odd_iter = fun.filter(odd, scores)
print("Odd Scores")
fun.each(print, odd_iter) -- 7 3

-- Use `reduce` to sum the numbers in a table.
local function add(a, b) return a + b end
total = fun.reduce(add, 0, scores)
print("Total is " .. total) -- 24

-- Do the same with the `sum` function.
print("Total is " .. fun.sum(scores)) -- 24

-- There are MANY more functions in the luafun library!
```

TODO: How can you use a for loop to iterate over values in an iterator?

## Standard Library

The Lua standard library defines the following modules:

- <a href="https://www.lua.org/manual/5.4/manual.html#6.1" target="_blank">basic</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.2" target="_blank">coroutine</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.10" target="_blank">debug</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.8" target="_blank">io</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.7" target="_blank">math</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.9" target="_blank">os</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.3" target="_blank">package</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.4" target="_blank">string</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.6" target="_blank">table</a>
- <a href="https://www.lua.org/manual/5.4/manual.html#6.5" target="_blank">utf8</a>

Modules in the standard library do not need to be imported to use them.

### basic module

The `basic` module defines the following core Lua functions.

| Function                                      | Description                                                                                                |
| --------------------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| `assert(v [,message])`                        | raises an error if `v` is `false` or `nil`                                                                 |
| `collectgarbage([opt [,arg]])`                | controls the operation of the garbage collector                                                            |
| `dofile([filename])`                          | opens a file containing Lua code and executes it                                                           |
| `error(message [, level])`                    | raises an error that can be caught with `pcall` and `xpcall`                                               |
| `_G`                                          | holds the global environment                                                                               |
| `getmetatable(object)`                        | gets the metatable of a given object                                                                       |
| `ipairs(t)`                                   | returns three values needed to iterate over the indexes and values in an array-like table                  |
| `load(chunk, [, chunkname [, mode [, env]]])` | loads Lua code from a string or function and returns a function that will execute it                       |
| `loadfile([filename [, mode, [, env]]])`      | loads Lua code from a file and returns a function that will execute it                                     |
| `next(table [, index])`                       | gets the next index and value in array-like table                                                          |
| `pairs(t)`                                    | returns three values needed to iterate over the keys and values in a dictionary-like table                 |
| `pcall(f, [, arg1, ...])`                     | takes a function and arguments to pass to it; performs a "protected call" that catches errors              |
| `print(...)`                                  | takes any number of arguments and sends their `tostring` values to `stdout`                                |
| `rawequal(v1, v2)`                            | compares values without using their `__eq` metamethods                                                     |
| `rawget(table, index)`                        | gets the value at a given table index without using the `__index` metamethod                               |
| `rawlen(v)`                                   | gets the length of a value without using its `__len` metamethod                                            |
| `rawset(table, index, value)`                 | sets the value at a given table index without using the `__newindex` metamethod                            |
| `select(index, ...)`                          | returns a subset of its arguments starting at a given index or the number of arguments if the first is `#` |
| `setmetatable(table, metatable)`              | sets the metatable of a given table                                                                        |
| `tonumber(e [, base])`                        | converts a string to a number in a given base (defaults to 10)                                             |
| `tostring(v)`                                 | converts a value to a string                                                                               |
| `type(v)`                                     | returns the type of a value as a string                                                                    |
| `_VERSION`                                    | a global variable that holds a string that describes the current Lua version (ex. "Lua 5.4")               |
| `warn(msg1, ...)`                             | writes a warning message to `stdout` that constructed by concatenating its string arguments                |
| `xpcall(f, msgh, [, arg1, ...])`              | similar to `pcall`, but takes a message handler that can be used to capture additional error information   |

### debug module

TODO: Add detail on this module.

### io Module

The following code shows the most basic way to write to a new file.
If the file already exists, it is overwritten.

```lua
-- Open a file in text mode and make it the default output file.
-- If `file_path` is omitted, this just
-- returns the current default output file, if any.
io.output(file_path)

-- Write to the default output file.
io.write(some_string)

-- Close the default output file.
io.close()
```

The following code shows the most basic way to read from a file.

```lua
-- Open a file in text mode and make it the default input file.
-- If `file_path` is omitted, this just
-- returns the current default input file, if any.
io.input(file_path)

-- Read from the default input file.
-- TODO: What arguments does this accept and what do they do?
local data = io.read()

-- Get and close the default input file.
io.input():close()
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

To open a file in a specific mode:

```lua
local stream = io.open(file_path, mode) -- mode defaults to "r"
```

To write to a file using the method syntax:

```lua
stream:write(data)
```

The following code demonstrates many ways to read from a stream.

```lua
contents = stream:read("a") -- reads entire contents

line = stream:read("l") -- reads next line, discarding newline
line = stream:read("L") -- reads next line, keeiping newline

number = stream:read("n") -- reads one number

n1, n2 = stream:read("n", "n") -- reads two numbers

text = stream:read(n) -- reads a string of up to "n" bytes

end_test = stream:read(0) -- returns nil if at end of file; otherwise ""
```

To seek to a specific byte offset in a stream:

```lua
stream:seek("set", offset)
```

To close a stream:

```lua
stream:close()
```

### math module

Constants defined by this module include:

| Constant          | Meaning                                            |
| ----------------- | -------------------------------------------------- |
| `math.huge`       | floating point value greater than any other number |
| `math.maxinteger` | maximum integer value                              |
| `math.mininteger` | minimum integer value                              |
| `math.pi`         | value of π                                         |

A constant for `e` is not defined, but it can be obtained from `math.exp(1)`.

Trigonometry functions defined by this module include
`sin`, `cos`, `tan`, `asin`, `acos`, and `atan`.
All of these take or return angles in radians.
To convert an angle from degrees to radians, use the `math.rad(x)` function.
To convert an angle from radians to degrees, use the `math.deg(x)` function.

To generate random numbers, use the
`math.randomseed` and `math.random` functions.
For example:

- `math.randomseed(os.time())` seeds the random number generator
- `math.random()` returns a floating point number in the range [0, 1)
- `math.random(10)` returns an integer in the range [1, 10]
- `math.random(5, 10)` returns an integer in the range [5, 10]

Other functions defined in this module include:

| Function              | Returns                                                               |
| --------------------- | --------------------------------------------------------------------- |
| `math.abs(x)`         | absolute value                                                        |
| `math.ceil(x)`        | ceiling (rounds up)                                                   |
| `math.exp(x)`         | e raised to the x power                                               |
| `math.floor(x)`       | floor (rounds up)                                                     |
| `math.fmod(x, y)`     | floating point remainder of x / y                                     |
| `math.log(x, [base])` | logarithm of x with specified base or e                               |
| `math.max(...)`       | maximum of a set of numbers                                           |
| `math.min(...)`       | minimum of a set of numbers                                           |
| `math.modf(x)`        | integral and fractional parts of a floating point number              |
| `math.sqrt(x)`        | square root                                                           |
| `math.tointeger(x)`   | integer value if x can be converted to an integer; otherwise `nil`    |
| `math.type(x)`        | `"integer"`, `"float"`, or `nil`                                      |
| `math.ult(m, n)`      | `true` if m < n when compared as unsigned integers; otherwise `false` |

`math.modf(3.14)` returns `3` and `0.14`

The value passed to `math.tointeger` can be a number or string.
For example:

- `math.tointeger(3.0)` and `math.tointeger("3.0")` both return `3`
- `math.tointeger(3.1)` returns `nil`
  because it cannot be converted to an integer

### os Module

The `os` module defines the following functions:

| Function                                      | Purpose                                                                                           |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| `os.time()`                                   | returns seconds since 1970 or a given date/time                                                   |
| `os.difftime()`                               | returns difference in seconds between two times                                                   |
| `os.date()`                                   | returns a string describing the current date and time                                             |
| `os.exit(code)`                               | code can be true (exits with EXIT_SUCCESS; default), false (exits with EXIT_FAILURE), or a number |
| `os.getenv("environment-variable-name")`      | returns the value of an environment variables                                                     |
| `os.rename(current_file_path, new_file_path)` | renames a file or directory                                                                       |
| `os.remove(current_file_path)`                | deletes a file or empty directory                                                                 |
| `os.execute(shell_command)`                   | executes a shell command                                                                          |

The `os.date` function takes a format string
and an optional time that defaults to now.
The format string uses the same characters as the C {% aTargetBlank
"https://man7.org/linux/man-pages/man3/strftime.3.html", "strftime" %} function
For example:

```lua
now = os.date()
print(now) -- Fri Apr  7 03:09:52 2023
print(type(now)) -- string

time = os.time({
  year = 1961,
  month = 4,
  day = 16,
  hour = 10,
  min = 20,
  sec = 19,
  isdst = true -- daylight savings time
})
print(time) -- -274891181; negative because it is before 1970
print(type(time)) -- number

format = "%A, %B %e, %Y"
print(os.date(format, time)) -- Sunday, April 16, 1961
```

To get the time required to run some code:

```lua
local start = os.clock()
-— Add some code here.
print(os.clock() - start)
```

### package Module

TODO: Add details about the `package module.

### string Module

String operations supported by this module include the following,
all using method syntax where `s` is a string:

| Function                             | Description                                                   |
| ------------------------------------ | ------------------------------------------------------------- |
| `s:len()` or `#s`                    | returns length in bytes                                       |
| `s:sub(startIndex, endIndex)`        | returns a substring                                           |
| `s:gsub(source, oldValue, newValue)` | performs global substitution                                  |
| `s:find(target)`                     | returns start and end index inclusive                         |
| `s:upper()`                          | returns uppercase version                                     |
| `s:lower()`                          | returns lowercase version                                     |
| `s:match('pattern', i=1)`            | returns first match found in s starting at index i            |
| `s:gmatch('pattern', i=1)`           | returns iterator over all matches found in s start at index i |

The `gsub` function returns a new string and
the number of occurrences that were replaced.

The following code demonstrates using the `gmatch` function
to find all the integers in a string:

```lua
local s = "apple 19 banana 7 cherry 21"
local matches = s:gmatch("%d+")
for match in matches do
  print(match) -- 19, 7, and 21
end
```

Some common string operations are not directly supported.
For example, there are no functions that test whether a string
contains, starts with, or ends with a substring.
We can define these using the provided `find` function
as follows:

```lua
function string.contains(source, target)
  return source:find(target, 1, true) ~= nil
end

function string.startsWith(source, target)
  return source:find(target, 1, true) == 1
end

function string.endsWith(source, target)
  startIndex, endIndex = source:find(target, 1, true)
  return endIndex == #source
end

text = "This is used to test of our new functions."
```

The `contains` function defined above can be called in two ways.

```lua
print(string.contains(text, "test")) -- true
print(text:contains("test")) -- true
```

Here are more examples of calling the functions defined above
using the method syntax.

```lua
print(text:contains("missing")) -- false
print(text:startsWith("This")) -- true
print(text:startsWith("Test")) -- false
print(text:endsWith("functions.")) -- true
print(text:endsWith("test.")) -- false
```

Lua does not support string interpolation.
The closest Lua feature to this is the `string.format` function.
This takes a format string as its first argument.
This can contain literal text and
formatting directives that begin with a percent sign.

The supported formatting directives are:

- `%o` for octal numbers
- `%d` for decimal numbers
- `%x` for hexadecimal numbers
- `%f` for floating point numbers
- `%s` for strings

For example:

```lua
name = "Mark"
color = "yellow"
sentence = string.format("%s's favorite color is %s.", name, color)
```

The `%f` directive can specify the number of decimal places to output.
For example:

```lua
-- Print pi to four decimal places.
print(string.format("pi is %.4f", math.pi)) -- pi is 3.1416
```

### table module

TODO: Add detail on this module.

## Environments

"Free variables" are variables that are not declared with the `local` keyword.
They are stored in the current "environment"
which is a table referred to with the name `_ENV`.
By default the value of `_ENV` is the same as `_G`.
For example:

```lua
fruit = "apple"
local fruit = "banana"
print(fruit) -- banana
print(_ENV.fruit) -- apple
print(_G.fruit) -- apple
```

To see all the keys defined in the current environment,
execute the following code.
This typically includes standard libraries such as `coroutine`, `debug`,
`io`, `math`, `os`, `string`, `table`, and `utf8`.
It also typically includes functions and variables defined in the
`basic` standard library such as `assert`, `print`, `pairs`, `ipairs`,
`pcall`, `getmetatable`, `setmetatable`, and `_G`.

```lua
for k, v in pairs(_ENV) do
  print(k)
end
```

A function or `do` block can change its environment.
Doing so changes where free variables are stored.
This can be useful to avoid changing variables in the outer scope.
For example:

```lua
fruit = "apple"

function demo1()
  -- Create a new environment that is only used by this function.
  -- It starts empty and looks in the global environment
  -- for anything it doesn't have.
  -- We can't set _ENV to {} here because that
  -- would prevent us from calling setmetatable.
  env = {}
  setmetatable(env, {__index = _G})
  -- If `local` is not used here, the value of
  -- `_ENV` outside this function will be changed.
  local _ENV = env

  -- Assign a variable in the new environment.
  fruit = "banana"

  -- The print function is not in the new environment,
  -- so the version in the global environment will be used.
  print(fruit) -- banana

  -- Functions called from here will use the global _ENV
  -- rather the local one we created above
  -- unless we pass it.
  demo2(_ENV)
end

function demo2(_ENV)
  print(fruit) -- banana
end

demo1()
print(fruit) -- apple
```

## Coroutines

Lua is single-threaded like JavaScript.
It supports collaborative multitasking with coroutines.
At any point in time on only one coroutine is running.
Coroutines do not run in parallel.

The following code demonstrates using a coroutine
to generate integers in given multiples
stopping at a given limit.

The `coroutine.create` function returns a value with the type `thread`,
but it is a coroutine instance, not a thread in the usual sense.

The `coroutine.resume` function takes a thread and]
arguments to be passed to the coroutine function.
It returns a boolean indicating if it was successful
and any values passed to `coroutine.yield`.

```lua
local function nextNumber(delta, limit, previous)
  local next = (previous or 0) + delta
  if next <= limit then
    coroutine.yield(next)
    nextNumber(delta, limit, next) -- recursive call
  end
end

local thread = coroutine.create(nextNumber)
print(type(thread)) -- thread
print(coroutine.status(thread)) -- "suspended"

-- We only need to pass arguments in the
-- first call to `resume` for this coroutine.
local success, v = coroutine.resume(thread, 3, 15)

while success and v do
  print(v) -- 3, 6, 9, 12, and 15
  success, v = coroutine.resume(thread)
end

print(coroutine.status(thread)) -- "dead"
```

The `coroutine.wrap` function provides an alternative to `coroutine.create`.
Rather than returning a `thread`, `coroutine.wrap` returns a function
than can be called to resume the `thread`.
Rather than returning a boolean status and the values passed to `coroutine.yield`,
it only returns the values passed to `coroutine.yield`,

The following code re-implements the previous example
to use `coroutine.wrap` instead of `coroutine.create`.
The `nextNumber` function remains unchanged.

```lua
local iterator = coroutine.wrap(nextNumber)

print(type(iterator)) -- function

local v = iterator(3, 15)
while v do
  print(v) -- 3, 6, 9, 12, and 15
  v = iterator()
end
```

Lua does not directly support concurrently running threads,
but they can be implemented using the approach described in
chapter 26 of the "Programming in Lua - Fourth edition" book.
There are many Lua libraries that support multithreading including
{% aTargetBlank "http://lualanes.github.io/lanes/", "Lua Lanes" %}.

Lua does not have the equivalent of the `async` and `await` keywords
in other programming languages, but those can be simulated.
See {% aTargetBlank "https://github.com/iamcco/async-await.lua",
"async-await.lua" %}.

For sending HTTP requests, see the "Networking" section below.

## Networking

There are several popular Lua networking libraries.

- {% aTargetBlank "http://openresty.org/en/", "OpenResty" %} is
  "a dynamic web platform based on NGINX and LuaJIT."
- {% aTargetBlank "https://github.com/daurnimator/lua-http", "lua-http" %} -
  supports implementing HTTP servers and clients
- {% aTargetBlank "https://lunarmodules.github.io/luasocket/", "LuaSocket" %} -
  "a C core that provides support for the TCP and UDP transport layers, and
  a set of Lua modules that add support for functionality
  commonly needed by applications that deal with the Internet"
- {% aTargetBlank "https://luvit.io", "Luvit" %} -
  "asynchronous I/O for Lua; implements the same APIs as Node.js"

## Utility Functions

The following functions are helpful for debugging.
They can be defined in a file like `utility.lua`
and required where needed.

```lua
local M = {}

-- Returns a string description of the keys and values in a table.
-- Values can be nested tables.
function M.dump(value)
  if type(value) ~= "table" then
    return tostring(value)
  end

  local s = "{ "
  for k, v in pairs(value) do
    if type(k) ~= "number" then
      k = "\"" .. k .. "\""
    end
    s = s .. k .. "=" .. mod.dump(v) .. ", " -- recursive
  end
  if #s > 2 then s = s:sub(1, -3) end        -- removes last comma and space
  return s .. " }"
end

-- Returns a string containing all the values in a table,
-- each separated by a comma and a space.
-- Values cannot be nested tables.
function M.valuesString(obj)
  if type(obj) ~= "table" then
    return ""
  end

  s = ""
  for index, v in ipairs(obj) do
    s = s .. v .. ", "
  end
  return s:sub(1, -3)
end

return M
```

## Error Handling

Lua does not have a mechanism for throwing and catching exceptions.
In many cases errors result in a function returning the value `nil`
and it is left to developers to check for `nil` values.
Failing to do so can result in programs crashing
and outputting a stack trace.

The only error handling mechanism Lua provides are the
`pcall` and `xpcall` functions (short for "protected call").
These can be compared to a try block in other programming languages.

The `pcall` function is passed a function to execute and
optionally arguments to be passed to it.
It returns a boolean indicating whether the call completed without error
and an error message if one did occur.

The `xpcall` function is similar, but accepts a message handler function
that can be used to capture additional information about the error.

The `error` function is the Lua equivalent of
a `throw` statement in other programming languages.
It is passed a message and an optional integer error level.
The message can be any type, but is typically a string or a table.

The following code demonstrates using the `pcall` function.
It repeatedly prompts for a dividend and a divisor
and displays their quotient.
When invalid values are entered, error messages are output,
but the program does not crash.

```lua
local function read_number(prompt)
  io.write(prompt .. ": ")
  local number = io.read("*number")
  -- The previous line does not consume the newline character.
  -- Unless that is done, the next attempt to read a number will return `nil`.
  -- The following line consumes the newline character.
  local _ = io.read()
  return number
end

local function process()
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

## C Integration

A common reason to integrate Lua with C is to
use Lua as a configuration language.
This has many advantages over other configuration options
such as JSON and Python.

- clean, minimal syntax
- can include comments
- can be dynamic, using data such as environment variables
- can write simple code to compute values

It is common in Lua configuration files to expose values as global variables.
This is likely the reason that variables are global by default.

The Lua interpreter is small, only around 250 kb.
The total size of the Lua standard libraries is around 500 kb.
These are combined into the file `liblua.a`.

Download the source for Lua and build it
by following these steps:

- Download the Lua source code from {% aTargetBlank
  "https://www.lua.org/download.html", "Lua Download" %}.
- Unzip and untar the downloaded file.
- `cd` to the resulting directory.
- Enter `make all test`
- Enter `sudo make install` which will:
  - install the `lua` and `luac` executables in `/usr/local/bin`
  - copy the header files `lua.h`, `luaconf.h`, `lualib.h`,
    `lauxlib.h`, and `lua.hpp` in `/usr/local/include`
  - copy the library `liblua.a` to `/usr/local/lib`
  - copy the man pages for `lua` and `luac` to `/usr/local/man/man1`
- If compiling and linking with gcc (see below) results in
  the error "ignoring file /usr/local/lib/liblua.dylib,
  it may be necessary to remove the dynamic library by entering
  `rm /usr/local/lib/liblua.dylib`
  This will cause gcc to use `/usr/local/lib/liblua.a` instead
  building for macOS-arm64 but attempting to link with
  file built for macOS-x86_64".

For an example of embedding the Lua interpreter in a C application, see
the GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/lua-examples/tree/main/c-calls-lua",
"c-calls-lua" %}.

To compile and link the C program, enter:

```bash
gcc main.c -o main -llua
```

Run the executable by entering `./main`

For an example of embedding the Lua interpreter in a SwiftUI application, see
{% aTargetBlank "/blog/topics/#/blog/swift/CallingC", "Swift Calling C" %} and
the GitHub respository {% aTargetBlank
"https://github.com/mvolkmann/SwiftUICallsC", "SwiftUICallsC" %}.

To restrict what loaded Lua code is able to do,
only load a subset of the standard libraries.
Copy the following code from the Lua source file `linit.c`
and call `openlibs` in place of `luaL_openlibs`.

```c
static const luaL_Reg loadedlibs[] = {
  // This loads the "basic" standard library into the global environment.
  // It includes functions like pairs, ipairs, print,
  // tonumber, tostring, setmetatable, and getmetatable
  {LUA_GNAME, luaopen_base},

  // Comment out any of these to prevent their use.
  // It seems the order of these lines does not matter.
  {LUA_LOADLIBNAME, luaopen_package},
  {LUA_COLIBNAME, luaopen_coroutine},
  {LUA_TABLIBNAME, luaopen_table},
  {LUA_IOLIBNAME, luaopen_io},
  {LUA_OSLIBNAME, luaopen_os},
  {LUA_STRLIBNAME, luaopen_string},
  {LUA_MATHLIBNAME, luaopen_math},
  {LUA_UTF8LIBNAME, luaopen_utf8},
  {LUA_DBLIBNAME, luaopen_debug},
  {NULL, NULL} // marks end for for loop in openlibs below
};

LUALIB_API void openlibs(lua_State *L) {
  const luaL_Reg *lib;
  for (lib = loadedlibs; lib->func; lib++) {
    luaL_requiref(L, lib->name, lib->func, 1);
    lua_pop(L, 1);  /* remove lib */
  }

  // Assuming the io library is loaded,
  // remove the ability to change the default output file.
  // It will remain set to stdout, so `io.write` can be
  // used to write to stdout, but not to a file.
  getGlobalTable("io");
  setTableKeyValue("output", 0);
}
```

It is probably possible to load a standard library and
then disable some of its functions by setting them to `nil`.
For example, it may be desirable to allow reading files,
but not writing them.
TODO: Try this.

TODO: Add detail on calling C from Lua.
TODO: Add detail on calling Lua from C.

TODO: Are there two stacks, one for data going from C to Lua
TODO: and one for data going from Lua to C?

Calling `lua_openLibs` makes the standard library functions available.
Lua code that does not call those can be executed without this.

TODO: Write utility functions that get values of specific types from Lua code
TODO: and do all the error checking.

Before `lua_pcall` pushes return values onto the stack it
remove the function and its arguments from the stack.

See {% aTargetBlank
"http://www.troubleshooters.com/codecorn/lua/lua_c_calls_lua.htm",
"Calling Lua From a C Program" %}.

TODO: How can you prevent Lua code executed from C from
TODO: doing something dangerous like deleting files?

For calling Lua from JavaScript,
see https://daurnimator.github.io/lua.vm.js/lua.vm.js.html
and https://github.com/Doridian/LuaJS.

For calling Lua from Swift, see
https://www.larsgregori.de/2019/12/27/lua-and-swift-in-ios/.

## Games

### LÖVE

{% aTargetBlank "https://love2d.org", "LÖVE" %}
is a Lua framework for building 2D games.
It is free and open source.
LÖVE can be downloaded from the previous link.

For a great YouTube video on the LÖVE framework, see {% aTargetBlank
"https://www.youtube.com/watch?v=3k4CMAaNCuk&t=3309s",
"Falling in LÖVE with Lua" %}. In the video Colton Ogden
walks through the beginnings of writing a Super Mario game.
The source code is available at {% aTargetBlank
"https://github.com/coltonoscopy/cs502019games-track/tree/master/mario",
"cs502019games-track" %}.

For macOS:

- Click the "64-bit zipped" link under "macOS" to download `love.app`.
- Drag this file into the "Applications" directory.
- Double-click `love.app` to launch the app.
  This will fail the first time with the message
  "love.app cannot be opened because the developer cannot be verified".
  To fix this, open the Settings app, select "Privacy & Security",
  scroll down to "love.app was blocked ..." and click the "Open Anyway" button.

TODO: Does Love2D include its own version of Lua
TODO: and ignore the installed version?

To get started creating a game:

- Create a directory for a new game.
- Create a file in this directory named "main.lua".

If using VS Code:

- Install the extension "Love2D Support" from Pixelbyte Studios.
- Click the gear icon and select "Extension Settings".
- Change "Pixelbyte > love2d: Path" to
  "/Applications/love.app/Contents/MacOS/love".
- Open a `main.lua` file in an editor tab.
- Press cmd-l to run the game.

Love2D programs always define the functions
`love.load()`, `love.draw()`, and `love.update(dt)`.
The `love.load()` function performs initial game setup.
The `love.draw()` function specifies what should be
drawn on the screen at any point in time.
The `love.update(dt)` function implements the game logic.

The parameter `dt` in the `love.update` function is short for "delta time".
This is used to make game updates frame rate independent.
It is a floating point number that indicates
the number of seconds requires to display each frame.
This value can vary among devices.
For example, when `dt` is `0.1`, the device displays 10 frames per second.

To configure a game, add a `conf.lua` file to your game project directory.
For example:

```lua
function love.conf(t)
  t.title = "My Game"
  t.version = "1.2.3"
  t.console = true
  t.window.width = 1280
  t.window.height = 720
end
```

To run a game, use one of these approaches:

- Drag the game directory onto the Love application icon.
- If VS Code has been configured property, press cmd-l.
- TODO: What other options are available?

When comparing the distance between two points to some value,
compare the square of the distance.
This removes the need to use the `math.sqrt` function
which can hurt game performance.

TODO: See lua/love/love-game/main.lua.

### LÖVR

{% aTargetBlank "https://lovr.org", "LÖVR" %} is
"an open source framework for rapidly building immersive 3D experiences."

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

Are there recommended Lua linters and code formatters that run outside of VS Code?

What is the difference between these?
my_var = 1 — Is this only global within the current source file?
\_G.my_var = 1 — Does this make it accessible outside the current source file?

Functions can call themselves recursively.

Did you describe the fact that there is no compact syntax for defining them?

Coroutine status values include running, suspended, normal, and dead (computed or stopped due to error).
coroutine.resume(my_coroutine) — starts or resumes a coroutine

Can a Lua program create multiple threads that run concurrently?
What happens if you call coroutine.resume on a coroutine that has a status of dead?

The things in the standard library are “modules”, not “libraries”.
io.read(…) returns nil if there is no more file content to read.
There are addition file mode values that include “b” to work with binary files.

There are probably many variable assignments in the
examples here that should be preceded by `local`.

OO-like objects hold their properties and methods in a table.
Does each object get its own copy of every method?

you can use the or operator to avoid setting instance values to know when they are missing in the passed table.
but beware of cases where are the table passed in contains false values because those will not be used if you simply check with the or operator.

this video suggest multiple ways to handle this: https://youtu.be/IQf82d3cr20

In the table section, describe copying tables like the following.
Is there a way to make a deep copy? Maybe something recursive?

```lua
t = {"alpha", {"beta", "gamma"}}
for k, v in pairs(t) do
  print(k, v)
end

-- This makes a shallow copy.
copy = {table.unpack(t)}
for k, v in pairs(copy) do
  print(k, v)
end

-- This wraps the approach above in a function.
function table.copy(source)
  return {table.unpack(source)}
end
copy2 = table.copy(t)
for k, v in pairs(copy2) do
  print(k, v)
end
```

Reorder some of the sections above so no section relies on
information found in a section that follows it (if possible).
