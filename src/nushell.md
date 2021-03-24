---
eleventyNavigation:
  key: Nushell
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Overview

{% aTargetBlank "https://www.nushell.sh", "Nushell" %} is "a new type of shell".
"The goal of this project is to take the Unix philosophy of shells,
where pipes connect simple commands together,
and bring it to the modern style of development."
Nushell was created by Jonathan Turner, Yehuda Katz, and Andres Robalino.
It runs in Linux, macOS, and Windows.

Nushell is implemented in Rust.
And like Rust, Nushell produces helpful, nicely formatted error messages.

Nushell includes the Nu language which excels at
"processing streams of structured data".
This provides features that are somewhat like SQL for databases.
It also has similarities to the Python-based Pandas data analysis library.
These are features not typically seen in shell environments.
It is far from a toy project and uses recursive descent parser
that is driven by its many supported data types.

Nushell continues the UNIX tradition of
commands whose input can be piped in from a previous command
and whose output can be piped to a subsequent command.
This is done in a streaming fashion so that a command
does not have to run to completion before the next command
in the pipeline can begin receiving that output as its input.

Some commands, such as `echo` and `ls`, are lazy.
This means they do not produce output unless
something is requesting data from their output stream.
One way to do this it to pipe their output to the `autoview` command
which determines how to render the data based on its type.
Piping to `autoview` occurs implicitly in the shell
after the last command in a pipeline.
So the command `ls` is processed as if `ls | autoview` was entered.
When semicolons are used to separate multiple commands on the same line,
`autoview` is only applied to the last pipeline.
For example, `let a = 2; let b = 3; = $a + $b` outputs `5`.

The Nu language can be used outside Nushell, such as in Jupyter Notebooks.
Because Rust is a great source for compiling to WebAssembly, it was
possible to implement a web-based environment for experimenting with Nushell.
This can be found at
{% aTargetBlank "https://www.nushell.sh/demo/", "Nushell demo" %}.

It costs nothing but some disk space to try it (about 50 MB).
You don't have to commit to making it your default shell.
Just pop in periodically to try it and exit to return to your current shell.
Over time you may decide you like it enough to make it your default shell.

Color coding is applied to commands as they are typed.
If a command becomes invalid, all the text changes to red.

## Terminology

Nushell commands are often chained together using the pipe (|) character.
The output of the command on the left of a pipe
is used as the input of the command on the right.
Nushell refers to these sequences of commands as "pipelines".

Command pipelines entered in the shell must be on a single line,
but inside scripts lines that end with `|` continue on the next line.
Nushell does not support using `\` as a line continuation character
like in other shells such as Bash.

Commands that take no input and produce output
are referred to as "inputs" or "sources".
An example is the `open` command.

Commands that take input and transform it are referred to as "filters".
Examples include the `where` and `sort-by` commands.

Commands that take input and display it or write it somewhere such as file
are referred to as "outputs" or "sinks".
Examples include `autoview` and `save`.

## Installing

There are many options for installing the Nushell.
If you have Rust installed, enter `cargo install nu`.
This takes a long time to complete.
If you are on macOS and have Homebrew installed, enter `brew install nushell`.
Note that Homebrew may install a version
that is several versions behind the latest.
Prebuilt binaries for Linux, macOS, and Windows can also be downloaded.
For more information on installation options, click the Nushell link above.

## Getting Started

To start the shell, enter `nu`.

For help, enter `help`.
For a list of supported commands, enter `help commands`.
In version 0.28.0 there are 105 Nushell commands.
For help on a specific command,
enter `help {command-name}` or {command-name} -h`.

When a command not defined by Nushell is encountered,
the directories listed in the `path` configuration setting
are searched to find a matching executable.
To run a command in the `path` that happens to have the same name
as a Nushell command, prefix the command name with `^`.
For example, `^ls *.html`.

Like all shells, enter commands and press enter to execute them.
Multi-line commands can be entered by pressing enter
before a block, delimited by square brackets or curly braces, is complete.
Nushell has great command recall and completion like the Fish and zsh shells.
Command recall even supports multi-line command editing.

For detailed documentation, see the
{% aTargetBlank "https://www.nushell.sh/book/", "Book" %}
link in the top nav of the website.

For more help, join the {% aTargetBlank
"https://discord.gg/NtAbbGn", "nushell Discord channel" %}.

## Configuration

The configuration for Nushell is stored in a TOML file
whose path can be obtained by entering `config path`.
Configuration settings can be changed by editing this file
or using the `config` subcommands described below.
To edit the config file with Vim, enter `vim $(config path)`.
To edit the config file with VS Code, enter `code $(config path)`.
Changes to the config file take effect immediately in the current shell session.

Some Nushell configuration settings are top-level
and appear in a specific TOML section.
Notable top-level options include:

| Setting                 | Description                                                                                       |
| ----------------------- | ------------------------------------------------------------------------------------------------- |
| `disable_table_indexes` | when `true`, omits index column from table output                                                 |
| `path`                  | quoted list of directories                                                                        |
| `prompt`                | command whose output is used for the prompt                                                       |
| `skip_welcome_message`  | when `true`, starting a shell doesn't output<br>welcome message including Nushell version         |
| `startup`               | list of commands to execute when a shell starts;<br>typically defines aliases and custom commands |
| `table_mode`            | controls the border lines drawn when tables are rendered<br>(more detail below)                   |

For details on configuration options, see {% aTargetBlank
"https://www.nushell.sh/book/configuration.html", "Configuration" %}.

The `config` command supports many subcommands that operate on the config file.
These are summarized in the table below.

| Command                                | Description                                     |
| -------------------------------------- | ----------------------------------------------- |
| `config`                               | outputs all the settings                        |
| `config path`                          | outputs the file path of the configuration file |
| `config set {name} {value}`            | sets or updates a specific setting              |
| `{pipeline} \| config set_into {name}` | sets a specific setting to a piped-in value     |
| `config get {name}`                    | gets a specific setting                         |
| `config remove {name}`                 | removes a specific setting                      |
| `config load {file-path}`              | loads settings from a file                      |

Note that changes to the config file at `$(config path)`
take effect immediately. Using `config load` is not required.

Configuration changes affect future shell sessions, not the current one.

For example, to change the prompt enter
`config set prompt "echo $(ansi yellow) '🦀ν> '"`.
This adds the line `prompt = "echo $(ansi yellow) '🦀ν> '"`
to the configuration file.
🦀 is for the Rust mascot Ferris and ν is the Greek letter nu.
Here is a fancier prompt setting that includes
the current working directory in yellow,
and a right-pointing triangle at the end:

{% raw %}

```bash
prompt = "echo `🦀{{$(ansi yellow)}}ν {{$(pwd)}}{{$(char prompt)}} `"
```

{% endraw %}

To output the value of each setting in the config file, enter `config`.

Another option for customizing the prompt is
to enable the use of [Starship](/blog/starship),
which has many more options.
It also has the advantage that it can be used with nearly any shell,
which is great for users that sometimes switch between shells.

### startup Setting

The `startup` setting specifies a list of commands
to run each time a new Nushell session is started.
Typically the commands define aliases and custom commands (using `def`).
For example:

```toml
startup = [
  "alias cls = clear",
  "def cdjs [] { cd $nu.env.JS_DIR }",
  "def cdrust [] { cd $nu.env.RUST_DIR }"
]
```

Aliases that use the "cd" command above currently causes Nushell to crash.
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3138",
"issue 3138" %}.

### table_mode Setting

Table borders can be customized with the `table_mode` setting.
Supported values include:

- basic: uses +, -, and | characters for borders
- compact: omits outer left and right borders
- compact_double: like compact, but uses double lines for borders
- heavy: thicker border on every cell
- light: horizontal line below header and no other borders
- none: no borders
- reinforced: border on every cell with thick table corners
- rounded: border on every cell and rounded table corners (my favorite)
- thin: border on every cell
- with_love: heart characters for borders

### color_config Section

The colors used to output data of each
table element and data type can be customized
by adding a `[color_config]` section in the config file.
Supported colors include
red, yellow, green, cyan, blue, purple, white, and black.
Supported modifiers include bold, underline, italic, dimmed, and reverse.
Colors and modifiers can be specified with their full names,
separated by an underscore. For example, `yellow_bold`.
They can also be abbreviated using only their first letters.
For example, `yb`.
The only exception is blue which uses the letter "u"
because "b" is used for black.

The header elements that can be configured include
header_align, header_bold, header_color, index_color,
leading_trailing_space_bg, and separator_color (used for table lines).
The types that can be configured include
primitive_binary, primitive_boolean, primitive_columnpath, primitive_date,
primitive_decimal, primitive_duration, primitive_filesize, primitive_int,
primitive_line, primitive_path, primitive_pattern, primitive_range, and
primitive_string

For example:

```toml
[color_config]
header_align = "l" # left|l, right|r, center|c
header_bold = true
header_color = "r"
index_color = "wd"
separator_color = "wd" # for table lines

primitive_binary = "c"
primitive_boolean = "p"
primitive_date = "u"
primitive_decimal = "g"
primitive_filesize = "g"
primitive_int = "g"
primitive_path = "y"
primitive_string = "w"
```

### line_editor Section

Nushell line editing is provided by
{% aTargetBlank "https://crates.io/crates/rustyline", "rustyline" %}.
It can be configured with the "[line_editor]" section.
Notable settings include:

| Setting                | Description                                                                                                                           |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `edit_mode`            | `"emacs"` or `"vi"`<br>default is to use {% aTargetBlank "https://github.com/kkawakam/rustyline#actions", "rustyline key bindings" %} |
| `history_duplicates`   | `"alwaysadd"` or `"ignoreconsecutive"` (default)                                                                                      |
| `history_ignore_space` | `true` (default) or `false`; doesn't add commands with leading whitespace to history                                                  |

For example:

```toml
[line_editor]
edit_mode = "vi" # or "emacs"; omit for default keystrokes (see rustyline docs)
history_ignore_space = true # omits whitespace around commands saved in history?
```

### path setting

The `path` setting lists directories to be searched for executables.

To see a nicely formatted list of directories in your path,
enter `echo $nu.path` or `config | get path`.

The command `config set path $nu.path`
sets the `path` setting to the value of `$nu.path`,
which is the value of the `PATH` environment variable in the parent shell.
This is useful because it also enables
setting a path that is specific to Nushell.
Note that if Nushell is your login shell then
there is no parent shell from which to inherit a path.

### env Section

The command `config set env $nu.env` adds all the current
environment variables in the `env` section of the config file.
This section can also define environment variables
that are specific to Nushell.
Note that if Nushell is your login shell then
there is no parent shell from which to inherit environment variables.

### textview Section

These settings affect use of the `bat` crate for viewing text files
using the `open` command.

| Setting                  | Description                                 |
| ------------------------ | ------------------------------------------- |
| grid                     | `true` (default) or `false`                 |
| header                   | `true` (default) or `false`                 |
| line_numbers             | `true` (default) or `false`                 |
| theme                    | ex. "Coldark-Dark"                          |
| true_color               | `true` or `false`                           |
| vcs_modification_markers | `true` or `false` (seems to have no effect) |

To see the supported themes, install `bat` by entering `cargo install bat`
and enter `bat --list-themes`. There are over 20.

## Data Types

Unlike most shells where only strings are used for command input and output,
Nushell supports many primitive and structured data types.

| Type        | Description                                                                                    |
| ----------- | ---------------------------------------------------------------------------------------------- |
| `any`       | any type below (default)                                                                       |
| `binary`    | sequence of raw bytes                                                                          |
| `block`     | block of nu script code that can be executed on each row of a table                            |
| `boolean`   | literal values are `$true` and `$false`                                                        |
| column path | dot-separated list of nested column names                                                      |
| `date`      | timezone-aware; defaults to UTC                                                                |
| `decimal`   | numbers with a fractional part and infinite precision                                          |
| `duration`  | number followed by a unit which can be `ms`, `sec`, `min`, `hr`, `day`, `wk`, `mon`, or `yr`   |
| `filesize`  | number followed by a unit which can be `b`, `kb`, `mb`, `gb`, `tb`, or `pb`                    |
| `group`     | semicolon-separated list of pipelines that can be run in parallel?                             |
| `int`       | whole numbers with infinite precision                                                          |
| `line`      | a string with an OS-dependent line ending                                                      |
| list        | sequence of values of any type                                                                 |
| `number`    | `int` or `decimal`, both with infinite precision                                               |
| `path`      | platform-independent path to a file or directory                                               |
| `pattern`   | glob pattern that can include `*` wildcard and `**` for traversing directories                 |
| `range`     | `{start}..{end}` (inclusive) or `{start}..<{end}` (end is exclusive); use 2 dots, not 3        |
| row         | list where each value represents a column with an associated name                              |
| `string`    | single words need no delimiter; multiple words need single quotes, double quotes, or backticks |
| `table`     | list of rows; returned by many Nushell commands                                                |

To see the type of an expression, pipe it to the `describe` command.
For example, `date now | describe` outputs `date`.

Values of type of `int` are reported as `integer`.
This is likely a bug. See {% aTargetBlank
"https://github.com/nushell/nushell/issues/3206", "issue 3206" %}.

The following types are never output by the `describe` command:
`number`, list, `pattern`, `range`, and `table`.
TODO: Perhaps the `describe` command should be modified to report these types.

Details about these data types can be found at {% aTargetBlank
"https://www.nushell.sh/book/types_of_data.html", "Types of data" %}.

### Type Conversions

The following type conversions are supported:

TODO: Fill in the ??? in this table.

| From       | To         | Command                                            |
| ---------- | ---------- | -------------------------------------------------- |
| `any`      | `string`   | pipe to `describe`                                 |
| `binary`   | `string`   | pipe to `???`                                      |
| `boolean`  | `string`   | pipe to `str from`                                 |
| `date`     | `string`   | pipe to `str from`                                 |
| `decimal`  | `string`   | pipe to `str from`                                 |
| `duration` | `string`   | pipe to `str from` \*                              |
| `filesize` | `string`   | pipe to `str from`                                 |
| `int`      | `string`   | pipe to `str from`                                 |
| `line`     | `string`   | pipe to `???`                                      |
| `list`     | `string`   | pipe to `str collect` if the list contains strings |
| `path`     | `string`   | pipe to `???`                                      |
| `pattern`  | `string`   | pipe to `???`                                      |
| `range`    | `string`   | pipe to `???`                                      |
| `row`      | `string`   | pipe to `???`                                      |
| `string`   | `binary`   | pipe to `???`                                      |
| `string`   | `boolean`  | pipe to `???`                                      |
| `string`   | `date`     | pipe to `str to-datetime`                          |
| `string`   | `decimal`  | pipe to `str to-decimal`                           |
| `string`   | `duration` | pipe to `???`                                      |
| `string`   | `filesize` | pipe to `???`                                      |
| `string`   | `int`      | pipe to `str to-int`                               |
| `string`   | `list`     | pipe to `???`                                      |
| `string`   | `path`     | pipe to `???`                                      |
| `string`   | `range`    | pipe to `???`                                      |
| `string`   | `row`      | pipe to `???`                                      |
| `string`   | `table`    | pipe to `???`                                      |
| `table`    | `string`   | pipe to ``                                         |

\* THIS GIVES AN ERROR!

The get the starting and ending values of a `range`,
pipe it to the `first` and `last` commands.

The `echo` command is often used to
feed the initial value into a command pipeline.
This can be a literal value or an expression such as a variable reference.

For example,

`echo "2021-3-21 14:30" | str to-datetime`

### Strings

Strings delimited by backticks support templating (a.k.a interpolation)
with expressions in pairs of double curly brackets.
For example:

{% raw %}

```bash
let x = 19; echo `x is {{$x}}`

let x = 3
let y = 5
echo `product of {{$x}} and {{$y}} is {{$(= $x * $y)}}`
```

{% endraw %}

The operators `=~` and `!~` test whether
one string contains or does not contain another.

### Ranges

Values of the `range` type can use default values for their start or end.
If the start value of a range is omitted, it defaults to zero.
If the end value of a range is omitted, the range has no upper bound.

### Types With Units

Duration values with different units can be added.
For example, `2hr + 57min + 11sec` (my best marathon time).

Values of the `filesize` type with different units can be added.
For example, `2mb + 57kb + 11b`.

Combining values of different types results in a coercion error.
For example, `3hr + 2mb` gives this kind of error and clearly identifies
that the first value is a `duration` and the 2nd is a `filesize`.

### Lists

The literal syntax for creating a `list` is to include expressions
in square brackets only separated by spaces or commas (for readability).
For example, `["foo" "bar" "baz"]` or `["foo", "bar", "baz"]`.

To iterate over the elements in a list, use the `each` command.
For example:

```bash
let names = [Mark Tami Amanda Jeremy]
echo $names | each {
  echo $(build-string $it $(char newline))
} | str collect # converts table to a single string
```

The `$it` special variable holds the output of the previous command
so it can be used in a block.

To access a list element at a given index, use `$name.index`.
For example, the second element in the list above
which is "Tami" can be accessed with `$names.1`.

The `in` and `not in` operators are used to test whether a value is in a list.
For example:

```bash
let colors = [red green blue]
# As discussed in the "Operators" section below, "=" enables math mode
# which is required to use the "in" and "not-in" operators.
= blue in $colors # true
= yellow in $colors # false
```

The `empty?` command is used to test whether a string, list, or table is empty.
For example:

```bash
# Strings
let name = "Mark"
= $name | empty? # false
let name = ""
= $name | empty? # true

# Lists
= $colors | empty? # false
let colors = []
= $colors | empty? # true

# Tables
let scores = [[Name Score]; [Mark 19] [Tami 21]]
= $scores | empty? # false
let scores = []
= $scores | empty? # true; not empty header row is present
```

The `reduce` command computes a single value from a list.
It takes a block which can use the special variables `$acc` and `$it`.
To specify an initial value for `$acc`, use the `--fold` flag.
To change `$it` to have `$it.index` and `$it.item` values,
add the `--numbered` flag.
For example:

```bash
let scores = [3 8 4]
echo "total =" $(echo $scores | reduce { = $acc + $it }) # 15
echo "total =" $(echo $scores | math sum) # easier approach, same result
echo "product =" $(echo $scores | reduce --fold 1 { = $acc * $it }) # 96
```

### Tables

The literal syntax for creating a table describe each row with a list
and separate the header row from the data rows with a semicolon.
For example, `echo [[Name Score]; [Mark 19] [Tami 21]]`
outputs the following table:

```text
───┬──────┬───────
 # │ Name │ Score
───┼──────┼───────
 0 │ Mark │    19
 1 │ Tami │    21
───┴──────┴───────
```

SQL-like syntax can be used to retrieve data from a table.
For example:

```bash
let scores = [[Name Score]; [Mark 19] [Tami 21]]
echo $scores | where Name == 'Tami' | get Score # 21
```

The use of the `where` command above is shorthand for
the expanded syntax using the special variable `$it` which holds
the result of the previous command or the current iteration value.
The previous pipeline can be written as follows using this syntax.
The curly braces after the `where` command define a block on which it operates.
The `=` enters "math mode".

```bash
echo $scores | where { = $it.Name == 'Tami'} | get Score # 21
```

Single-row tables can be used like objects in other languages.
For example:

````bash
let data = [[color flavor]; [yellow vanilla]]
echo $data.flavor # outputs vanilla

Nushell is currently very picky about
splitting table data over multiple lines.
The newlines in the table definition below break it!
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3186",
"issue 3186" %}.

```bash
let sports = [
  [name players];
  [baseball 9]
  [basketball 5]
  [football 11]
  [hockey 6]
]
let sport = basketball
let players = $(echo $sports | where name == $sport | get players)
echo `The number of active players in {{$sport}} is {{$players}}.`
````

Tables can contain nested tables.
Note the placement of newlines in the example below
which avoids the issue described above.
Note the use of `to json` to generate JSON from a table.

```bash
let person = [
  [name address]; [
    "Mark Volkmann", [
      [street city state zip]; [
        "123 Some Street" "Somewhere" "MO" 12345
      ]
    ]
  ]
]

echo $person
echo $person | get address
echo $person | to json --pretty 2
```

Running the code above produces the following result:

```text
╭───┬───────────────┬────────────────╮
│ # │ name          │ address        │
├───┼───────────────┼────────────────┤
│ 0 │ Mark Volkmann │ [table 1 rows] │
╰───┴───────────────┴────────────────╯

╭───┬─────────────────┬───────────┬───────┬───────╮
│ # │ street          │ city      │ state │ zip   │
├───┼─────────────────┼───────────┼───────┼───────┤
│ 0 │ 123 Some Street │ Somewhere │ MO    │ 12345 │
╰───┴─────────────────┴───────────┴───────┴───────╯

{
  "address": [
    {
      "city": "Somewhere",
      "state": "MO",
      "street": "123 Some Street",
      "zip": 12345
    }
  ],
  "name": "Mark Volkmann"
}
```

## Operators

Nushell supports the following operators:

| Operator | Description                     |
| -------- | ------------------------------- |
| `+`      | add                             |
| `-`      | subtract                        |
| `*`      | multiply                        |
| `/`      | divide                          |
| `**`     | exponentiation (power)          |
| `mod`    | modulo                          |
| `==`     | equal                           |
| `!=`     | not equal                       |
| `<`      | less than                       |
| `<=`     | less than or equal              |
| `>`      | greater than                    |
| `>=`     | greater than or equal           |
| `=~`     | string contains another         |
| `!~`     | string does not contain another |
| `in`     | value in list                   |
| `not in` | value not in list               |
| `&&`     | and two Boolean values          |
| `\|\|`   | or two Boolean values           |

Parentheses can be used for grouping in order to specify evaluation order.
Operators can only be used in "math mode".
An expression is in math mode if it begins with `=`.
Some commands, such as `where` are automatically evaluated in math mode.

For example, `let a = 2; let b = 3; = $a * $b` outputs `6`.

## Working with numbers

Many of the operators listed in the previous section operate on numbers.

The `inc` command has three uses.

The first use of the `inc` command is to
return a value that is one higher that the piped in value.
For example, `echo 2 | inc` gives `3`.
Since variables are immutable,
this cannot be used to increment the value in a variable.

The second use of the `inc` command is to increment all the `int` values
in a given table column if all the values in the column are of type `int`.
For example, `echo [[Name Size]; [Mark 33] [Tami 28]] | inc Size`
results in a table where the values in the "Size" column are 34 and 29.

The third use of the `inc` command is to increment
a specify part of a semantic version number
that includes major, minor, and patch parts.
For example:

```bash
echo 1.2.3 | inc -M # increments major resulting in 2.0.0
echo 1.2.3 | inc -m # increments minor resulting in 1.3.0
echo 1.2.3 | inc -p # increments patch resulting in 1.2.4
```

There is no `dec` command for decrementing values.

## Working with URLs

The `fetch` command can be used to get data from a URL.
The website {% aTargetBlank
"https://jsonplaceholder.typicode.com", "JSONPlaceholder" %}
provides access to free sample JSON data.
The following example gets data about TODOs from this site.

```bash
fetch https://jsonplaceholder.typicode.com/todos |
  where userId == 5 && completed == $false |
  sort-by title
```

The `post` command sends an HTTP POST requests to a server
and returns the response as a table.
The following example simulates creating a TODO resource
using the JSONPlaceholder site.
This particular service just simulates creating a resource.

```bash
let json = '{"title": "get milk", "userId": 5}"'
post https://jsonplaceholder.typicode.com/todos $json
```

Nushell does not currently provide commands
to send PUT, PATCH, or DELETE requests.

## Common Commands

Many common UNIX commands are supported by Nushell.
These are implemented in Rust and are very fast.
They include:

- `cal` displays a calendar
- `cd` changes the current working directory
- `clear` clears the terminal
- `cp` copies a file or directory
- `date` gets the current date
- `du` gets information about disk usage
- `echo` outputs the values of expressions
- `exit` exits the current shell; can specify status code
- `help` displays help information
- `history` displays command history
- `kill` kills a process
- `ls` lists the contents of the current directory or path
- `mkdir` makes (creates) a directory
- `mv` moves a file or directory
- `open` opens a file
- `ps` prints process information
- `pwd` prints the current working directory
- `rm` removes (deletes) a file or directory
- `source` runs a script file in the current context
- `which` outputs the path of an executable AND
  information about aliases and custom commands

Note that the `cp`, `mv`, and `rm` commands do not
currently support the `-i` flag to prompt for confirmation.
However, aliases for these can be defined to
use the corresponding commands from the parent shell.
For example, `alias rm = ^rm -i`.

To clear the screen, enter `clear`.

The nu shell displays most data in tables.
Commands that produce tables include `ls`, `ps`, and `sys`.
When a column displays `[table n rows]`,
use the `get` filter to display those rows (examples below).
SQL-like filters like `where`, `sort-by`, and `reverse`
can be used to modify the output.
For example:

```bash
# Change directory to a subdirectory named "src".
src # no need to type the "cd" command

# List all the package.json files in and below the current directory
# using a glob pattern.
ls **/package.json

# List TypeScript files in and below the current directory
# using a glob pattern.
ls **/*.ts

# List files in a tree layout
ls | tree

# List files in the current directory with a size of 2kb or more,
# sorted on size from largest to smallest.
ls | where type == File && size >= 2kb | sort-by size | reverse
ls | where type == File && size >= 2kb | sort-by -r size # same

# List directories in the current directory,
# sorted on size from largest to smallest,
# excluding the type column since all will be "Dir".
ls | where type == Dir | sort-by -r size | reject type

# Output processes using more than 5% of a CPU
# sorted on the usage in reverse order.
ps | where cpu > 5 | sort-by -r cpu

# Output information about the current machine
# including OS version, host name, and uptime.
sys | get host

# Output the temperature of the CPUs, GPU, and battery (tested on macOS).
sys | get temp

# Output the parts of the current date and time in a table.
date now | date to-table
```

To convert a table to a specific text format, use the `to` command.
Supported formats include csv (comma-separated), html, json,
md (Markdown), toml, tsv (tab-separated), url (url-encoded text), xml, and yaml.

To write the text output of a command to a file, use the `save` command.
This is alternative to the `>` redirect operator using in bash.

For example:

```bash
# Create an HTML file describing the largest directories
# in the current directory that are more than 1 KB.
ls | where type == Dir && size > 1024 | sort-by -r size | to html | save big-zips.html

# The text format defaults to CSV if the file extension is .csv.
# It does not do this for other file extensions.
ls | where type == Dir && size > 1024 | sort-by -r size | save big-zips.csv
```

## Aliases

To create an alias for a command, enter `alias {name} = {command}`.
For example, `alias cls = clear`.

The command can be built-in or custom
and literal arguments can be specified.
When the aliases is used, additional arguments can be specified.
For example, alias

To make aliases available in each new Nushell session,
add them to the `startup` list in the config file
as shown in the "Configuration" section.

Aliases cannot use pipelines. Custom commands must be used instead.
For example, this does not work:

```bash
alias top = ps | sort-by -r cpu | first 10
```

But this does work:

```bash
def top [] { ps | sort-by cpu -r | first 10 }
```

For more detail on supported commands, see the {% aTargetBlank
"https://www.nushell.sh/book/command_reference.html", "Command Reference" %}.

## Custom Commands

To define a custom command, enter `def {name} [params] { commands }`.
Names can be in kebab-case, including hyphens for readability.
They can end with `?` to indicate that they return a Boolean value.
Square brackets are used to surround the parameters because
they are treated as a list and that is the syntax for lists.
If no parameters are required, `[]` must still be included.

The result of a custom command is
the result of the last command pipeline
or a string formed by the accumulation of everything it echoes.

The following custom command has no parameters and outputs a table
showing the processes that are using more 5% or more of CPU usage
sorted from highest to lowest.

```bash
def top [] { ps | where cpu >= 5 | sort-by -r cpu }
```

To run this, enter `top`.

Here is a version that has a CPU percentage parameter.
Parameter values are accessed by adding `$` before their names.

```bash
def topn [pct] { ps | where cpu >= $pct | sort-by -r cpu }
```

To run this, enter `top` followed by a number like `top 5`.

The type of each parameter can optionally be specified after a colon to
provide better documentation and better error messages when used incorrectly.
Supported types include `any`, `int`, `number` (for float),
`path` (for file paths), `pattern` (for glob patterns), `range`,
`string`, `table`, `block`, and `unit` (like void?).
For example:

```bash
def topn [pct: number] { ps | where cpu >= $pct | sort-by -r cpu }
```

When invoking a command, multiple arguments are separated by spaces.
For example:

```bash
def sum [n1: number, n2: number] { = $n1 + $n2 }
```

To run this, enter a command like `sum 1 2` which outputs `3`.

Here are examples of custom commands whose result is defined by what they echo.

```bash
def evaluate [n: int] {
  echo $(build-string "The number " $n " is ")
  if $n > 5 { echo big } { echo small }
  echo "."
  echo $(char newline)
}

echo $(evaluate 1) # The number 1 is small.
echo $(evaluate 9) # The number 9 is big.

def evaluate2 [n: int] {
  let word = $(if $n > 5 { echo big } { echo small })
  echo $(build-string "The number " $n " is " $word "." $(char newline))
}

echo $(evaluate2 1) # The number 1 is small.
echo $(evaluate2 9) # The number 9 is big.
```

Input from other commands can be piped into a custom command
and accessed with the `$it` variable.
Output from custom commands can be piped into other commands.

The parameters in the examples above are positional.
Named parameters (a.k.a. flags) can also be specified
by adding `--` before their names.
These are long-form names.
To also specify short-form names, follow the long name with `(-short-name)`.
Like positional parameters, types can be specified for flags.
For example:

{% raw %}

```bash
# Prints a value followed by a newline.
def logv [value: any] {
  echo $(build-string $value $(char newline))
  # Another way to write the line above is:
  #echo [`{{$value}}` $(char newline)] | str collect
}

# Prints "name = value" followed by a newline.
def lognv [name: string, value: any] {
  logv $(build-string $name " = " $value)
}

def logv-color [
  text: string,
  --color (-c): string # a flag
] {
  # This code does not work yet.
  # It gives a coercion error that supposed is fixed in the next version of nu.
  # See https://github.com/nushell/nushell/discussions/3178.
  #if $color == $nothing {

  #if $(echo $color | empty?) { # same as next line
  if $(= $color | empty?) {
    logv $text
  } {
    logv $(build-string $(ansi $color) $text $(ansi reset))
  }
}

logv-color "Giraffes are cool!" "yellow"
```

{% endraw %}

Custom commands can take arguments with a type of `block`.
The `do` command can be used to execute the block.
For example, this can be used to implement `map`
which takes a list and a block.

```bash
# You asked about this in Discord.
def map [values, code: block] { # What type can be specified for values?
  echo $values | each $code
}

let names = [Mark Tami Amanda Jeremy]

map $names {
  echo $(build-string "Hello, " $it $(char newline))
} | str collect

# Same result just using each.
echo $names | each {
  echo $(build-string "Hello, " $it ($(char newline))
} | str collect
```

Custom commands can take a variable number of arguments
using the parameter syntax `...rest`.
The parameter name must be "rest" and it must be the last parameter.
Its value is a list.
For example:

```bash
def labelled-sum [label: string, ...rest: int] {
  echo $(build-string $label " = " $(echo $rest | math sum) $(char newline))
}

labelled-sum "sum of scores" 3 7 19 # sum of scores = 29
```

Help for custom commands is obtained in the same way it is for
built-in commands, using `help {command-name}` or `{command-name} -h`.

To add documentation to custom commands,
add a comment before the definition and after each parameter.
For example:

```bash
# Computes the sum of two numbers.
def sum [
  n1: number, # first number
  n2: number # second number
] {
  = $n1 + $n2
}
```

Subcommands provide a way of grouping commands under a common "namespace".
Examples of such built-in namespaces include
`date`, `from`, `to`, `math`, `str`, `url`.

The `date` subcommands are
`format`, `list-timezone`, `now`, `to-table`, and `to-timezone`.

The `from` subcommands convert data from a given format to a table.
They are
`csv`, `eml`, `ics`, `ini`, `json`, `ods`, `ssv`, `toml`, `tsv`,
`url`, `vcf`, `xlsx`, `xml`, `yaml`, and `yml`.

The `to` subcommands convert a table into a given output format and
are commonly piped to the `save` command to write the result to a file.
They are
`csv`, `html`, `json`, `md`, `toml`, `tsv`, `url`, `xml`, and `yaml`.

The `math` subcommands perform math calculations.
They are
`abs`, `avg`, `ceil`, `eval`, `floor`, `max`, `median`, `min`, `mode`,
`product`, `round`, `stddev`, `sum`, and `variance`.
Also see the commands `count`, `inc`, `into-int`, `random`, and `seq`.

The `str` subcommands perform string operations.
They are
`camel-case`, `capitalize`, `collect`, `contains`, `downcase`, `ends-with`,
`find-replace`, `from`, `index-of`, `kebab-case`, `length`, `lpad`, `ltrim`,
`pascal-case`, `reverse`, `rpad`, `rtrim`, `screaming-snake-case`,
`snake-case`, `starts-with`, `substring`, `to-datetime`, `to-decimal`,
`to-int`, `trim`, and `upcase`.
Also see the commands `build-string`, `char`, `flatten`, `format`, and `size`.

The `url` subcommands get information from a URL.
They are `host`, `path`, `query`, and `scheme` (ex. http).

Defining a custom subcommand is similar to defining a custom command,
but the name is specified as the parent command and subcommand name
separated by a space and inside quotes.
In the following custom commands, the parent command `rmv` is my initials:

```bash
# Parent command.
def rmv [] {}

# Increments a number by 1.
def "rmv increment" [n: number] { = $n + 1 }

# Doubles a number.
def "rmv double" [n: number] { = $n * 2 }

let score = 3
lognv 'score' $(rmv double $(rmv increment $score)) # 8
```

## Variables

Variables in Nushell are distinct from environment variables.
They are immutable, so they must be set when they are declared.
However, they can be shadowed to have different values in different scopes.

To set a variable, enter `let name = value`.
Their scope is the context or block in which they are defined.

To set a variable to the result of a command pipeline,
which may contain only a single command,
enter `let name = $(pipeline)`.
The syntax `$(...)` is referred to as an "invocation".
It can also be used to pass the result of a command pipeline
as a argument to a command.

To use a variable in an expression, precede its name with `$`.
For example, `$total`.
When a variable holds a structured value such as a table,
dot syntax can be used to access nested values.
For example:

```bash
# A single row type can be used like objects in other languages.
let data = [[color flavor]; [yellow vanilla]]
echo $data.flavor # outputs vanilla
```

To set a variable to the result of concatenating two variable values as strings,
use `let v3 = $(build-str $v1 $v2)`.

To get the type of a primitive value, pipe it into the `describe` command.
For example, `echo 19 | describe` outputs "integer"
and `echo 3.14 | describe` outputs "decimal".

## Environment Variables

Environment variables are distinct from regular variables.
Unlike regular variables, environment variables can be
used in executables that are run from Nushell.

To set the value of an environment variable only in the current scope,
not permanently, enter `let-env NAME = value`.
This adds `NAME` to `$nu.env` which is a like a map of environment variables.

To set the value of an environment variable
so it is available in subsequent Nushell sessions,
add it to the `env` section of the Nushell configuration file.

To get the value of an environment variable, use `$nu.env.NAME`.
To print the value, enter `echo $nu.env.NAME` or `config | get env.{name}`.

To see a nicely formatted list of environment variables,
enter `echo $nu.env | pivot` or `config | get env | pivot`.

## open Command

The `open` command renders certain file types as tables.
These file types include csv, ini, json, toml, xml, and yaml.

For example, the following outputs a table of scripts in a `package.json` file.

```bash
open package.json | get scripts | pivot
```

The output will be similar to the following:

```text
───┬─────────┬─────────────────────────────────────────────────────────
 # │ Column0 │                         Column1
───┼─────────┼─────────────────────────────────────────────────────────
 0 │ build   │ rollup -c
 1 │ dev     │ rollup -c -w
 2 │ format  │ prettier --write '{public,src}/**/*.{css,html,js,svelte}'
 3 │ lint    │ eslint --fix --quiet src --ext .js,.svelte
 4 │ start   │ sirv public
───┴─────────┴─────────────────────────────────────────────────────────
```

To see the commands in the Nushell configuration file `startup` section,
enter `open $(config path) | get startup`.

Other types of files are treated as a list of lines and
rendered in a table where the first column contains line numbers.
For known programming language file extensions, syntax highlighting is provided.
Compare this to using the `cat` command where this does not happen.

To render delimited data as a table we can use the `lines` and `split` commands.
For example, consider the following file content:

```text
Go|2012|Rob Pike
Java|1995|James Gosling
JavaScript|1995|Brendan Eich
Python|1991|Guido van Rossum
Ruby|1995|Yukihiro Matsumoto
Rust|2010|Graydon Hoare
TypeScript|2012|Anders Hejlsberg
```

To render a table where each row describes a programming language,
the columns have proper names, and
the rows are sorted on ascending year of creation, enter
`open languages.txt | lines | split column '|' Language Year Creator | sort-by Year`.
The `lines` command converts input text into a list of separate lines of text.
The following table is produced:

```text
───┬────────────┬──────┬────────────────────
 # │  Language  │ Year │      Creator
───┼────────────┼──────┼────────────────────
 0 │ Python     │ 1991 │ Guido van Rossum
 1 │ Java       │ 1995 │ James Gosling
 2 │ JavaScript │ 1995 │ Brendan Eich
 3 │ Ruby       │ 1995 │ Yukihiro Matsumoto
 4 │ Rust       │ 2010 │ Graydon Hoare
 5 │ Go         │ 2012 │ Rob Pike
 6 │ TypeScript │ 2012 │ Anders Hejlsberg
───┴────────────┴──────┴────────────────────
```

If the file extension on a file does not match its content type,
use the `from` command to specify the actual content type.
For example, `open really-json.txt | from json`.

To prevent the `open` command from processing a file, add the `--raw` flag.
For example, `open scores.csv --raw`.

To process data from a URL instead of a local file, use the `fetch` command.
For example, the
{% aTargetBlank "https://jsonplaceholder.typicode.com", "{JSON} Placeholder" %}
site provides free data for testing and prototyping.
Todo data from this site can be rendered as a table with the following commands:

```bash
fetch https://jsonplaceholder.typicode.com/todos | first 10

fetch https://jsonplaceholder.typicode.com/todos | where userId == 2 && completed == $true | sort-by title
```

## Table Commands

Many Nushell commands operate on tables.

| Command                    | Description                                                      |
| -------------------------- | ---------------------------------------------------------------- |
| `append`                   | appends a row                                                    |
| `autoview`                 | renders data as a table or list                                  |
| `compact`                  | removes empty rows                                               |
| `count`                    | counts rows or list items                                        |
| `drop n`                   | removes the last n rows (n defaults to 1)                        |
| `drop column n`            | removes the last n columns (n defaults to 1)                     |
| `each`                     | runs a block of code on each row                                 |
| `every n`                  | show or skip every nth row                                       |
| `first n`                  | show only the first n rows (n defaults to 1)                     |
| `flatten`                  | flattens a table                                                 |
| `format`                   | formats columns into a string                                    |
| `from {format}`            | parses a given file format into a table                          |
| `get {column-name}`        | gets the content of a given column name as a table               |
| `group-by`                 | TODO: STUDY THIS                                                 |
| `headers`                  | uses the first row as column names                               |
| `histogram`                | TODO: STUDY THIS                                                 |
| `insert`                   | inserts a column                                                 |
| `keep n`                   | keeps the first n rows (n defaults to 1); same as `first`?       |
| `last n`                   | show only the last n rows (n defaults to 1)                      |
| `lines`                    | splits a string of lines into rows                               |
| `match`                    | filter rows using a regular expression                           |
| `merge`                    | merges tables by adding columns                                  |
| `move`                     | moves columns; TODO: STUDY THIS                                  |
| `nth`                      | keep or skip specified rows                                      |
| `parse`                    | parses columns from a string using a pattern                     |
| `pivot`                    | swaps the rows and columns                                       |
| `prepend`                  | prepends a row                                                   |
| `range`                    | gets a subset of rows                                            |
| `reduce`                   | computes a single value from a list table                        |
| `reject`                   | removes columns by name                                          |
| `rename`                   | renames columns                                                  |
| `reverse`                  | reverses the order of the rows                                   |
| `roll n`                   | rolls the bottom n rows to the top (n defaults to 1)             |
| `rotate`                   | rotates the table 90 degrees clockwise; can apply multiple times |
| `rotate counter-clockwise` | rotates the table 90 degrees counter-clockwise                   |
| `select {column-names}`    | specifies columns to be retained by name and their order         |
| `shuffle`                  | shuffles the rows randomly                                       |
| `skip n`                   | skips the first n rows (n defaults to 1)                         |
| `skip until condition`     | skips runs until the condition is met (alternative to `where`)   |
| `skip while condition`     | skips runs while the condition is met (alternative to `where`)   |
| `sort-by`                  | sorts rows on given columns                                      |
| `split-by`                 | ?                                                                |
| `table`                    | views pipeline output as a table                                 |
| `to {format}`              | converts a table to a given format such as json                  |
| `uniq`                     | gets unique rows                                                 |
| `update`                   | updates data in a given column                                   |
| `where`                    | specifies a condition rows must meet to render                   |
| `wrap`                     | creates a table column from its data and a name                  |

The `sort-by` command accepts
the `--insensitive` flag to make the sort case-insensitive and
the `--reverse` flag to reverse the sort order.

Let's look at some examples using the `ls` command.
This produces a table with the columns "name", "type", "size", and "modified".
Here is a command that lists the files with a `.ts` file extension,
only includes the "name" and "size" columns,
sorts the files from largest to smallest,
and only outputs the three largest files:

```bash
ls *.ts | select name size | sort-by -r size | first 3
```

This produces output similar to the following:

```text
───┬────────────────────────┬──────────
 # │          name          │   size
───┼────────────────────────┼──────────
 0 │ lib.deno.unstable.d.ts │ 194.4 KB
 1 │ lib.deno.d.ts          │ 145.8 KB
 2 │ my_server.ts           │   2.7 KB
───┴────────────────────────┴──────────
```

The row indexes in the first column can be used to
retrieve only a specific row using the `nth` command.
For example, we can add `| nth 1` to the end of the command
to only output the row for the file `lib.deno.d.ts`.

The `get` command can be used to output
only the values in a single column.
It returns a list rather than a table.
For example, the following outputs the
names of the three largest TypeScript files:

```bash
ls *.ts | sort-by size | reverse | first 3 | get name
```

The `get` command is especially useful when the type of a field is "table".
The key can be arbitrarily deep with sub-keys separated by periods.
For example, `sys | get host.sessions | where name == 'root' | get groups`.

The following example demonstrates getting the headings from a table.
TODO: Is this the best way?

```bash
echo $my-table | first | pivot | select Column0
```

TODO: Is there a command get a column by its index rather than its name?

The columns of one table can be added to another to produce a new table
using the `merge` command. For example:

```bash
let t1 = [[name score]; [Mark 19] [Tami 21]]
echo $t1

# Add a single column.
let column = $(echo [yellow blue] | wrap color)
echo $t1 | merge { echo $column }

# Add all the columns from another table.
let t2 = [[color flavor]; [yellow vanilla] [blue chocolate]]
echo $t1 | merge { echo $t2 }
```

The output produced by this example is:

```text
╭───┬──────┬───────╮
│ # │ name │ score │
├───┼──────┼───────┤
│ 0 │ Mark │    19 │
│ 1 │ Tami │    21 │
╰───┴──────┴───────╯

╭───┬──────┬───────┬────────╮
│ # │ name │ score │ color  │
├───┼──────┼───────┼────────┤
│ 0 │ Mark │    19 │ yellow │
│ 1 │ Tami │    21 │ blue   │
╰───┴──────┴───────┴────────╯

╭───┬──────┬───────┬────────┬───────────╮
│ # │ name │ score │ color  │ flavor    │
├───┼──────┼───────┼────────┼───────────┤
│ 0 │ Mark │    19 │ yellow │ vanilla   │
│ 1 │ Tami │    21 │ blue   │ chocolate │
╰───┴──────┴───────┴────────┴───────────╯
```

TODO: Is there a way to add rows to a table?

## Plugins

Nushell plugins add new commands.
They can be installed using the Rust `cargo` utility.
After installing, open a new shell to gain access to commands that they add.

### nu_plugin_start

This plugin adds the `start` command that opens a given file
using its default application.
To install this, enter `cargo install nu_plugin_start`.
Then open a new shell and enter `start file-path`.

## Default Shell

To make Nushell your default shell in Linux or macOS, first
add a line containing the file path to the `nu` executable in `/etc/shells`.
For example, for me using macOS I entered `su vim /etc/shells`
and added the line `/Users/mark/.cargo/bin/nu`.
Then enter `chsh -s nu`.
If using tmux, also change the value of `default-shell` in `~/.tmux.conf`.

## Scripts

Nushell scripts are written in files with a `.nu` extension.
To execute a script, enter `nu {name}.nu`.
To "source" a script so its definitions become available in the current shell,
enter `source {name}.nu`.

For examples of Nushell scripts, see {% aTargetBlank
"https://github.com/nushell/nu_scripts/tree/main/nu_101", "Nu_101 Scripts" %}.

To add comments or "comment-out" a line of code,
preceded the comment with a `#` character.

Commands commonly used in Nushell scripts include
`def`, `if`, `each`, and `seq`.

The `def` command defines a custom command
which can be used like a function in many programming languages.

Conditional processing is implemented with the `if` command.
Its syntax is `if condition { then-block } { else-block }`.
Note that the condition is not surrounded by parentheses and
curly braces are required around the then and else blocks.
There are no `then` or `else` keywords.
Nested ifs must be placed inside a then or else block.
An `if` command can be split over multiple lines.
For example:

```bash
let temperature = 80
if $temperature <= 32 {
  echo cold
} {
  if $temperature >= 80 {
    echo hot
  } {
    echo warm
  }
}
```

To iterate over a range of integers, use the `seq` command
and pipe the result to the `each` command.
The special variable `$it` holds each iteration value.
For example:

```bash
seq 1 4 | each { build-string $it $(char newline) } | str collect
```

Let's break this down.

- `seq 1 4` returns the list `[1 2 3 4]`.
- `each { build-string $it $(char newline) }` returns the list
  `["1\n" "2\n" "3\n" "4\n"]`
- `str collect` returns a string created by concatenating the list values.
- The output is:

  ```text
  1
  2
  3
  4
  ```

The `seq` command creates a list of strings, not integers.
An alternative is to use the `echo` command with a range
specified with `start..end` which creates a list of integers.
The previous example can be written as follows:

```bash
echo 1..4 | each { build-string $it $(char newline) } | str collect
```

Iteration over the elements of a list is implemented with the `each` command.
Its syntax is `echo some-list | each { block }`.
The block can use the special variable `$it`
to access the current element in the iteration.
For example:

```bash
def log-value [label, value] {
  echo $(build-string $label " = " $value $(char newline))
}

def report [list] {
  # Without the --numbered flag, $it is set to each list value.
  # With it, $it is an object with the properties index and item.
  echo $list | each --numbered {
    build-string $(= $it.index + 1) ") " $it.item $(char newline)
  } | str collect # with this the result is a table instead of a string
}

let names = [Mark Tami Amanda Jeremy]

log-value "name at index 2" $(echo $names | nth 2) # Amanda

report $names
# 1) Mark
# 2) Tami
# 3) Amanda
# 4) Jeremy
```

To calculate the combined size of the `nu` executable and installed plugins,
enter `ls $(build-string $(which nu | get path) '*') | get size | math sum`.

## VS Code

There is a VS Code extension for Nushell that
provides syntax highlighting for Nushell scripts.
See {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=TheNuProjectContributors.vscode-nushell-lang",
"vscode-nushell-lang" %}.

## Comparison to Bash

The following table shows the Nushell equivalent of some common Bash commands.

| Bash                   | Nushell                                  | Description                                                                  |
| ---------------------- | ---------------------------------------- | ---------------------------------------------------------------------------- |
| `man command`          | `help command` only for Nushell commands |                                                                              |
| `$PATH`                | `$nu.path`                               | list of directories search for executables                                   |
| `cat file-path`        | `open --raw file-path`                   | print the contents of a file; omit `--raw` to output structured data         |
| `command > file-path`  | `command \| save --raw file-path`        | saves command output to a file<br>without converting based on file extension |
| `mkdir -p foo/bar/baz` | `mkdir foo/bar/baz`                      | creates directory structure, including any missing directories               |
| cmd1 && cmd2           | cmd1; cmd2                               | run cmd1 and then only run cmd2 if cmd1 was successful                       |

The command whose output is piped in the `save` command must produce a string.
For example, `date now | save --raw timestamp.txt` does not work,
but `date now | str from | save --raw timestamp.txt` does.

## Charts

TODO: Learn about rendering bar and line charts with the `chart` command.
TODO: Also see the `histogram` command.

## Per Directory Environment Variables

Nushell supports defining things to happen when changing to given directories.
To configure this, create a `.nu-env` file in each directory.
These are TOML files with the following sections:

| TOML Section   | Description                                                                                                         |
| -------------- | ------------------------------------------------------------------------------------------------------------------- |
| `[env]`        | sets environment variables to literal values; lines have syntax<br>`name = "value"`                                 |
| `[scriptvars]` | sets environment variables to command output; lines have syntax<br>`name = "command-pipeline"`                      |
| `[scripts]`    | specifies commands to run when entering and exiting the directory<br>with `entryscripts` and `exitscripts` commands |

After creating the `.nu-env` file in each directory,
enter `autoenv trust` to give Nushell permission to read the file.

For example, suppose we want to set the environment variable `NODE_ENV`
to `development` or `production` based on the current directory.
Add the following setting in the Nushell configuration file:

```toml
nu_env_dirs = [
  "~/projects/airline-reservations",
  "~/projects/bank-accounts"
]
```

In the `~/projects/airline-reservations` directory, create the file `.nu-env`
containing the following and then enter `autoenv trust`:

```toml
[env]
NODE_ENV = "development"
```

Currently the `echo` command does not write to stdout when run from
`entryscripts` or `exitscripts`.

In the `~/projects/bank-accounts` directory, create the file `.nu-env`
containing the following and then enter `autoenv trust`:

```toml
[env]
NODE_ENV = "production"
```

Now `cd` to each of these directories and verify that
the `NODE_ENV` environment variable is set to the expected value.

For more details on this feature, enter `help autoenv`.

## Additional Shells

New shells can be created from the current shell.
This enables retaining the working directory of the current shell and
switching back to it using the `n` (for next) and `p` (for previous) commands.

| Command           | Description                                                                       |
| ----------------- | --------------------------------------------------------------------------------- |
| `enter dir-path`  | similar to `cd`, but creates a new shell starting in the given directory          |
| `enter file-path` | creates a new shell whose context is the content of the given file; odd           |
| `shells`          | lists the existing shells and indicates which one is active                       |
| `n`               | makes the next shell in the list active;<br>wrapping to first if on last          |
| `p`               | makes the previous shell in the list active;<br>wrapping to last if on first      |
| `exit`            | exits the current shell, removing it from the list                                |
| `exit --now`      | exits all the shells;<br>depending on configuration the terminal window may close |

## Issues

- Fuzzy completion is not yet supported.
  For example, entering `cd foo` and pressing the tab key
  doesn’t auto complete to a directory that contains `foo`.
- The literal syntax for tables is currently very picky
  about the location of newline characters due to a parser bug.
  See {% aTargetBlank "https://github.com/nushell/nushell/issues/3204",
  "issue 3204" %}
- There is no built-in `grep` command.
  TODO: How can another grep command (like `ripgrep`)
  TODO: be used with the output of `ls **/*.file-type`?
