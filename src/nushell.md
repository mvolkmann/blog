---
eleventyNavigation:
  key: Nushell
layout: topic-layout.njk
---

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
So `ls` is processed as if `ls | autoview` was entered.
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

## Starship

Another option for customizing the prompt is to enable the use of
{% aTargetBlank "https://starship.rs", "Starship" %}.
Edit the configuration file and add `prompt = "starship prompt"`.
One way to install Starship is to enter `cargo install starship`.
Download "FiraCode Nerd Font" from {% aTargetBlank
"https://www.nerdfonts.com/font-downloads", "nerdfonts.com" %}.
In macOS, double-click the file `Fira Code Regular Nerd Font Complete.ttf`
and press the "Install Font" button.
In iTerm2, select Preferences...Profiles...Text and
select "FiraCode Nerd Font" from the Font drop-down.
Open a new terminal window for the changes to take effect.
To configure Starship, create the file `~/.config/starship.toml`.
Changes to this file take effect immediately.

For example:

```toml
format = "🦀$git_branch$git_status$directory$character"
# Nushell controls the color of commands typed after the prompt.

[character]
success_symbol = "[▶](bold green)"
error_symbol = "[✗](bold red)"

[directory]
format = "[$path]($style)"
style = "yellow"
truncate_to_repo = false
truncation_length = 3 # parent directories to show; default is 3
truncation_symbol = "…/"

[git_branch]
format = "[$symbol](green)[$branch]($style)"
style = "italic green"
symbol = ""

[git_status]
format = "[$all_status]($style) "
style = "bold red"
```

TODO: Why doesn't `config load $(config path)` work?
TODO: You asked in the Nushell discussion page.

### startup

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
"this issue" %}.

### table_mode

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

## env Section

The command `config set env $nu.env` adds all the current
environment variables in the `env` section of the config file.
This section can also define environment variables
that are specific to Nushell.
Note that if Nushell is your login shell then
there is no parent shell from which to inherit environment variables.

## textview Section

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

| Type         | Description                                                                                    |
| ------------ | ---------------------------------------------------------------------------------------------- |
| `boolean`    | literal values are `$true` and `$false`                                                        |
| `integer`    | whole numbers with infinite precision                                                          |
| `decimal`    | numbers with a fractional part and infinite precision                                          |
| `number`     | floating point numbers with infinite precision                                                 |
| `range`      | `{start}..{end}` (inclusive) or `{start}..<{end}` (end is exclusive)                           |
| `string`     | single words need no delimiter; multiple words need single quotes, double quotes, or backticks |
| `line`       | a string with an OS-dependent line ending                                                      |
| glob pattern | can include `*` wildcard and `**` for traversing directories                                   |
| `date`       | timezone-aware; defaults to UTC                                                                |
| `duration`   | number followed by a unit which can be `ms`, `sec`, `min`, `hr`, `day`, `wk`, `mon`, or `yr`   |
| `file size`  |                                                                                                |
| column path  | dot-separated list of nested column names                                                      |
| file path    | platform-independent path to a file or directory                                               |
| `filesize`   | number followed by a unit which can be `b`, `kb`, `mb`, `gb`, `tb`, or `pb`                    |
| `binary`     | sequence of raw bytes                                                                          |
| `list`       | sequence of values of any type                                                                 |
| `row`        | list where each value represents a column with an associated name                              |
| `table`      | list of rows; returned by many Nushell commands                                                |
| `block`      | block of nu script code that can be executed on each row of a table                            |
| `group`      | semicolon-separated list of pipelines that can be run in parallel?                             |

Details about these data types can be found at {% aTargetBlank
"https://www.nushell.sh/book/types_of_data.html", "Types of data" %}.

### Strings

Strings delimited by backticks support templating
with expressions in pairs of double curly brackets.
For example:

{% raw %}

```bash
let x = 19; echo `x is {{$x}}`
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

The `in` and `not in` operators are used to test whether a value is in a list.
For example:

```bash
let colors = [red green blue]
# As discussed in the "Operators" section below, "=" enables math mode
# which is required to use the "in" and "not-in" operators.
= blue in $colors # true
= yellow in $colors # false
```

The `empty?` function is used to test whether a string, list, or table is empty.
For example:

TODO: Add examples of testing strings and tables.

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

```bash
let data = [[color flavor]; [yellow vanilla]]
echo $data.flavor # outputs vanilla

# The newlines in the table definition below break it!
# See https://github.com/nushell/nushell/issues/3186.
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
```

## Operators

Nushell supports the following operators:

| Operator | Description                     |
| -------- | ------------------------------- |
| `+`      | add                             |
| `-`      | subtract                        |
| `*`      | multiply                        |
| `/`      | divide                          |
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
- `exit` exits the current shell
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

# List directories in the current directory,
# sorted on size from largest to smallest,
# excluding the type column since all will be "Dir".
ls | where type == Dir | sort-by size | reverse | reject type

# Output processes using more than 5% of a CPU
# sorted on the usage in reverse order.
ps | where cpu > 5 | sort-by cpu | reverse

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
ls | where type == Dir && size > 1024 | sort-by size | reverse | to html | save big-zips.html

# The text format defaults to CSV if the file extension is .csv.
# It does not do this for other file extensions.
ls | where type == Dir && size > 1024 | sort-by size | reverse | save big-zips.csv
```

## Custom Commands

To define a custom command, enter `def {name} [params] { commands }`.
Names can be in kebab-case, including hyphens for readability.
They can end with `?` to indicate that they return a Boolean value.
Square brackets are used to surround the parameters because
they are treated as a list and that is the syntax for lists.
If no parameters are required, `[]` must still be included.

For example, this command has no parameters.

```bash
def top [] { ps | where cpu > 5 | sort-by cpu | reverse }
```

To run this, enter `top`.

Here is a version that has a CPU percentage parameter.
Parameter values are accessed by adding `$` before their names.

```bash
def topn [pct] { ps | where cpu > $pct | sort-by cpu | reverse }
```

To run this, enter `top` followed by a number like `top 5`.

The type of each parameter can optionally be specified after a colon to
provide better documentation and better error messages when used incorrectly.
Supported types include `any`, `int`, `number` (for float), `path` (for file paths),
`pattern` (for glob patterns), `range`, `string`, `table`, `block`,
and `unit` (like void?).
For example:

```bash
def topn [pct: number] { ps | where cpu > $pct | sort-by cpu | reverse }
```

Multiple arguments are separated by spaces. For example:

```bash
def sum [n1: number, n2: number] { = $n1 + $n2 }
```

To run this, enter a command like `sum 1 2` which outputs `3`.

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
  if $color == $nothing {
    logv $text
  } {
    logv $(build-string $(ansi $color) $text $(ansi reset))
  }
}

logv-color "Giraffes are cool!" "yellow"
```

{% endraw %}

Help for custom commands is obtained in the same way it is for built-in commands,
using `help {command-name}` or `{command-name} -h`.

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
Also see the commands `count`, `inc`, `into-int`, `random`, `seq`, ....

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

To set a variable, enter `let name = value`.
Note that these are distinct from environment variables.
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
alias top = ps | sort-by cpu | reverse | first 10
```

But this does work:

```bash
def top [] { ps | sort-by cpu | reverse | first 10 }
```

TODO: Why is everything after the first pipe ignored?

## VS Code

There is a VS Code extension for Nushell that provides syntax highlighting.
See {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=TheNuProjectContributors.vscode-nushell-lang",
"vscode-nushell-lang" %}.

## More Commands

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
the rows are sorted on ascending year of creation,
enter `open languages.txt | lines | split column '|' Language Year Creator | sort-by Year`.
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

To prevent the `open` command from processing a file, add the `--raw` option.
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
| `get {column}`             | gets the content of a given column name as a table               |
| `group-by`                 | STUDY THIS                                                       |
| `headers`                  | uses the first row as column names                               |
| `histogram`                | STUDY THIS                                                       |
| `insert`                   | inserts a column                                                 |
| `keep n`                   | keeps the first n rows (n defaults to 1); same as `first`?       |
| `last n`                   | show only the last n rows (n defaults to 1)                      |
| `lines`                    | splits a string of lines into rows                               |
| `match`                    | filter rows using a regular expression                           |
| `merge`                    | merges tables; STUDY THIS                                        |
| `move`                     | moves columns; STUDY THIS                                        |
| `nth`                      | keep or skip specified rows                                      |
| `parse`                    | parses columns from a string using a pattern                     |
| `pivot`                    | swaps the rows and columns                                       |
| `prepend`                  | prepends a row                                                   |
| `sort-by`                  | sorts the rows on a given column                                 |
| `range`                    | gets a subset of rows                                            |
| `reduce`                   | computes a single value from a list table                        |
| `reject`                   | removes columns by name                                          |
| `rename`                   | renames columns                                                  |
| `reverse`                  | reverses the order of the rows                                   |
| `roll n`                   | rolls the bottom n rows to the top (n defaults to 1)             |
| `rotate`                   | rotates the table 90 degrees clockwise; can apply multiple times |
| `rotate counter-clockwise` | rotates the table 90 degrees counter-clockwise                   |
| `select`                   | specifies columns to be retained by name and their order         |
| `shuffle`                  | shuffles the rows randomly                                       |
| `skip n`                   | skips the first n rows (n defaults to 1)                         |
| `sort-by`                  | sorts by given columns                                           |
| `split-by`                 | ?                                                                |
| `table`                    | views pipeline output as a table                                 |
| `to {format}`              | converts a table to a given format such as json                  |
| `uniq`                     | gets unique rows                                                 |
| `update`                   | updates data in a given column                                   |
| `where`                    | specifies a condition rows must meet to render                   |
| `wrap`                     | wraps data in a table                                            |

Let's look at some examples using the `ls` command.
This produces a table with the columns "name", "type", "size", and "modified".
Here is a command that lists the files with a `.ts` file extension,
only includes the "name" and "size" columns,
sorts the files from largest to smallest,
and only outputs the three largest files:

```bash
ls *.ts | select name size | sort-by size | reverse | first 3
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

## Plugins

Nushell supports adding functionality through plugins.
These can be installed using the Rust `cargo` utility.
For example, `cargo install nu_plugin_chart`.

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

Commands commonly used in Nushell scripts include `if`, `each`, ...

To iterate over a range of integers, use
`seq start end | each { ... }`.

To calculate the combined size of the `nu` executable and installed plugins,
enter `ls $(build-string $(which nu | get path) '*') | get size | math sum`.

## Charts

TODO: Learn about rendering bar and line charts with the `chart` command.
TODO: Also see the `histogram` command.

## Working with URLs

TODO: Describe the `fetch` and `post` commands.

## Questions

TODO: Is it possible to change the nu shell prompt?

The `$it` variable holds the output of the previous command
so it can be used in a block.
TODO: Show examples of using this.

## Issues

- Defining and using aliases named "pull" and "push" crashes the shell.
- Fuzzy completion is not yet supported.
  For example, entering `cd foo` and pressing the tab key
  doesn’t auto complete to a directory that contains `foo`.
