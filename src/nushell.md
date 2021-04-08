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
It also has similarities to the Python-based
{% aTargetBlank "https://pandas.pydata.org", "Pandas" %} data analysis library.
These are features not typically seen in shell environments.
It is far from a toy project and uses recursive descent parser
that is driven by its many supported data types.

Nushell continues the UNIX tradition of
commands whose input can be piped in from a previous command
and whose output can be piped to a subsequent command.
A series of commands separated by the pipe character `|`
is referred to as a "pipeline".
Commands stream their output to the next command in the pipeline.
A command does not have to run to completion before the next command
in the pipeline can begin receiving that output as its input.

Some commands such as `echo` and `ls` are lazy.
This means they do not produce output unless
something is requesting data from their output stream.
One way to do this it to pipe their output to the `autoview` command
which determines how to render the data based on its type.
Piping to `autoview` occurs implicitly in the shell
after the last command in a pipeline.
So the command `ls` is processed as if `ls | autoview` was entered.
When semicolons are used to separate multiple pipelines on the same line,
`autoview` is only applied to the last pipeline.
For example, `let a = 2; let b = 3; = $a + $b` outputs `5`.

Color coding is applied to commands as they are typed.
If a command becomes invalid, all the text changes to red.

Because Rust is a great source for compiling to WebAssembly, it was
possible to implement a web-based environment for experimenting with Nushell.
This can be found at
{% aTargetBlank "https://www.nushell.sh/demo/", "Nushell demo" %}.

The Nu language can be used outside Nushell, such as in Jupyter Notebooks.

It costs nothing but some disk space to try Nushell (about 50 MB).
You don't have to commit to making it your default shell.
Just pop in periodically to try it and exit to return to your current shell.
Over time you may decide you like it enough to make it your default shell.

## Terminology

The term "pipeline" was described earlier.
Command pipelines entered in the shell must be on a single line,
but inside scripts lines that end with `|` continue on the next line.
Nushell does not support using `\` as a line continuation character
like in other shells such as Bash.

Commands that take no input and produce output
are referred to as "inputs" or "sources".
An example is the `open` command which loads a file.

Commands that take input and transform it are referred to as "filters".
Examples include the `where` and `sort-by` commands.

Commands that take input and display it or write it somewhere such as file
are referred to as "outputs" or "sinks".
Examples include the `autoview` and `save` commands.

## Installing

There are many options for installing the Nushell.

If you have Rust installed, enter `cargo install nu`.
This takes several minutes to complete.

If you are on macOS and have Homebrew installed, enter `brew install nushell`.
Note that Homebrew may install a version
that is several versions behind the latest.

Prebuilt binaries for Linux, macOS, and Windows can also be downloaded.

For more information on installation options, click the Nushell link above.

## Getting Started

After installing Nushell, enter `nu` in a terminal to start a shell.

For help, enter `help`.
For a list of Nushell commands and custom commands, enter `help commands`.
In version 0.28.0 there are 105 Nushell commands.
For help on a specific command,
enter `help {command-name}` or {command-name} -h`.
For more detail on supported commands, see the {% aTargetBlank
"https://www.nushell.sh/book/command_reference.html", "Command Reference" %}.

<aside>
To search for commands whose name or help text contains given text,
define the following custom command and pass the text to it.
Later we will see how to define custom commands
so they are automatically available in new Nushell sessions.

```bash
def help-text [s: string] {
  help commands |
    select name description |
    append $(help commands | get subcommands ) |
    flatten |
    where description =~ $s || name =~ $s
}
```

</aside>

Like in all shells, commands are executed by typing them
and pressing the enter key.
Multi-line commands can be entered by pressing enter
before a block, delimited by square brackets or curly braces, is complete.

Nushell has great command recall and completion like the Fish and Zsh shells.
Command recall even supports multi-line command editing.

When a command not defined by Nushell is encountered,
the directories listed in the `path` configuration setting
are searched to find a matching executable.
To run a command in the `path` that happens to have the same name
as a Nushell command, prefix the command name with `^`.
For example, `^ls *.html` uses the `ls` command defined in root shell
rather than the version defined by Nushell.

For detailed documentation on Nushell commands, see the
{% aTargetBlank "https://www.nushell.sh/book/", "Book" %}
link in the top nav of the Nushell website.

For more help, join the {% aTargetBlank
"https://discord.gg/NtAbbGn", "nushell Discord channel" %}.

## Subcommands

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
`product`, `round`, `sqrt`, `stddev`, `sum`, and `variance`.
Also see the commands `inc`, `into-int`, `random`, and `seq`.

The `str` subcommands perform string operations.
They are
`camel-case`, `capitalize`, `collect`, `contains`, `downcase`, `ends-with`,
`find-replace`, `from`, `index-of`, `kebab-case`, `length`, `lpad`, `ltrim`,
`pascal-case`, `reverse`, `rpad`, `rtrim`, `screaming-snake-case`,
`snake-case`, `starts-with`, `substring`, `to-datetime`, `to-decimal`,
`to-int`, `trim`, and `upcase`.
Also see the commands `build-string`, `char`, `format`, and `size`.

The `url` subcommands get information from a URL.
They are `host`, `path`, `query`, and `scheme` (ex. http).

## Configuration

The configuration for Nushell is stored in a
{% aTargetBlank "https://github.com/toml-lang/toml", "TOML" %}
(Tom's Obvious, Minimal Language) file
whose path can be obtained by entering `config path`.
Configuration settings can be changed by editing this file
or using the `config` subcommands described below.
Changes to the config file only affect new shell sessions,
not the current one.

To edit the config file with Vim, enter `vim $(config path)`.
To edit the config file with VS Code, enter `code $(config path)`.
When editing the config file using VS Code,
consider installing the "TOML Language Support" extension
which profiles syntax highlighting and formatting.

<aside>
The syntax <code>$(pipeline)</code> is referred to as an "invocation".
This can be used to pass the result of a command pipeline
as a argument to a command.
</aside>

Some Nushell configuration settings are top-level
and do not appear in a specific TOML section.
Notable top-level options include:

| Setting                 | Description                                                                                       |
| ----------------------- | ------------------------------------------------------------------------------------------------- |
| `disable_table_indexes` | when `true`, omits index column from table output                                                 |
| `path`                  | list of directories in quotes to search for executables                                           |
| `prompt`                | command whose output is used for the prompt                                                       |
| `skip_welcome_message`  | when `true`, starting a shell doesn't output a<br>welcome message including the Nushell version   |
| `startup`               | list of commands to execute when a shell starts;<br>typically defines aliases and custom commands |
| `table_mode`            | controls the border lines drawn when tables are rendered<br>(more detail below)                   |

For details on configuration options, see {% aTargetBlank
"https://www.nushell.sh/book/configuration.html", "Configuration" %}.

The `config` command supports many subcommands that operate on the config file.
These are summarized in the table below.

| Command                                | Description                                     |
| -------------------------------------- | ----------------------------------------------- |
| `config`                               | outputs all the settings                        |
| `config path`                          | outputs the file path to the configuration file |
| `config set {name} {value}`            | sets or updates a specific setting              |
| `{pipeline} \| config set_into {name}` | sets a specific setting to a piped-in value     |
| `config get {name}`                    | gets a specific setting                         |
| `config remove {name}`                 | removes a specific setting                      |

For example, to change the prompt enter the following:

```bash
config set prompt "echo $(ansi yellow) '🦀ν> '"
```

This adds the line `prompt = "echo $(ansi yellow) '🦀ν> '"`
to the configuration file.
🦀 is for the Rust mascot Ferris and ν is the Greek letter nu.

For a fancier prompt setting that includes
the current working directory in yellow,
and a green, right-pointing triangle at the end,
add the following line in the config file:

{% raw %}

```bash
prompt =
  "echo `🦀ν {{$(ansi yellow)}}{{$(pwd)}}{{$(ansi green)}}{{$(char prompt)}} `"
```

{% endraw %}

Another option for customizing the prompt is
to enable the use of [Starship](/blog/starship),
which has many more options.
It also has the advantage that it can be used with nearly any shell,
which is great for users that sometimes switch between shells.

### `startup` Setting

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

Some code formatters reformat TOML arrays to be on a single line
which makes reading these definitions difficult.
VS Code and the "TOML Language Support" extension does not do this.

Aliases that use the "cd" command currently causes Nushell to crash.
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3138",
"issue 3138" %}.

### `table_mode` Setting

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

### `color_config` Section

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

The header elements and styles that can be configured include
`header_align`, `header_bold`, `header_color`, `index_color`,
`leading_trailing_space_bg`, and `separator_color` (used for table lines).
The data types that can be configured include
`primitive_binary`, `primitive_boolean`, `primitive_columnpath`,
`primitive_date`, `primitive_decimal`, `primitive_duration`,
`primitive_filesize`, `primitive_int`, `primitive_line`, `primitive_path`,
`primitive_pattern`, `primitive_range`, and `primitive_string`.

For example:

```toml
[color_config]
header_align = "l" # use left or l, right or r, and center or c
header_bold = true
header_color = "r"
index_color = "wd" # white dimmed
separator_color = "wd" # white dimmed; for table lines

primitive_binary = "c"
primitive_boolean = "p"
primitive_date = "u"
primitive_decimal = "g"
primitive_filesize = "g"
primitive_int = "g"
primitive_path = "y"
primitive_string = "w"
```

### `line_editor` Section

Nushell line editing is provided by
{% aTargetBlank "https://crates.io/crates/rustyline", "rustyline" %}.
It can be configured with the "[line_editor]" section.
Notable settings include:

| Setting                | Description                                                                                                                           |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `edit_mode`            | `"emacs"` or `"vi"`<br>default is to use {% aTargetBlank "https://github.com/kkawakam/rustyline#actions", "rustyline key bindings" %} |
| `history_duplicates`   | `"alwaysadd"` or `"ignoreconsecutive"` (default)                                                                                      |
| `history_ignore_space` | `true` (default) or `false`;<br>when true, commands with leading whitespace are not added to history                                  |

For example:

```toml
[line_editor]
edit_mode = "vi" # omit for default keystrokes (see rustyline link above)
history_ignore_space = true
```

### `path` setting

The `path` setting lists directories to be searched for executables.

To see a nicely formatted list of directories in your path,
enter `echo $nu.path` or `config | get path`.

The command `config set path $nu.path`
sets the `path` setting to the value of `$nu.path`,
which is the value of the `PATH` environment variable in the parent shell.
Once set, this can be customized to be specific to Nushell.
Note that if Nushell is your login shell then
there is no parent shell from which to inherit a path.

### `env` Section

The command `config set env $nu.env` adds all the current
environment variables in the `env` section of the config file.
This section can also define environment variables
that are specific to Nushell.
Note that if Nushell is your login shell then
there is no parent shell from which to inherit environment variables.

For example:

```toml
[env]
GITHUB_USER = "mvolkmann"
```

### `textview` Section

These settings affect operation of the `bat` crate
which is used by the `open` command to view text files.

| Setting                  | Description                                 |
| ------------------------ | ------------------------------------------- |
| grid                     | `true` (default) or `false`                 |
| header                   | `true` (default) or `false`                 |
| line_numbers             | `true` (default) or `false`                 |
| theme                    | ex. "Coldark-Dark"                          |
| true_color               | `true` or `false`                           |
| vcs_modification_markers | `true` or `false` (seems to have no effect) |

TODO: Setting vcs_modification_markers to true has no effect.
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3254",
"issue 3254" %}.

To see the supported themes, install `bat` by entering `cargo install bat`
and enter `bat --list-themes`. There are over 20.

## Data Types

Unlike most shells where only strings are used for command input and output,
Nushell supports many primitive and structured data types.
Some types described in the Nushell documentation are purely conceptual,
meaning that they cannot be used as the type of a custom command parameter.
The following tables list all the real types and conceptual types.

Real Types:

| Type      | Description                                                                                    |
| --------- | ---------------------------------------------------------------------------------------------- |
| `any`     | any type below (default for variables and custom command parameters)                           |
| `block`   | block of nu script code (can be executed on each row of a table)                               |
| `int`     | whole number with infinite precision                                                           |
| `number`  | `int` or `decimal`, both with infinite precision                                               |
| `path`    | platform-independent path to a file or directory                                               |
| `pattern` | glob pattern that can include `*` wildcard and `**` for traversing directories                 |
| `range`   | `{start}..{end}` (inclusive) or `{start}..<{end}` (end is exclusive); use 2 dots, not 3        |
| `string`  | single words need no delimiter; multiple words need single quotes, double quotes, or backticks |
| `table`   | list of rows; returned by many Nushell commands                                                |
| `unit`    | any value with a unit; includes `duration` and `filesize` types                                |

The ability to specify single-word literal strings with no delimiters
is one of my favorite features in Nushell!
It makes defining lists and tables that contain single-word strings
much more compact.
For example, the following line creates a list of color names.

```bash
let colors = [red orange yellow green blue purple]
```

Conceptual Types:

| Type        | Description                                                                                  |
| ----------- | -------------------------------------------------------------------------------------------- |
| binary      | sequence of raw bytes                                                                        |
| boolean     | literal values are `$true` and `$false`                                                      |
| column path | dot-separated list of nested column names                                                    |
| date        | timezone-aware; defaults to UTC                                                              |
| decimal     | number with a fractional part and infinite precision                                         |
| duration    | number followed by a unit which can be `ms`, `sec`, `min`, `hr`, `day`, `wk`, `mon`, or `yr` |
| filesize    | number followed by a unit which can be `b`, `kb`, `mb`, `gb`, `tb`, or `pb`                  |
| group       | semicolon-separated list of pipelines where only output from the last is output              |
| line        | string with an OS-dependent line ending                                                      |
| list        | sequence of values of any type                                                               |
| row         | list where each value represents a column with an associated name                            |

To see a description of the value of an expression,
pipe it to the `describe` command.
This is not its real type.
Values of type of `int` are reported as `integer`.
This is likely a bug. See {% aTargetBlank
"https://github.com/nushell/nushell/issues/3206", "issue 3206" %}.
For example, `date now | describe` outputs `date`.

<aside>
It seems like many of the conceptual types should become real types long-term.
I would think at least these could become real types:
boolean, date, decimal (remove number?), duration, filesize, and list.

Related to this is the fact that the `describe` command
doesn't strictly return real type names.
One of the oddities is that if you pipe an `int` value to `describe`,
it outputs "integer".
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3206",
"issue 3206" %}.
Maybe it would be good to add a `type` command
that returns the real type of a value.

</aside>

Details about Nushell data types can be found at {% aTargetBlank
"https://www.nushell.sh/book/types_of_data.html", "Types of data" %}.

### Type Conversions

The following type conversions are supported:

TODO: Fill in the ??? in this table.

TODO: How can you create values with types binary, line, path, pattern, and row?

| From      | To       | Command                                            |
| --------- | -------- | -------------------------------------------------- |
| `any`     | `string` | pipe to `describe`                                 |
| boolean   | `string` | pipe to `str from`                                 |
| date      | `string` | pipe to `str from`                                 |
| `int`     | `string` | pipe to `str from`                                 |
| list      | `string` | pipe to `str collect` if the list contains strings |
| `number`  | `string` | pipe to `str from`                                 |
| `path`    | `string` | pipe to `???`                                      |
| `pattern` | `string` | pipe to `???`                                      |
| `range`   | `string` | pipe to `???`                                      |
| `table`   | `string` | pipe to `???`                                      |
| `unit`    | `string` | pipe to `str from` \*                              |
| `string`  | boolean  | pipe to `???`                                      |
| `string`  | date     | pipe to `str to-datetime`                          |
| `string`  | `int`    | pipe to `str to-int`                               |
| `string`  | list     | pipe to `split row`                                |
| `string`  | `number` | pipe to `str to-decimal`                           |
| `string`  | `path`   | pipe to `???`                                      |
| `string`  | `range`  | pipe to `???`                                      |
| `string`  | `table`  | pipe to `???`                                      |
| `string`  | `unit`   | pipe to `???`                                      |

\* TODO: This gives an error. Why?

The `echo` command is often used to
feed the initial value into a command pipeline.
This can be a literal value or an expression such as a variable reference.
For example:

```bash
echo "2021-3-21 14:30" |
  str to-datetime |
  date format -t '%B %-d, %Y' |
  get formatted
# This outputs the string "March 21, 2021".
```

Formatting characters supported by `date format` are described
{% aTargetBlank "https://man7.org/linux/man-pages/man1/date.1.html", "here" %}.

This handy custom command produces a string from the items in a list
where the value of each item is followed by a newline character.
It is used in several examples that follow.
Consider adding this to the startup array in your Nushell config file.

```bash
# The `each` command iterates over list items that are piped to this command.
# During the iteration, the special variable $it (for item)
# is set to the current item.
def as-lines [] {
  each { echo $(build-string $it $(char newline)) } | str collect
}
```

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
# As discussed in the "Operators" section later,
# operators can only be used in "math mode".
# An expression is in math mode if it begins with `=`.
```

{% endraw %}

The operators `=~` and `!~` test whether
one string contains or does not contain another.

For example:

```bash
= "foobarbaz" =~ "bar" # true
= "foobarbaz" !~ "bar" # false
```

The `lines` command creates a list of strings
from a string that contains newline characters.
Nushell doesn't currently support escape characters like `\n`,
so the `char` command must be used to insert newlines into a literal string.
For example:

{% raw %}

```bash
let s = `This{{$(char newline)}}has{{$(char newline)}}three lines.`
let l = $(echo $s | lines) # ["This", "has", "three lines."]
```

{% endraw %}

### Ranges

Values of the `range` type can use default values for their start or end.
If the start value of a range is omitted, it defaults to zero.
For example, `..10` is the same as `0..10`.
If the end value of a range is omitted, the range has no upper bound.

The get the starting and ending values of a `range`,
pipe it to the `first` and `last` commands.

For example:

```bash
echo 3..7 | first # 3
echo 3..<7 | last # 6
echo 1..3 | as-lines # outputs 1, 2, and 3 on separate lines
```

### Types With Units

Duration values with different units can be added.
For example, `2hr + 57min + 11sec` (my best marathon time).

Values of the `filesize` type with different units can be added.
For example, `2mb + 57kb + 11b`.

When values of different types are combined in a coercion error occurs.
For example, `3hr + 2mb` gives this error and clearly identifies
that the first value is a `duration` and the 2nd is a `filesize`.

### Lists

The literal syntax for creating a `list` is to include expressions
in square brackets separated by spaces or commas (for readability).
For example, `["foo" "bar" "baz"]` or `["foo", "bar", "baz"]`.

To iterate over the elements in a list, use the `each` command.
For example:

```bash
let names = [Mark Tami Amanda Jeremy]
echo $names | as-lines # outputs each name on a separate line
```

The `split row` command creates a list from a string based on a delimiter.
For example, `let colors = $(echo "red,green,blue" | split row ",")`
creates the list `[red green blue]`.

The `$it` special variable holds the output of the previous command.
When used in a block passed to the `each` command,
it holds the current iteration value.

To access a list element at a given index, use `$name.index`
where `$name` is a variable that holds a list.
For example, the second element in the list above
which is "Tami" can be accessed with `$names.1`.

The `in` and `not in` operators are used to test whether a value is in a list.
For example:

```bash
let colors = [red green blue]
# As discussed in the "Operators" section later,
# operators can only be used in "math mode".
# An expression is in math mode if it begins with `=`.
= blue in $colors # true
= yellow in $colors # false
```

The `where` command can be used to create a subset of a list.
The following example gets all the colors whose names end in "e".

```bash
let colors = [red orange yellow green blue purple]
echo $colors | where {= $(echo $it | str ends-with 'e')}
# The block passed to where must evaluate to a boolean.
# This outputs the list [orange blue purple].
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
= $scores | empty? # true; not empty if only header row is present
```

The `append` command appends a value to the end of a list.
The `prepend` command prepends a value to the beginning of a list.
For example:

```bash
let colors = [yellow green]
let colors = $(echo $colors | prepend red)
let colors = $(echo $colors | append purple)
echo $colors
```

This outputs the following:

```text
╭───┬────────╮
│ 0 │ red    │
│ 1 │ yellow │
│ 2 │ green  │
│ 3 │ purple │
╰───┴────────╯
```

The `flatten` command creates a new list from an existing list
by adding items in nested lists to the top-level list.
This can be called multiple times to flatten lists nested at any depth.
For example:

```bash
echo [1 [2 3] 4 [5 6]] | flatten # [1 2 3 4 5 6]

echo [[1 2] [3 [4 5 [6 7 8]]]] |
  flatten | flatten | flatten # [1 2 3 4 5 6 7 8]
```

The `length` command returns the number of items in a list.
For example, `echo [red green blue] | length` outputs `3`.

The `reduce` command computes a single value from a list.
It takes a block which can use the special variables
`$acc` (for accumulator) and `$it` (for item).
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

The literal syntax for creating a table defines each row with a list,
starting with the header row which is followed by a semicolon.
For example, `echo [[Name Score]; [Mark 19] [Tami 21]]`
outputs the following table:

```text
╭───┬──────┬───────╮
│ # │ Name │ Score │
├───┼──────┼───────┤
│ 0 │ Mark │    19 │
│ 1 │ Tami │    21 │
╰───┴──────┴───────╯
```

SQL-like syntax can be used to retrieve data from a table.
For example:

```bash
let scores = [[Name Score]; [Mark 19] [Tami 21]]
echo $scores | where Name == 'Tami' | get Score # 21
```

The use of the `where` command above is shorthand for the expanded syntax.
This uses the special variable `$it` which holds
the result of the previous command or the current iteration value.
The previous pipeline can be written as shown below using this syntax.
The curly braces after the `where` command define a block on which it operates.
As we have seen, the leading `=` enters "math mode"
which enables use of operators.

```bash
echo $scores | where { = $it.Name == 'Tami'} | get Score # 21
```

Single-row tables can be used like objects in other languages.
For example:

```bash
let data = [[color flavor]; [yellow vanilla]]
echo $data.color # outputs yellow
echo $data.flavor # outputs vanilla
```

Nushell is currently very picky about
splitting table data over multiple lines.
The newlines in the table definition below break it!
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3204",
"issue 3204" %}.

```bash
# This way of adding newlines in a table definition is not parsed correctly.
let sports = [
  [name players];
  [baseball 9]
  [basketball 5]
  [football 11]
  [hockey 6]
]
# This way is parsed correctly.
let sports = [
  [name players
  ]; [baseball 9
  ] [basketball 5
  ] [football 11
  ] [hockey 6]
]
let sport = basketball
let players = $(echo $sports | where name == $sport | get players)
echo `The number of active players in {{$sport}} is {{$players}}.`
```

Tables can contain nested tables.
Note the placement of newlines in the example below
which avoids the parsing issue described above.
The `to json` command is used to generate JSON from a table.

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

Parentheses can be used for grouping to specify evaluation order.
Operators can only be used in "math mode".
An expression is in math mode if it begins with `=`.
Commands that take a boolean expression, such as
`where`, `keep while`, `keep until`, `skip while`, and `skip util`,
are automatically evaluated in math mode.

For example, `let a = 2; let b = 3; = $a * $b` outputs `6`.

## Working with Numbers

Many of the operators listed in the previous section operate on numbers.

The `inc` command has three uses.

The first use of `inc` is to
return a value that is one higher that the piped in value.
For example, `echo 2 | inc` gives `3`.
Since variables are immutable,
this cannot be used to increment the value in a variable.

The second use of `inc` is to increment all the `int` values
in a given table column if all the values are of type `int`.
For example, `echo [[Name Size]; [Mark 33] [Tami 28]] | inc Size`
results in a table where the values in the "Size" column are 34 and 29.

The third use of `inc` is to increment
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
  sort-by title | keep 3
```

This produces output like the following:

```text
╭───┬────────┬─────┬─────────────────────────────────────────┬───────────╮
│ # │ userId │ id  │ title                                   │ completed │
├───┼────────┼─────┼─────────────────────────────────────────┼───────────┤
│ 0 │      5 │  97 │ dolorum laboriosam eos qui iure aliquam │ false     │
│ 1 │      5 │ 100 │ excepturi a et neque qui expedita vel   │ false     │
│ 2 │      5 │  94 │ facilis modi saepe mollitia             │ false     │
╰───┴────────┴─────┴─────────────────────────────────────────┴───────────╯
```

The `post` command sends an HTTP POST requests to a server
and returns the response as a table.
The following example simulates creating a TODO using the
JSONPlaceholder site and returns the id of the newly created TODO.

```bash
let json = '{"title": "get milk", "userId": 5}"'
post https://jsonplaceholder.typicode.com/todos $json
```

Nushell does not currently provide commands
to send `PUT`, `PATCH`, or `DELETE` requests.

## Common UNIX Commands

Many common UNIX commands are supported by Nushell.
These are implemented in Rust and are very fast.
They include:

| Command    | Description                                                                           |
| ---------- | ------------------------------------------------------------------------------------- |
| `cal`      | displays a calendar for the current month or an entire specified year                 |
| `cd`       | changes the current working directory                                                 |
| `clear`    | clears the terminal                                                                   |
| `cp`       | copies a file or directory                                                            |
| `date now` | gets the current date (see the other `date` subcommands)                              |
| `du`       | gets information about disk usage                                                     |
| `echo`     | outputs the values of expressions                                                     |
| `exit`     | exits the current shell; can specify a status code                                    |
| `help`     | outputs help information                                                              |
| `history`  | outputs command history (last 100 commands)                                           |
| `kill`     | kills a process                                                                       |
| `ls`       | lists the contents of the current directory or specified path                         |
| `mkdir`    | makes (creates) a directory                                                           |
| `mv`       | moves a file or directory                                                             |
| `open`     | opens a file                                                                          |
| `ps`       | outputs process information                                                           |
| `pwd`      | outputs the present (current) working directory                                       |
| `rm`       | removes (deletes) a file or directory                                                 |
| `source`   | executes a script file in the current context                                         |
| `which`    | outputs the path of an executable OR<br>information about aliases and custom commands |

The change to a subdirectory named "sub", enter `cd sub` or just `sub`.

The `cp`, `mv`, and `rm` commands do not currently
support the `-i` flag to prompt for confirmation.
However, aliases for these can be defined to
use the corresponding commands from the parent shell.
For example, `alias rm = ^rm -i`.

Commands such as `ls`, `ps`, and `sys` output data as a table.
When a cell displays `[table n rows]`,
that indicates that it contains a nested table.
Use the `get` filter to display them (see examples below).

SQL-like filters such as `where`, `sort-by`, and `reverse`
can be used to modify the output.
The `sort-by` command accepts
the `--insensitive` (`-i`) flag to make the sort case-insensitive and
the `--reverse` (`-r`) flag to reverse the sort order.

Let's walk through some examples.

```bash
# List all the package.json files in and below the current directory
# using a glob pattern.
ls **/package.json

# List Rust files in and below the current directory
# using a glob pattern.
ls **/*.rs

# List files in a tree layout.
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
# sorted on CPU usage in reverse order.
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
For example, to generate JSON from the `ls` command output,
enter `ls | to json`.

The `save` command writes the string output of a command to a file.
This is alternative to the `>` redirect operator used in the Bash shell.
The `to` command can be used to convert non-string data to a string.

For example:

```bash
# Create an HTML file describing the directories
# in the current directory that are larger than 1 KB.
ls | where type == Dir && size > 1024 | sort-by -r size | to html |
  save big-dirs.html

# The text format defaults to CSV if the file extension is .csv.
# It does not do this for any other file extensions.
ls | where type == Dir && size > 1024 | sort-by -r size | save big-dirs.csv
```

We can compute the combined size of all the executables
installed using the Rust `cargo` command in the default location.
This includes the `nu` executable and plugins.

```bash
ls $(build-string $(which nu | get path) '*') | get size | math sum
```

## Aliases

To create an alias for a command, enter `alias {name} = {command}`.
For example, `alias cls = clear`.

The command can be a built-in command or a custom command,
and literal arguments can be specified.
When aliases are used, additional arguments can be specified.
For example:

```bash
alias df = date format
date now | df "%B %-d, %Y" # April 3, 2021
```

To make aliases available in each new Nushell session,
add them to the `startup` list in the config file
as shown in the "Configuration" section.

Aliases cannot use pipelines. Custom commands must be used instead.
For example, the following does not work:

```bash
alias top = ps | sort-by -r cpu | first 10
```

But defining it as as custom command as follows does work:

```bash
def top [] { ps | sort-by cpu -r | first 10 }
```

## Custom Commands

To define a custom command, enter `def {name} [params] { commands }`.
Names can be in kebab-case, including hyphens for readability.
They can end with `?` to indicate that they return a Boolean value.
Square brackets are used to surround the parameters because
they are treated as a list and that syntax is used for lists.
If no parameters are required, `[]` must still be included.

The result of a custom command is
the result of the last command pipeline
or a string formed by the accumulation of everything it echoes.

The following custom command has no parameters and outputs a table
showing the processes that have CPU usage of 5% or more
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

To run this enter `top` followed by a number.
For example, `top 5`.

To make custom commands available in each new Nushell session,
add them to the `startup` list in the config file
as shown in the "Configuration" section.

The type of each parameter can optionally be specified after a colon to
provide better documentation and better error messages when used incorrectly.
See the list of "real types" described in the "Data Types" section.
For example:

```bash
def sum [n1: number, n2: number] { = $n1 + $n2 }

def topn [pct: number] { ps | where cpu >= $pct | sort-by -r cpu }
```

When invoking a command, multiple arguments are separated by spaces.
For example, entering `sum 1 2` which outputs `3`.

Here are examples of custom commands whose result is defined by what they echo
rather than the result of a command pipeline.

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
and accessed with the `$it` special variable.
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
  # echo [`{{$value}}` $(char newline)] | str collect
}

# Prints "name = value" followed by a newline.
def lognv [name: string, value: any] {
  logv $(build-string $name " = " $value)
}

def logv-color [
  text: string,
  --color (-c): string # a flag
] {
  #if $(echo $color | empty?) { # same as next line
  if $(= $color | empty?) {
    logv $text
  } {
    logv $(build-string $(ansi $color) $text $(ansi reset))
  }
}

logv-color "Giraffes are cool!" -c "yellow"
```

{% endraw %}

Custom commands can take arguments with a type of `block`.
The `do` command can be used to execute the block.
For example, this can be used to implement a `map` command
which takes a list and a block.

```bash
def map [values: any, code: block] {
  echo $values | each $code
}

let names = [Mark Tami Amanda Jeremy]

map $names {
  echo $(build-string "Hello, " $it $(char newline))
} | str collect

# Same result using built-in "each" command instead of custom "map" command.
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

Defining a custom subcommand is similar to defining a custom command,
but the name is specified as the parent command name and the subcommand name
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
# The "lognv" command is defined above.
lognv 'score' $(rmv double $(rmv increment $score)) # 8
```

## Variables

Variables in Nushell are distinct from environment variables.
They are immutable, so they must be set when they are declared.
However, they can be shadowed to have different values in different scopes.

To set a variable, enter `let name = value`.
The scope of a variable is the context or block in which it is defined.

To set a variable to the result of a command pipeline,
which can be comprised of one or more commands,
enter `let name = $(pipeline)`.

To use a variable in an expression, precede its name with `$`.
For example, `$total`.

The `build-str` command concatenates the values of multiple expressions
into a single string.
For example, the following sets a variable to the result of
concatenating two variable values as strings:

```bash
let v3 = $(build-str $v1 $v2)
```

## Environment Variables

Environment variables are distinct from regular variables.
Unlike regular variables, environment variables can be
accessed by executables that are run from Nushell.

To set the value of an environment variable only in the current scope,
not permanently, enter `let-env NAME = value`.
This adds `NAME` to `$nu.env` which holds a map of environment variables.

To set the value of an environment variable
so it is available in subsequent Nushell sessions,
add it to the `env` section of the Nushell configuration file
as shown in the "Configuration" section.

To get the value of an environment variable, use `$nu.env.NAME`.
To print the value, enter  
`echo $nu.env.NAME` or `config | get env.{name}`.

To see a nicely formatted list of environment variables, enter  
`echo $nu.env | pivot` or `config | get env | pivot`.

## open Command

The `open` command renders certain file types as tables.
These file types include csv, ini, json, toml, xml, and yaml.

Consider the file `scores.csv` containing the follow:

```text
Name,Score
Mark,19
Tami,21
Amanda,17
Jeremy,15
```

The command `open scores.csv` renders this as follows:

```text
╭───┬────────┬───────╮
│ # │ Name   │ Score │
├───┼────────┼───────┤
│ 0 │ Mark   │    19 │
│ 1 │ Tami   │    21 │
│ 2 │ Amanda │    17 │
│ 3 │ Jeremy │    15 │
╰───┴────────┴───────╯
```

When run on a JSON file, the `open` command produces a table
from which specific data can be extracted.
For example, the following outputs a table of scripts in a `package.json` file.

```bash
open package.json | get scripts | pivot
```

The output will be similar to the following:

```text
╭───┬─────────┬────────────────────────────────╮
│ # │ Column0 │ Column1                        │
├───┼─────────┼────────────────────────────────┤
│ 0 │ dev     │ svelte-kit dev                 │
│ 1 │ build   │ svelte-kit build               │
│ 2 │ start   │ svelte-kit start               │
│ 3 │ lint    │ prettier --check . && eslint . │
│ 4 │ format  │ prettier --write .             │
│ 5 │ open    │ svelte-kit dev --open          │
╰───┴─────────┴────────────────────────────────╯
```

There is no way to output a table without the heading row.
The "#" column can be suppressed from all table output by setting
the `disable_table_indexes` configuration option to `true`,
but there is no way to do this for the output of a specific command.

To see the commands in the Nushell configuration file `startup` section, enter  
`open $(config path) | get startup`.

The `lines` and `split` commands can be used
to render delimited data as a table.
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

The following command renders a table where
each row describes a programming language,
the columns have proper names, and
the rows are sorted on ascending year of creation:

```bash
open languages.txt | lines | split column '|' Language Year Creator |
  sort-by Year
```

The `lines` command converts input text
into a list of separate lines from the text.
The command above produces the following output.

```text
╭───┬────────────┬──────┬────────────────────╮
│ # │ Language   │ Year │ Creator            │
├───┼────────────┼──────┼────────────────────┤
│ 0 │ Python     │ 1991 │ Guido van Rossum   │
│ 1 │ Java       │ 1995 │ James Gosling      │
│ 2 │ JavaScript │ 1995 │ Brendan Eich       │
│ 3 │ Ruby       │ 1995 │ Yukihiro Matsumoto │
│ 4 │ Rust       │ 2010 │ Graydon Hoare      │
│ 5 │ Go         │ 2012 │ Rob Pike           │
│ 6 │ TypeScript │ 2012 │ Anders Hejlsberg   │
╰───┴────────────┴──────┴────────────────────╯
```

The `open` command treats other types of files as a list of lines and
produces a table where the first column contains line numbers.
For known programming language file extensions,
the `open` command uses the `bat` crate which provides syntax highlighting.
Compare this to using the `cat` command where this does not happen.

If the file extension on a file does not match its content type,
use the `from` command to specify the actual content type.
For example, `open really-json.txt | from json`.

To prevent the `open` command from processing a file, add the `--raw` flag.
For example, `open scores.csv --raw` outputs the following:

```text
───────┬────────────
       │ scores.csv
───────┼────────────
   1   │ Name,Score
   2   │ Mark,19
   3   │ Tami,21
   4   │ Amanda,17
   5   │ Jeremy,15
───────┴───────────
```

To process data from a URL instead of a local file, use the `fetch` command.
For example, the
{% aTargetBlank "https://jsonplaceholder.typicode.com", "{JSON} Placeholder" %}
site provides free data for testing and prototyping.
Todo data from this site can be rendered as a table with the following commands:

```bash
fetch https://jsonplaceholder.typicode.com/todos | first 10

fetch https://jsonplaceholder.typicode.com/todos |
  where userId == 2 && completed == $true | sort-by title
```

## Table Commands

The many Nushell commands that operate on tables
are summarized in the table below.
Examples of using many of them appear in the sub-sections that follow.

| Command                    | Description                                                                                                               |
| -------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `append`                   | creates a new table by appending a single row to an existing table                                                        |
| `autoview`                 | renders data as a table or list                                                                                           |
| `compact`                  | removes empty rows                                                                                                        |
| `drop n`                   | removes the last `n` rows (`n` defaults to 1)                                                                             |
| `drop column n`            | removes the last `n` columns (`n` defaults to 1)                                                                          |
| `each`                     | runs a block of code on each row                                                                                          |
| `every n`                  | shows (default) or skips (with `-s` option) every `n`th row                                                               |
| `first n`                  | shows only the first `n` rows (`n` defaults to 1); alternative to `keep`                                                  |
| `flatten`                  | flattens a table or list, turning nested values into top-level values                                                     |
| `format`                   | formats specified columns into a single string using a pattern                                                            |
| `from {format}`            | parses a given file format into a table                                                                                   |
| `get {column-name}`        | gets the content of a given column                                                                                        |
| `group-by`                 | creates multiple tables from one based on some grouping                                                                   |
| `headers`                  | creates a table from an existing one where the first row replaces the current column headers                              |
| `histogram`                | creates a table with "value", "count", "percentage", and "frequency"<br>columns based on a given column in an input table |
| `insert`                   | inserts a column                                                                                                          |
| `keep n`                   | keeps the first `n` rows (`n` defaults to 1); alternative to `first`                                                      |
| `keep until {condition}`   | keeps rows until the condition is met                                                                                     |
| `keep while {condition}`   | keeps rows while the condition is met                                                                                     |
| `last n`                   | shows only the last `n` rows (`n` defaults to 1)                                                                          |
| `length`                   | counts rows or list items                                                                                                 |
| `match`                    | filters rows by matching the values in given column against a regular expression                                          |
| `merge`                    | creates a new table by merging the columns of existing tables                                                             |
| `move`                     | moves columns to another position                                                                                         |
| `nth`                      | keeps or skips specified rows                                                                                             |
| `parse`                    | creates a single-row table by parsing columns from a string according to a pattern                                        |
| `pivot`                    | swaps the rows and columns of a table                                                                                     |
| `prepend`                  | prepends a row to a table                                                                                                 |
| `range`                    | gets a subset of rows                                                                                                     |
| `reject`                   | removes columns by name                                                                                                   |
| `rename`                   | renames columns                                                                                                           |
| `reverse`                  | reverses the order of the rows                                                                                            |
| `roll n`                   | rolls the bottom `n` rows to the top (`n` defaults to 1)                                                                  |
| `rotate`                   | rotates the table 90 degrees clockwise; can apply multiple times                                                          |
| `rotate counter-clockwise` | rotates the table 90 degrees counter-clockwise                                                                            |
| `select {column-names}`    | specifies columns to be retained by name and their order                                                                  |
| `shuffle`                  | shuffles the rows randomly                                                                                                |
| `skip n`                   | skips the first `n` rows (`n` defaults to 1)                                                                              |
| `skip until {condition}`   | skips rows until the condition is met                                                                                     |
| `skip while {condition}`   | skips rows while the condition is met                                                                                     |
| `sort-by`                  | sorts rows on given columns                                                                                               |
| `split column`             | creates a table from a string based on a delimiter                                                                        |
| `split-by`                 | creates a new table from one with nested tables<br>where column headings are values of a given nested table heading       |
| `table`                    | views pipeline output as a table                                                                                          |
| `to {format}`              | converts a table to a given format such as JSON                                                                           |
| `uniq`                     | gets unique rows                                                                                                          |
| `update`                   | updates data in a given column                                                                                            |
| `where`                    | specifies a condition rows must meet to render                                                                            |
| `wrap`                     | creates a table column from its data and a name                                                                           |

Let's look at some examples using the `ls` command.
This produces a table with the columns "name", "type", "size", and "modified".
Here is a command that lists the files
in the current directory with a `.ts` file extension,
only includes the "name" and "size" columns,
sorts the files from largest to smallest,
and only outputs the three largest files:

```bash
ls *.ts | select name size | sort-by -r size | first 3
```

This produces output similar to the following:

```text
╭───┬────────────────────────┬──────────╮
│ # │ name                   │ size     │
├───┼────────────────────────┼──────────┤
│ 0 │ lib.deno.unstable.d.ts │ 194.4 KB │
│ 1 │ lib.deno.d.ts          │ 145.8 KB │
│ 2 │ my_server.ts           │   2.7 KB │
╰───┴────────────────────────┴──────────╯
```

The row indexes in the first column can be used to
retrieve only a specific row using the `nth` command.
For example, adding `| nth 1` to the end of the command
causes it to only output the row for the file `lib.deno.d.ts`.

The following sub-sections provide more detail
on some of the table commands described above.

### `append` Command

The `append` command creates a new table by
appending a single row to an existing table.
For example:

```bash
let primaryColors = [[name red green blue]; [
  red 255 0 0] [
  green 0 255 0] [
  blue 0 0 255]
]
let purple = [[name red green blue]; [purple 255 0 255]]
let colors = $(echo $primaryColors | append $purple)
echo $colors
```

This produces the following output:

```text
╭───┬────────┬─────┬───────┬──────╮
│ # │ name   │ red │ green │ blue │
├───┼────────┼─────┼───────┼──────┤
│ 0 │ red    │ 255 │     0 │    0 │
│ 1 │ green  │   0 │   255 │    0 │
│ 2 │ blue   │   0 │     0 │  255 │
│ 3 │ purple │ 255 │     0 │  255 │
╰───┴────────┴─────┴───────┴──────╯
```

### `each` Command

The `each` command runs a block of code on each row of a table.
For example:

```bash
ls *.nu | each {
  let name = $(echo $it | get name)
  let size = $(echo $it | get size)
  echo $(build-string $name ' is ' $size '.')
} | as-lines
```

This produces output like the following:

```text
append-demo.nu is 670 B.
block-param.nu is 305 B.
chart-demo.nu is 189 B.
```

### `flatten` Command

The `flatten` command operates on both lists and tables.
It can create a new table from an existing table,
replacing columns that whose values are nested tables
with the columns in those tables.
For example, the `sys` command creates a table with the columns
"host", "cpu", "disks", "mem" (for memory),
"temp" (for temperature), and "net" (for network activity).
Piping this output to the `flatten` command
replaces the "host" and "mem" columns with the columns in their nested tables.

### `format` Command

The `format` command formats specified columns
into a single string using a pattern.
For example:

```bash
ls | format '{name} is a {size} {type} and was modified {modified}.' |
  str downcase
# downcase is used to change the type to lowercase.
```

This outputs lines like the following:

```text
histogram.nu is a 233 b file and was modified 1 week ago.
```

### `from` Command

The `from` command parses a given file format into a table.
For example:

```bash
let data = $(open --raw scores.csv)
let table = echo $data | from csv
```

This can be done in a single line with `let table = $(open scores.csv)`.

TODO: How could you iterate over the rows of a table
and add each one to another table? Use reduce and append?
See `append-demo.nu`.

### `get` Command

The `get` command can be used to output only the values in a single column.
It returns a list rather than a table.
For example, the following outputs the
names of the three largest TypeScript files in the current directory:

```bash
ls *.ts | sort-by size | reverse | first 3 | get name
```

The `get` command is especially useful when the type of a field is "table".
The key can be arbitrarily deep with sub-keys separated by periods.
For example, `sys | get host.sessions | where name == 'root' | get groups`.

The following example demonstrates getting all the headings from a table.

```bash
echo $myTable | get
```

Table columns can be accessed by their header name
using the `get` and `select` commands,
but there is no command to get a table column by its index.
However, this can be done with the following pipeline:

```bash
echo $myTable | select $(echo $myTable | get | nth $columnIndex)
```

### `group-by` Command

The rows of a table can be segregated into multiple tables
using the `group-by` command.
For example, the output of `ls` can be split into two tables
where one contains rows with a type of "File"
and the other contains rows with a type of "Dir".
The following command produces a table with the columns "File" and "Dir"
that contains a single row whose cells are themselves tables.

```bash
ls | group-by type
```

The following code outputs the nested tables:

```bash
let temp = $(ls | group-by type)
echo $temp | get File
echo $temp | get Dir
```

The `group-by` command can be passed a block
that computes the value used to group the rows.
The following example groups files based on their file extension:

```bash
ls | group-by { get name | path extension }
```

To see the contents of one of the nested tables, pipe this to `get ext-name`.

Another way to see only the files whose name ends with certain characters
is the use the `where` command with a block as follows:

```bash
ls | where {= $(echo $it.name | str ends-with ".rs") } | get name
```

### `histogram` Command

The `histogram` command generates a histogram
from the data in a given table row.
For example:

```bash
# The newlines need to be placed like this
# due to a parser bug.
let data = [
  [name color]; [
  Mark yellow] [
  Tami blue] [
  Amanda green] [
  Jeremy yellow] [
  Sally blue] [
  Sam yellow]
]
echo $data | get color | histogram
```

This produces the following table:

```text
╭───┬────────┬───────┬────────────┬──────────────────────────────────────────╮
│ # │ value  │ count │ percentage │ frequency                                │
├───┼────────┼───────┼────────────┼──────────────────────────────────────────┤
│ 0 │ blue   │     2 │ 66.67%     │ **************************               │
│ 1 │ green  │     1 │ 33.33%     │ *************                            │
│ 2 │ yellow │     3 │ 100.00%    │ **************************************** │
╰───┴────────┴───────┴────────────┴──────────────────────────────────────────╯
```

The percentage values are double what you might expect.
This is because the value when the largest count is assigned a percentage of 100
and all the other percentages are calculated relative to that count.
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3215",
"issue 3215" %}.

### `split` and `headers` Commands

The table in the previous example can be created from a string
using the `split` and `headers` commands.

```bash
let data =
  "name red green blue|red 255 0 0|green 0 255 0|blue 0 0 255|purple 255 0 255"
let colors = $(echo $data | split row "|" | split column " " | headers)
echo $colors
```

The `split-by` command doesn't seem very useful.

TODO: See `split-by-demo.nu`.

### `match` Command

The `match` command filters rows by matching the
values in given column against a regular expression.
For example, `ls | where type == File | match name "^c.*\.nu$"`
lists files in the current directory whose name
begin with "c" and have a file extension of ".nu".

### `merge` Command

The `merge` command creates a new table
by merging the columns of existing tables.
For example:

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

### `move` Command

The `move` command moves specified columns to after or before another column.
For example, by default the `ls` command outputs a type
with the columns "name", "type", "size", and "modified".
The following command moves the "type" and "size" columns
to be after the "modified" column.
It also reorders the "size" and "type" columns to the specified order.

```bash
ls | move size type --after modified
```

### `parse` Command

The `parse` command creates a single-row table
by parsing columns from a string using a pattern.
For example:

```bash
echo "123 Some St., St. Charles, MO 63304" |
  parse "{street}, {city}, {state} {zip}"
```

This outputs the following table:

```text
╭───┬──────────────┬─────────────┬───────┬───────╮
│ # │ street       │ city        │ state │ zip   │
├───┼──────────────┼─────────────┼───────┼───────┤
│ 0 │ 123 Some St. │ St. Charles │ MO    │ 63304 │
╰───┴──────────────┴─────────────┴───────┴───────╯
```

### `pivot` Command

The `pivot` command swaps the rows and columns of a table.
For example,  
`ls *.nu | sort-by -r size | first 3 | pivot`  
produces output like the following:

```text
╭───┬──────────┬────────────┬──────────────────┬────────────────╮
│ # │ Column0  │ Column1    │ Column2          │ Column3        │
├───┼──────────┼────────────┼──────────────────┼────────────────┤
│ 0 │ name     │ rmv.nu     │ split-by-demo.nu │ append-demo.nu │
│ 1 │ type     │ File       │ File             │ File           │
│ 2 │ size     │     1.3 KB │            927 B │          670 B │
│ 3 │ modified │ 1 week ago │ 1 week ago       │ 4 days ago     │
╰───┴──────────┴────────────┴──────────────────┴────────────────╯
```

### `prepend` Command

The `prepend` command creates a new table by
prepending a single row to an existing table.
For example:

```bash
let primaryColors = [[name red green blue]; [
  red 255 0 0] [
  green 0 255 0] [
  blue 0 0 255]
]
let white = [[name red green blue]; [white 255 255 255]]
let colors = $(echo $primaryColors | prepend $white)
echo $colors
```

This does not currently produce the expected output,
but piping the output to the `flatten` command fixes it.
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3269",
"issue 3269" %}.

### `split column` Command

The `split column` command creates a table from a string based on a delimiter.
For example, `echo "red,green,blue" | split column ","`
produces the following output:

```text
╭───┬─────────┬─────────┬─────────╮
│ # │ Column1 │ Column2 │ Column3 │
├───┼─────────┼─────────┼─────────┤
│ 0 │ red     │ green   │ blue    │
╰───┴─────────┴─────────┴─────────╯
```

### `to` Command

The `to` command converts a table to a given format such as JSON.
It is followed by a format name which can be one of the following:
`csv`, `html`, `json`, `md`, `toml`, `tsv`, `url`, `xml`, and `yaml`.
The `toml`, `url`, and `xml` formats are only applicable on tables
that meet specific requirements.
For example:

```bash
let scores = [[Name Score]; [Mark 19] [Tami 21]]
echo $scores | to csv
echo $scores | to json
echo $scores | to md
echo $scores | to yaml
```

This produces the following output:

```text
Name,Score
Mark,19
Tami,21

[{"Name":"Mark","Score":19},{"Name":"Tami","Score":21}]

|Name|Score|
|-|-|
|Mark|19|
|Tami|21|

---
- Name: Mark
  Score: 19
- Name: Tami
  Score: 21
```

## Plugins

Nushell plugins add new commands.
Supported plugins can be found in the {% aTargetBlank
"https://github.com/nushell/nushell/tree/main/crates", "crates" %}
directory of the Nushell GitHub repository.
Look for subdirectories whose names being with "nu_plugin\_".
All of these are installed by default if Nushell is installed
using the command `cargo install nu --features=extra`.

Additional plugins can be installed using the Rust `cargo` utility.
Specify the directories where these plugins are installed
in the configuration setting `plugin_dirs` which is a
list of directories where Nushell should search for plugins.
After installing plugins and changing this setting,
open a new shell to gain access to the commands that they add.

The following sub-sections describe some commonly used plugins.

### nu_plugin_start

This plugin adds the `start` command that opens a given file
using its default application.
For example, `start demo.html` will
open the HTML file in the default web browser.

### nu_plugin_chart

This plugin adds the `chart` command with the subcommands `bar` and `line`.

Here is an example of creating both kinds of charts.

```bash
let data = [[name score]; [
  Mark 19] [
  Tami 21] [
  Amanda 17] [
  Jeremy 15]
]

echo $data | chart bar [name score]
echo $data | chart line [name score]
```

After each chart is rendered, press enter to go to the next chart
or exit if the last chart has been rendered.

Unfortunately these plot the frequencies of the values
and not values themselves.
So the bar chart shows four bars at 100% because
the values 19, 21, 17, and 15 each occur once.
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3096",
"issue 3096" %}.

## Default Shell

To make Nushell your default shell in Linux or macOS, first
add a line containing the file path to the `nu` executable in `/etc/shells`.
For example, using macOS I entered `su vim /etc/shells`
and added the line `/Users/mark/.cargo/bin/nu`.
Then change the default shell by entering `chsh -s nu`.
If using tmux, also change the value of `default-shell` in `~/.tmux.conf`.

## Scripts

Nushell scripts are written in files with a `.nu` extension.
To execute a script, enter `nu {name}.nu`.
To "source" a script so its definitions become available in the current shell,
enter `source {name}.nu`.

For examples of Nushell scripts, see {% aTargetBlank
"https://github.com/nushell/nu_scripts/tree/main/nu_101", "Nu_101 Scripts" %}.

To add comments or comment-out a line of code,
begin the comment with a `#` character.

Commands commonly used in Nushell scripts include
`def`, `if`, `each`, and `seq`.

As we saw in the "Custom Commands" section,
the `def` command defines a custom command
that can be used like a function in many programming languages.

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
The previous example can also be written as follows:

```bash
echo 1..4 | each { build-string $it $(char newline) } | str collect
```

Iteration over the elements of a list is implemented with the `each` command.
Its syntax is `echo some-list | each { block }`.
The block can use the special variable `$it`
to access the current item in the iteration.
For example:

```bash
def log-value [label, value] {
  echo $(build-string $label " = " $value $(char newline))
}

def report [list] {
  # Without the --numbered flag, $it is set to each list value.
  # With it, $it is set to an object with the properties index and item.
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

## VS Code

There is a VS Code extension for Nushell that
provides syntax highlighting for Nushell scripts.
See {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=TheNuProjectContributors.vscode-nushell-lang",
"vscode-nushell-lang" %}.

## Comparison to Bash

The following table shows the Nushell equivalent of some common Bash commands.

| Bash                    | Nushell                             | Description                                                                                  |
| ----------------------- | ----------------------------------- | -------------------------------------------------------------------------------------------- |
| `man command`           | `help {command}`                    | outputs help for a command;<br>`help commands` lists Nushell<br>commands and custom commands |
| `$PATH`                 | `$nu.path`                          | holds the list of directories<br>searched for executables                                    |
| `cat {file-path}`       | `open --raw {file-path}`            | prints the contents of a file;<br>omit `--raw` to output structured data                     |
| `command > {file-path}` | `command \| save --raw {file-path}` | saves command output to a file<br>without converting based on file extension                 |
| `mkdir -p foo/bar/baz`  | `mkdir foo/bar/baz`                 | creates directory structure,<br>including any missing directories                            |
| cmd1 && cmd2            | cmd1; cmd2                          | runs cmd1 and then only runs<br>cmd2 if cmd1 was successful                                  |

The command whose output is piped in the `save` command must produce a string.
For example,  
`date now | save --raw timestamp.txt` does not work, but  
`date now | str from | save --raw timestamp.txt` does.

## Per Directory Environment Variables

Nushell supports defining actions to happen when changing to given directories.
To configure this, create a `.nu-env` file in each directory.
These are TOML files with the following sections:

| TOML Section   | Description                                                                                                         |
| -------------- | ------------------------------------------------------------------------------------------------------------------- |
| `[env]`        | sets environment variables to literal values;<br>lines have the syntax `name = "value"`                             |
| `[scriptvars]` | sets environment variables to command output;<br>lines have the syntax `name = "command-pipeline"`                  |
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

In the `~/projects/bank-accounts` directory, create the file `.nu-env`
containing the following and then enter `autoenv trust`:

```toml
[env]
NODE_ENV = "production"
```

Now `cd` to each of these directories and verify that
the `NODE_ENV` environment variable is set to the expected value.

Currently the `echo` command does not write to stdout when run from
`entryscripts` or `exitscripts`.

For more details on this feature, enter `help autoenv`.

## Additional Shells

New shells can be created from the current shell.
This enables retaining the working directory of the current shell and
switching back to it using the `n` (for next) and `p` (for previous) commands.

Here is a summary of the commands related to working with multiple shells.

| Command           | Description                                                                                      |
| ----------------- | ------------------------------------------------------------------------------------------------ |
| `enter dir-path`  | similar to `cd`, but creates a new shell starting in the given directory                         |
| `enter file-path` | creates a new shell whose context is the content of the given file;<br>seems like an odd feature |
| `shells`          | lists the existing shells and indicates which one is active                                      |
| `n`               | makes the next shell in the list active;<br>wrapping to the first if in the last                 |
| `p`               | makes the previous shell in the list active;<br>wrapping to the last if in the first             |
| `exit`            | exits the current shell, removing it from the list                                               |
| `exit --now`      | exits all the shells;<br>depending on configuration the terminal window may close                |

## Issues

- Fuzzy completion is not yet supported.
  For example, entering `cd foo` and pressing the tab key
  doesn’t auto complete to a directory that contains `foo`.
- The literal syntax for tables is currently very picky
  about the location of newline characters due to a parser bug.
  See {% aTargetBlank "https://github.com/nushell/nushell/issues/3204",
  "issue 3204" %}
- There is no built-in `grep` command.
  Consider using {% aTargetBlank "https://github.com/BurntSushi/ripgrep",
  "ripgrep" %}.
