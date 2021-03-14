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
It is implemented in Rust.

Nushell runs in Linux, macOS, and Windows.

Color coding of commands is applied while they are typed.
When the command is invalid, all the text is red.

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
In version 0.28.0 there are 105 commands.
For help on a specific command,
enter `help {command-name}` or {command-name} -h`.

For detailed documentation, see the
{% aTargetBlank "https://www.nushell.sh/book/", "Book" %}
link in the top nav of the website.

Nushell has great command recall and completion like the Fish and zsh shells.

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
To output the value of each setting in the config file, enter `config`.

Another option for customizing the prompt is to enable the use of
{% aTargetBlank "https://starship.rs", "starship" %}
by setting the `use_starship` setting to `true`.

TODO: Why doesn't `config load $(config path)` work?
TODO: You asked in the Nushell discussion page.

### startup

The `startup` setting specifies a list of commands
to run each time a new Nushell session is started.
Typically the commands define aliases and custom commands.
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

## Environment Variables

To set the value of an environment variable,
enter `let-env NAME = value`.
To get the value of an environment variable, use `$nu.env.NAME`
which can be passed to the `echo` command to print the value.

To see a nicely formatted list of environment variables,
enter `echo $nu.env | pivot` or `config | get env | pivot`.

To get the value of a single environment variable,
enter `config | get env.{name}`.

## Data Types

Unlike most shells where only strings are used for command input and output,
Nushell supports many primitive and structured data types.

| Type         | Description                                                                                    |
| ------------ | ---------------------------------------------------------------------------------------------- |
| `boolean`    | literal values are `$true` and `$false`                                                        |
| `integer`    | whole numbers                                                                                  |
| `decimal`    | numbers with a fractional part                                                                 |
| `range`      | `{start}..{end}` (inclusive) or `{start}..<{end}` (end is exclusive)                           |
| `string`     | single words need no delimiter; multiple words need single quotes, double quotes, or backticks |
| `line`       | a string with an OS-dependent line ending                                                      |
| glob pattern | can include `*` wildcard and `**` for traversing directories                                   |
| `date`       | timezone-aware; defaults to UTC                                                                |
| `duration`   | number followed by a unit which can be `sec`, `min`, `hr`, `day`, `wk`, `mon`, or `yr`         |
| `file size`  |                                                                                                |
| column path  | dot-separated list of nested column names                                                      |
| file path    | platform-independent path to a file or directory                                               |
| file size    | number followed by a unit which can be `b`, `kb`, `mb`, `gb`, `tb`, or `pb`                    |
| `binary`     | sequence of raw bytes                                                                          |
| `list`       | sequence of values of any type                                                                 |
| `row`        | list where each value represents a column with an associated name                              |
| `table`      | list of rows; returned by many Nushell commands                                                |
| `block`      | block of nu script code that can be executed on each row of a table                            |
| `group`      | semicolon-separated list of pipelines that can be run in parallel?                             |

Details about these data types can be found at {% aTargetBlank
"https://www.nushell.sh/book/types_of_data.html", "Types of data" %}.

Strings delimited by backticks support templating
with expressions in pairs of double curly brackets.
For example:

{% raw %}

```bash
let x = 19; echo `x is {{$x}}`
```

{% endraw %}

If the start value of a range is omitted, it defaults to zero.
If the end value of a range is omitted, the range has no upper bound.

Tables have a literal syntax that allows them to be created manually.
all the data is surrounded in square brackets.
The values in each row are also surrounded by square brackets.
The row of column headings comes first and is followed by a semicolon.
The remaining rows are separated by a space.
For example, `echo [[Name, Score]; [Mark, 19] [Tami, 21]]`
outputs the following table:

```text
───┬──────┬───────
 # │ Name │ Score
───┼──────┼───────
 0 │ Mark │    19
 1 │ Tami │    21
───┴──────┴───────
```

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

Commands not defined in Nushell are processed
using the parent shell implementation.
For commands defined in Nushell, the parent shell implementation
can be invoked by prefixing the command name with `^`.
For example, `^ls *.html`.

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

The parameters in the examples above are positional.
Named parameters (a.k.a. flags) can also be specified
by adding `--` before their names.
These are long-form names.
To also specify short-form names, follow the long name with `(-short-name)`.
For example:

{% raw %}

```bash
def color-echo [
  text: string,
  --color (-c): string
] {
  #TODO: What is the best way to test whether a flag is present?
  #if $color { echo 'present' } { echo 'missing' }
  let len = $(echo `{{$color}}` | str length)
  if $len == 0 { echo $text} { echo [$(ansi $color), $text, $(ansi reset), $(char newline)] | str collect }
}
```

{% endraw %}

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

## Variables

To set a variable, enter `let name = value`.
Note that these are distinct from environment variables.
Their scope is the context or block in which they are defined.

To set a variable to the result of a command,
enter `let name = $(command)`.

To set a variable to the result of concatenating two string variables,
enter `let v3 = echo [$v1 $v2] | str collect`.

## Aliases

To create an alias for a command, enter `alias {name} = {value}`.
For example, `alias cls = clear`.

To make aliases available in each new Nushell session,
add them to the `startup` list in the config file
as shown in the "Configuration" section.

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
Todo data from this site can be rendered as a table with the following:

```bash
fetch https://jsonplaceholder.typicode.com/todos | where userId == 2 && completed == $true | sort-by title
```

## Tables

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
| `get {column}`             | gets the content of a given column as a table                    |
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
| `pivot`                    | swaps the rows and columns                                       |
| `sort-by`                  | sorts the rows on a given column                                 |
| `reject`                   | removes given columns                                            |
| `rename`                   | renames columns                                                  |
| `reverse`                  | reverses the order of the rows                                   |
| `roll n`                   | rolls the bottom n rows to the top (n defaults to 1)             |
| `rotate`                   | rotates the table 90 degrees clockwise; can apply multiple times |
| `rotate counter-clockwise` | rotates the table 90 degrees counter-clockwise                   |
| `select`                   | specifies columns to be retained and their order                 |
| `shuffle`                  | shuffles the rows randomly                                       |
| `skip n`                   | skips the first n rows (n defaults to 1)                         |
| `split-by`                 | ?                                                                |
| `to {format}`              | converts a table to a given format such as json                  |
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

For examples of Nushell scripts, see {% aTargetBlank
"https://github.com/nushell/nu_scripts/tree/main/nu_101", "Nu_101 Scripts" %}.

Commands commonly used in Nushell scripts include `if`, `each`, ...

To iterate over a range of integers, use
`seq start end | each { ... }`.

## Questions

TODO: Is it possible to change the nu shell prompt?

The `$it` variable holds the output of the previous command
so it can be used in a block.
TODO: Show examples of using this.

See the `str` subcommands.
