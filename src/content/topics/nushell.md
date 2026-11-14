---
navigation:
  key: Nushell
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Overview

[Nushell](https://www.nushell.sh) is "a new type of shell".
It brings the Unix philosophy of connecting small commands with pipes
to structured data such as lists, records, and tables.
Nushell runs on Linux, macOS, and Windows.

This article was updated for
[Nushell 0.115.1](https://github.com/nushell/nushell/releases/tag/0.115.1)
on September 26, 2026.
Nushell is still evolving, so check the release notes when upgrading.

Nushell is implemented in Rust.
And like Rust, Nushell produces helpful, nicely formatted error messages.

Nushell includes the Nu language which excels at
processing streams of structured data.
This provides features that are somewhat like SQL for databases.
It also has similarities to the Python-based
[Pandas](https://pandas.pydata.org) data analysis library.
These are features not typically seen in shell environments.

Nushell continues the UNIX tradition of
commands whose input can be piped in from a previous command
and whose output can be piped to a subsequent command.
A series of commands separated by the pipe character `|`
is referred to as a "pipeline".
Many commands stream their output, allowing the next command to start
before the previous command has finished producing all its data.
Other commands, such as sorting, need to collect their input first.

Nushell distinguishes returning a value from printing it.
`echo` returns its arguments as data
whereas `print` writes to the terminal.
In a script or custom command, only the final expression's value is returned.
For example, `let a = 2; let b = 3; $a + $b` returns `5`.
Use `print` for messages that should appear
even when another expression follows.

Commands are syntax-highlighted as they are typed,
and the editor provides completion and command history.

You don't have to commit to making Nushell your default shell.
Just enter `nu` periodically to try it,
and `exit` to return to your current shell.
Over time you may decide you like it enough to make it your default shell.

## Terminology

The term "pipeline" was described earlier.
A pipeline can span multiple lines by ending lines with `|`.
Parentheses can also enclose a multiline pipeline.
The interactive editor supports multiline input.
An unfinished expression such as an unclosed parenthesis
can be continued on the next line.
Nushell does not use `\` as a line continuation character like Bash.

Commands that take no input and produce output
are sometimes referred to as "inputs" or "sources".
An example is the `open` command which loads a file.

Commands that take input and transform it are referred to as "filters".
Examples include the `where` and `sort-by` commands.

Commands that take input and display it or write it somewhere such as a file
are referred to as "outputs" or "sinks".
An example is the `save` command.

## Installing

There are many options for installing Nushell.

If you have Rust and the required build tools installed,
enter `cargo install nu --locked`.
This compiles Nushell, which can take several minutes.
Cargo normally installs the `nu` executable in `$HOME/.cargo/bin`;
make sure that directory is on your `PATH`.
Plugins are installed separately.

If you are on macOS and have Homebrew installed, enter `brew install nushell`.
Prebuilt binaries for Linux, macOS, and Windows can also be downloaded
from the [release page](https://github.com/nushell/nushell/releases).

For more information, see
[Installing Nu](https://www.nushell.sh/book/installation.html).

If you are interested in peeking at the source code, clone the
[GitHub repository](https://github.com/nushell/nushell).

To build Nushell locally, enter `cargo build --release` from that repository.
To build the entire workspace, including its plugin packages,
use `cargo build --release --workspace`.

## Upgrading

Use the same installation method to upgrade:

- For Homebrew, enter `brew update` and then `brew upgrade nushell`.
- For Cargo, enter `cargo install nu --locked`.
  If a newer Rust toolchain is required, update it with `rustup update` first.
- For downloaded binaries,
  replace them with the binaries for the desired release.

Enter `nu --version` or `nu -v` to check the installed version.
Start a new Nushell session to use the upgraded executable.
Update separately installed plugins to compatible versions as well.

## Getting Started

After installing Nushell, enter `nu` in a terminal to start a shell.

For help, enter `help`.
For a list of available commands, enter `help commands`.
The list depends on your Nushell build and the plugins, modules,
and custom commands that are loaded.

For help on a specific command,
enter `help {command-name}` or `{command-name} --help`.
In Nushell version 0.115.1 there are 567 commands.
For more detail, see the [Command Reference](https://www.nushell.sh/commands/).

To search for commands, use `help --find`, for example `help --find string`.
You can also process help as structured data:

```nu
help commands | select name description | where name starts-with 'str '
```

Like in other shells, commands are executed by typing them
and pressing the return key.
Nushell has command recall and completion like the Fish and Zsh shells.
Command recall also supports multiline command editing.

When a command not defined by Nushell is encountered,
the directories in `$env.PATH` are searched for a matching executable.
To run an executable that has the same name as a Nushell command,
prefix the command name with `^`.
For example, `^find . -name '*.ts'` runs the external `find` executable.
It does not invoke a command or alias from a parent shell.

For detailed documentation, see the [Book](https://www.nushell.sh/book/).
For more help, join the [Nushell Discord channel](https://discord.gg/NtAbbGn).

## Subcommands

Subcommands provide a way of grouping commands under a common "namespace".
Examples include `date`, `from`, `http`, `into`, `math`, `path`,
`random`, `str`, `to`, and `url`.
Use `help {namespace}` to see the available subcommands.

The `date` subcommands include `from-human`, `humanize`, `list-timezone`,
`now`, and `to-timezone`.
To format a date, use `format date`.
To examine its components, use `date now | into record`.

The `from` subcommands parse data in formats such as CSV, JSON, TOML, and YAML.
The result may be a table, record, list, or scalar, depending on the data.
For example, `'{"foo": 1, "bar": 2}' | from json | get bar` returns `2`.
Additional parsers can be provided by plugins.

The `into` subcommands convert values to another type.
Examples include `binary`, `bool`, `datetime`, `duration`,
`filesize`, `float`, `int`, and `string`.
For example, `'19' | into int` gives the integer `19`.

The `math` subcommands perform calculations.
They include `abs`, `avg`, `cbrt`, `ceil`, `cos`, `exp`, `floor`, `ln`, `log`,
`max`, `median`, `min`, `mode`, `product`, `round`, `sin`, `sqrt`, `stddev`,
`sum`, `tan`, `variance`, and more.
Use ordinary expressions for arithmetic, for example `2 + 3 * 4`.

The `path` subcommands operate on file paths.
The table below describes these and shows the result of entering
`echo ~/Documents/demo.txt | path {subcommand}`.

| Subcommand    | Description                                                                                               |
| ------------- | --------------------------------------------------------------------------------------------------------- |
| `basename`    | returns the filename portion of a path (`demo.txt`)                                                       |
| `dirname`     | returns the parent directory path (`~/Documents`)                                                         |
| `exists`      | tests whether a path exists                                                                               |
| `expand`      | expands a path to an absolute path (`/Users/mark/Documents/demo.txt`)                                     |
| `join`        | joins path components; for example, `'Documents' \| path join demo.txt`<br>returns `~/Documents/demo.txt` |
| `parse`       | returns a record including `parent` (directory path), `stem` (filename w/o extension), and `extension`    |
| `relative-to` | returns a path relative to another path                                                                   |
| `split`       | returns a list of path components                                                                         |
| `type`        | returns a type such as `file`, `dir`, or `symlink`, or an empty string for a nonexistent path             |

The `str` subcommands perform string operations.
They are
`camel-case`, `capitalize`, `contains`, `distance`, `downcase`, `ends-with`,
`escape-regex`, `expand`, `index-of`, `join`, `kebab-case`, `length`,
`lowercase`, `pascal-case`, `replace`, `reverse`, `screaming-snake-case`,
`snake-case`, `starts-with`, `substring`, `title-case`, `trim`,
`upcase`, and `uppercase`.
Also see the command `char` and the `format` subcommands.

The `to` subcommands serialize data into formats such as CSV, JSON, and YAML.
They include `csv`, `html`, `json`, `md`, `text` (or `txt`),
`toml`, `tsv`, `xml`, and `yaml` (or `yml`).
These are commonly piped to `save` to write the result to a file.
The appropriate input shape depends on the format.
For example, CSV represents rows and columns, while JSON can represent nested data.

The `url` subcommands include `build-query`, `decode`, `encode`, `join`,
`parse`, and `split-query`.
For example:

```nu
'https://example.com/blog?tag=nu' | url parse | get host # example.com
```

## Configuration

Nushell configuration uses Nu scripts rather than TOML.
`config.nu` is the main interactive configuration file.
Its path is available as `$nu.config-path`.
`env.nu` can set environment variables and runs before `config.nu`.
Its path is available as `$nu.env-path`.
Login shells can also read `login.nu`.

To edit the main configuration file with Vim, enter `vim $nu.config-path`.
To edit it with VS Code, enter `code $nu.config-path`.
You can also set `$env.config.buffer_editor = 'code'` and use `config nu`.
Use `config env` to edit `env.nu`.

The current settings are in the `$env.config` record.
Enter `$env.config` to inspect them, or `config nu --doc`
to read documented configuration examples.
Assign a field to change a setting immediately:

```nu
$env.config.show_banner = false
$env.config.table.mode = 'rounded'
```

To persist a setting, add the assignment to `config.nu`.
Editing the file does not automatically change a running shell.
Start a new session, or use `source $nu.config-path` to execute the file again.
Sourcing repeats all its commands,
so additions to lists such as `PATH` will be repeated.

After modifying the `config.nu` file,
to start a new session that uses the changes, enter `exec nu`.
To reapply the config within the existing session,
enter `source $nu.config-path`.

To define abbreviations, which are like aliases in other shells,
add the following in the config file:

```nu
$env.config.abbreviations = {
  abbrev1: 'command 1'
  abbrev2: 'command 2'
  ...
}
```

For details, see [Configuration](https://www.nushell.sh/book/configuration.html).

### Prompt

Prompt components are configured through environment variables.
They can hold strings or closures that compute strings.
For example, add the following to `config.nu`:

```nu
$env.PROMPT_COMMAND = {|| $"🦀ν (ansi yellow)(pwd)(ansi reset)" }
$env.PROMPT_INDICATOR = $"(ansi green)▶ (ansi reset)"
$env.PROMPT_COMMAND_RIGHT = ''
```

🦀 is for the Rust mascot Ferris and ν is the Greek letter nu.
The closure is evaluated again for each prompt,
so the working directory stays current.

Another option is [Starship](/blog/starship),
which has many more options and works with multiple shells.
See Nushell's [third-party prompt setup](https://www.nushell.sh/book/3rdpartyprompts.html)
for integration instructions.

### Startup Commands

Define aliases, custom commands, and imports directly in `config.nu`.
For example:

```nu
alias cls = clear

# Changes to the directory stored in JS_DIR.
def --env cdjs [] { cd $env.JS_DIR }

# Changes to the directory stored in RUST_DIR.
def --env cdrust [] { cd $env.RUST_DIR }
```

The `--env` flag allows these custom commands to change
the caller's environment, including the working directory.
Set `JS_DIR` and `RUST_DIR` to your project directories before using them.
Aliases can also use `cd`, for example `alias cdh = cd ~`.

### Table Settings

Table borders can be customized with `$env.config.table.mode`.
Run `table --list` to see the supported themes.
Supported `table.mode` values include:

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

For example:

```nu
$env.config.table.mode = 'rounded'
$env.config.table.index_mode = 'never'
```

The second assignment hides the index column globally.
For one pipeline, use `ls | table --index false`.
To override the theme for one pipeline, use `ls | table --theme compact`.
These options affect rendering, not the underlying data.

### Colors

Customize colors through `$env.config.color_config`.
Keys include `header`, `row_index`, `separator`,
`bool`, `int`, `string`, and many more.
Values can be color names or style records.
For example:

```nu
$env.config.color_config.header = { fg: 'red', attr: 'b' }
$env.config.color_config.row_index = 'dark_gray'
$env.config.color_config.separator = 'dark_gray'
$env.config.color_config.int = 'green'
$env.config.color_config.string = 'white'
```

Run `ansi --list` to see available ANSI styles.
For more options, see
[Coloring and Theming](https://www.nushell.sh/book/coloring_and_theming.html).

### Line Editor

Nushell uses [Reedline](https://www.nushell.sh/book/line_editor.html)
for line editing, history, and completion.

Press the tab key for completions.
Use the arrow keys to move to an option
and press the return key to select the highlighted option.

For example:

```nu
$env.config.edit_mode = 'vi'
$env.config.history.ignore_space_prefixed = true
$env.config.completions.algorithm = 'fuzzy'
```

For Emacs-style editing, use `emacs` instead of `vi`.
The history setting above excludes commands
starting with a space from saved history.
The completion algorithm can be `prefix` or `fuzzy`.

### PATH and Environment

`$env.PATH` contains the directories searched for executables as a list.
To display them in a table, enter `$env.PATH`.
To prepend a directory:

```nu
$env.PATH = ($env.PATH | prepend ($env.HOME | path join '.cargo' 'bin'))
```

Nushell inherits environment variables from its parent process,
which need not be another shell.
To define an environment variable, use an assignment such as:

```nu
$env.GITHUB_USER = 'mvolkmann'
```

Put assignments in your configuration file
to make them available in new sessions.
When configuring Nushell as a login shell, also review the environment setup
previously performed by your old shell's startup files.

## Data Types

Unlike shells where command input and output are primarily text,
Nushell supports primitive and structured data types.
The following table lists commonly used types and annotations.

| Type        | Description or example                                        |
| ----------- | ------------------------------------------------------------- |
| `any`       | accepts any type; useful for untyped command parameters       |
| `binary`    | raw bytes, for example `0x[01 FF]`                            |
| `bool`      | `true` or `false`                                             |
| `cell-path` | a path into structured data, for example `$.address.city`     |
| `closure`   | executable code with parameters, for example `{\|n\| $n * 2}` |
| `datetime`  | a date and time, for example `2026-09-26T12:00:00-05:00`      |
| `duration`  | a time interval, for example `2hr + 30min`                    |
| `filesize`  | a byte count, for example `10MB` or `10MiB`                   |
| `float`     | a 64-bit floating-point number, for example `3.14`            |
| `glob`      | a glob pattern value; convert a string with `into glob`       |
| `int`       | a signed 64-bit integer                                       |
| `list`      | an ordered collection, for example `[red green blue]`         |
| `nothing`   | the type of `null`                                            |
| `number`    | a parameter annotation accepting either `int` or `float`      |
| `range`     | a numeric sequence, for example `1..5` or `1..<5`             |
| `record`    | named fields, for example `{name: Mark, score: 19}`           |
| `string`    | text                                                          |
| `table`     | a list of records, typically with matching columns            |

A filepath is normally represented as a string.
Parameter types such as `path` help Nushell parse command arguments.

Integers do not have unlimited precision.
Floating-point values are approximate, so arithmetic can have rounding effects.
Duration units include `ns`, `us`, `ms`, `sec`, `min`, `hr`, `day`, and `wk`.
Month and year durations are not supported because their lengths vary.

The ability to specify single-word strings without delimiters
in lists and command arguments is one of my favorite features!
For example:

```nu
let colors = [red orange yellow green blue purple]
```

Bare words are context-dependent: at the start of a pipeline,
a word is generally interpreted as a command name.
Quote strings when an expression could otherwise be ambiguous.

Pipe a value to `describe` to inspect its type:

```nu
42 | describe # int
3.14 | describe # float
date now | describe # datetime
```

For full details, see [Types of Data](https://www.nushell.sh/book/types_of_data.html).

### Type Conversions

Use `into` subcommands for conversions, `from` subcommands to parse formats,
and `to` subcommands to serialize data.

| Input                            | Desired result         | Command                                 |
| -------------------------------- | ---------------------- | --------------------------------------- |
| integer, float, boolean, or date | string representation  | `into string`                           |
| list of strings                  | a single joined string | `str join`, optionally with a separator |
| numeric string                   | integer                | `into int`                              |
| numeric string                   | floating-point number  | `into float`                            |
| boolean string                   | boolean                | `into bool`                             |
| date string                      | date/time              | `into datetime`                         |
| string with a separator          | list                   | `split row separator`                   |
| JSON string                      | structured value       | `from json`                             |
| structured value                 | JSON string            | `to json`                               |
| date/time                        | component record       | `into record`                           |

`describe` returns a type description, not a string representation of the value.

For example:

```nu
'2021-03-21T14:30:00+00:00' | into datetime | format date '%B %-d, %Y'
# March 21, 2021
```

Use `format date --list` to see the supported formatting specifiers.

This handy custom command joins input values with newlines.
Consider adding it to `config.nu`:

```nu
# Converts input values to strings and joins them with newlines.
def as-lines [] {
  $in | each {|item| $item | into string } | str join (char newline)
}

1..3 | as-lines
# 1
# 2
# 3
```

### Strings

Strings can use single quotes, double quotes, or backticks.
Double-quoted strings interpret escape sequences such as `\n` and `\t`.
Single quotes and backticks are useful when those characters should remain literal.

String interpolation uses `$"..."` or `$'...'`, with expressions in parentheses.
For example:

```nu
let x = 19
$"x is ($x)" # x is 19

let a = 3
let b = 5
$"product of ($a) and ($b) is ($a * $b)" # product of 3 and 5 is 15
```

`is-empty` determines whether a string, list, record, or table is empty:

```nu
'Mark' | is-empty # false
'' | is-empty # true
```

The `=~` and `!~` operators test whether a string matches a regular expression.
Use `str contains` to test for a literal substring.

```nu
'foobarbaz' =~ 'b.r' # true
'foobarbaz' !~ '^bar' # true
'foobarbaz' | str contains 'bar' # true
```

The `lines` command splits a string at line endings:

```nu
let s = "This\nhas\nthree lines."
$s | lines # [This has 'three lines.']
```

For more examples, see
[Working with Strings](https://www.nushell.sh/book/working_with_strings.html).

### Ranges

A range describes a numeric sequence.
`2..4` includes 2, 3, and 4, while `2..<4` excludes 4.
`4..2` counts down, and `-2..2` includes -2 through 2.
Floating-point bounds are supported, for example `1.2..4.0`.
An explicit second value sets the step: `2..4..10` produces 2, 4, 6, 8, and 10.

An omitted start defaults to zero, so `..10` is equivalent to `0..10`.
An omitted end makes the range unbounded.
Limit an unbounded range with a command such as `take` before collecting it.

```nu
3..7 | first # 3
3..<7 | last # 6
0.. | take 3 # [0 1 2]
```

### Types With Units

Duration values with different units can be added.
For example, `2hr + 57min + 11sec` (my best marathon time).

File sizes also support units and arithmetic.
`MB` uses powers of 1000; `MiB` uses powers of 1024.
For example, `2MB + 57KB + 11B` is a file size.
When comparing a size from `ls`, use a file-size value such as `size > 1KB`.

Incompatible types cause an error.
For example, adding a duration to a file size with `3hr + 2MB` is invalid.

### Lists

A list is an ordered collection of values.
Create one with expressions in square brackets separated by spaces or commas.
For example, `[foo bar baz]` or `[foo, bar, baz]`.

To transform each item, use `each` with a closure.
The closure declares the variable holding the current item between `|` characters.
To also obtain each item's index, insert `enumerate`:

```nu
let names = [Mark Tami Amanda Jeremy]
$names | each {|name| $"Hello, ($name)!" }
$names | enumerate | each {|entry| $"($entry.index + 1)) ($entry.item)" }
```

The second pipeline produces the strings `1) Mark`, `2) Tami`, and so on.
There is no implicit `$it` variable and no `each --numbered` flag.
You can name a closure parameter `it`, but you must declare it explicitly.

The `split row` command creates a list from a string based on a delimiter:

```nu
let colors = ('red,green,blue' | split row ',')
$colors.1 # green
$colors | length # 3
$colors | is-empty # false
'blue' in $colors # true
'yellow' not-in $colors # true
```

List indexes start at zero.
Use `$name.index` or `get index` to access one item.

The `where` command filters a list:

```nu
let colors = [red orange yellow green blue purple]
$colors | where {|color| $color | str ends-with 'e' }
# [orange blue purple]

let scores = [7 10 8 6 7]
$scores | where {|score| $score > 7 } # [10 8]
```

`any` tests whether at least one item satisfies a condition.
`all` tests whether every item satisfies it.
Both take closures:

```nu
let scores = [7 10 8 6 7]
$scores | any {|score| $score > 7 } # true
$scores | any {|score| $score mod 2 == 1 } # true
$scores | all {|score| $score >= 3 } # true
$scores | all {|score| $score mod 2 == 0 } # false
```

`append` and `prepend` return new lists.
A list argument contributes its elements; wrap it in another list
if you want to append a nested list as one item.

```nu
let colors = [yellow green]
let colors = ($colors | prepend red | append purple)
$colors # [red yellow green purple]
```

`flatten` removes a level of nesting:

```nu
[1 [2 3] 4 [5 6]] | flatten # [1 2 3 4 5 6]
[[1 2] [3 [4 5 [6 7 8]]]] | flatten | flatten | flatten
# [1 2 3 4 5 6 7 8]
```

`reduce` combines values using a closure with item and accumulator parameters,
in that order. `--fold` supplies the initial accumulator.
Without it, the first item becomes the initial accumulator.

```nu
let scores = [3 8 4]
$scores | reduce {|item, acc| $acc + $item } # 15
$scores | math sum # 15, an easier approach
$scores | reduce --fold 1 {|item, acc| $acc * $item } # 96
$scores | enumerate | reduce --fold 0 {|entry, acc|
  $acc + $entry.index * $entry.item
} # 16
```

### Tables

A table is a list of records.
The compact literal syntax starts with a list of column names,
followed by a semicolon and lists of row values:

```nu
let scores = [[Name Score]; [Mark 19] [Tami 21]]
$scores
```

This renders as a table similar to:

```text
╭───┬──────┬───────╮
│ # │ Name │ Score │
├───┼──────┼───────┤
│ 0 │ Mark │    19 │
│ 1 │ Tami │    21 │
╰───┴──────┴───────╯
```

SQL-like filters retrieve data from a table:

```nu
let scores = [[Name Score]; [Mark 19] [Tami 21]]
$scores | where Name == 'Tami' | get Score # [21]
$scores | where {|row| $row.Name == 'Tami' } | get Score | first # 21
```

Filtering a table still returns a table, even if only one row matches.
Getting a column returns a list. Use `first` or `get 0` when you need one value.

Use a record for a single object:

```nu
let data = {color: yellow, flavor: vanilla}
$data.color # yellow
$data.flavor # vanilla
```

Multiline table literals work normally:

```nu
let sports = [
  [name players];
  [baseball 9]
  [basketball 5]
  [football 11]
  [hockey 6]
]
let sport = 'basketball'
let players = ($sports | where name == $sport | get players | first)
$"The number of active players in ($sport) is ($players)."
```

Records and tables can contain nested structured data.
For example:

```nu
let person = {
  name: 'Mark Volkmann'
  address: {
    street: '123 Some Street'
    city: Somewhere
    state: MO
    zip: '12345'
  }
}
$person.address.city # Somewhere
$person | to json --indent 2
```

This serializes to a JSON object with a nested `address` object.
A table would serialize to an array of objects, including for a one-row table.

## Operators

Nushell supports the following operators, among others:

| Operator                   | Description                          |
| -------------------------- | ------------------------------------ |
| `+`, `-`, `*`, `/`         | arithmetic                           |
| `//`                       | floor division                       |
| `**`                       | exponentiation                       |
| `mod`                      | modulo                               |
| `==`, `!=`                 | equality and inequality              |
| `<`, `<=`, `>`, `>=`       | ordering comparisons                 |
| `=~`, `!~`                 | regex match and non-match            |
| `in`, `not-in`             | membership and non-membership        |
| `and`, `or`, `xor`, `not`  | Boolean operations                   |
| `++`                       | concatenate lists or strings         |
| `starts-with`, `ends-with` | string prefix and suffix comparisons |

Parentheses group expressions and capture the result of command pipelines.
For example, `let a = 2; let b = 3; $a * $b` returns `6`.
There is no leading `=` to enter a separate math mode.

When passing a calculation as a command argument, use parentheses:
`print (2 + 3)`.
In expression positions, such as after `let value =`, they are often unnecessary.

See [Operators](https://www.nushell.sh/book/operators.html) for the complete list.

## Working with Numbers

Use arithmetic to increment or decrement a value.
Immutable `let` bindings can be shadowed, or use `mut` for a mutable variable:

```nu
mut score = 2
$score += 1
$score # 3
```

To update numbers in a table column:

```nu
[[Name Size]; [Mark 33] [Tami 28]] | update Size {|row| $row.Size + 1 }
```

For semantic version numbers, Nushell 0.115.1 has `semver bump`:

```nu
'1.2.3' | semver bump major # 2.0.0
'1.2.3' | semver bump minor # 1.3.0
'1.2.3' | semver bump patch # 1.2.4
```

## Working with URLs

Use `http get` to retrieve data from a URL.
JSON responses are normally decoded into structured values automatically.
Use `--raw` when you want the response without format conversion.

The website [JSONPlaceholder](https://jsonplaceholder.typicode.com)
provides free sample JSON data.
The following example gets three incomplete TODOs for user 5:

```nu
http get https://jsonplaceholder.typicode.com/todos |
  where userId == 5 and completed == false |
  sort-by title | take 3
```

The `http post` command sends an HTTP POST request.
Specify the content type when sending JSON:

For a multiline command, enclose the invocation in parentheses:

```nu
(http post --content-type application/json
  https://jsonplaceholder.typicode.com/todos
  {title: 'get milk', userId: 5})
```

JSONPlaceholder simulates creation without permanently storing the TODO.
Nushell also provides `http put`, `http patch`, `http delete`,
`http head`, and `http options`.
See `help http` for the available subcommands.

## Common UNIX Commands

Many common UNIX commands have Nushell implementations.
Their options and output can differ from the external utilities with the same names.

| Command    | Description                                    |
| ---------- | ---------------------------------------------- |
| `cal`      | displays a calendar                            |
| `cd`       | changes the current working directory          |
| `clear`    | clears the terminal                            |
| `cp`       | copies files or directories                    |
| `date now` | gets the current date and time                 |
| `du`       | gets disk usage information                    |
| `echo`     | returns values as pipeline data                |
| `exit`     | exits Nushell, optionally with a status code   |
| `help`     | displays help                                  |
| `history`  | returns command history                        |
| `kill`     | sends a signal to a process                    |
| `ls`       | lists files and directories as a table         |
| `mkdir`    | creates directories, including missing parents |
| `mv`       | moves files or directories                     |
| `open`     | reads a file, parsing supported formats        |
| `print`    | prints values to the terminal                  |
| `ps`       | returns process information                    |
| `pwd`      | returns the current working directory          |
| `rm`       | removes files or directories                   |
| `source`   | runs a Nu script in the current context        |
| `which`    | locates commands                               |

To change to a subdirectory named "sub", enter `cd sub`.
Interactive Nushell also supports directory shortcuts such as `../`.
The `cp`, `mv`, and `rm` commands support `--interactive` (`-i`).

Commands such as `ls` and `ps` return tables.
The `sys` command has subcommands such as `sys host` and `sys mem`.
Use cell paths or `get` to access nested data.

Filters such as `where`, `sort-by`, and `reverse` modify the output.
`sort-by --reverse` (`-r`) reverses the sort order;
`--ignore-case` (`-i`) makes string sorting case-insensitive.

```nu
# List package.json files in and below the current directory.
ls **/package.json

# List Rust files recursively.
ls **/*.rs

# List files of at least 2 KB, largest first.
ls | where type == file and size >= 2KB | sort-by --reverse size

# List directory names alphabetically.
ls | where type == dir | sort-by name | get name

# Show processes using more than 5% of a CPU, busiest first.
ps | where cpu > 5 | sort-by --reverse cpu

# Show host information.
sys host

# Show temperatures, where supported by the platform.
sys temp

# Show date/time components.
date now | into record
```

The type strings returned by `ls` are lowercase, such as `file` and `dir`.
Directory sizes in a plain `ls` listing are not recursive totals;
use `du` when that is what you need.

To serialize a table, use a `to` subcommand, for example `ls | to json`.
`save` can also choose a serializer based on the output extension:

```nu
ls | where type == file and size > 1KB | sort-by --reverse size |
  to html | save big-files.html

ls | where type == file and size > 1KB | sort-by --reverse size |
  save big-files.csv
```

The output files must not already exist; use `save --force` to replace them.
Automatic serialization also works for other supported extensions such as `.json`.
`save --raw` bypasses extension-based conversion.

To compute the total size of files in Cargo's default binary directory:

```nu
ls ($env.HOME | path join '.cargo' 'bin') |
  where type == file | get size | math sum
```

This assumes Cargo uses its default installation directory.

## Aliases

To create an alias, enter `alias name = command`.
For example, `alias cls = clear`.
The command can be built in, external, or custom, and fixed arguments can be included.
Additional arguments can be supplied when using the alias.

```nu
alias df = format date
date now | df '%B %-d, %Y'
```

Put aliases directly in `config.nu` to make them available in new sessions.
An alias represents a command invocation, not an entire pipeline.
Use a custom command for a pipeline:

```nu
# Lists the ten busiest processes.
def top [] { ps | sort-by --reverse cpu | first 10 }
```

## Custom Commands

Define a custom command with `def name [parameters] { body }`.
Names can contain hyphens.
Use `[]` when there are no parameters.
Parameter values are accessed with `$` before their names.

```nu
# Lists processes using at least the specified CPU percentage.
def topn [pct: number] {
  ps | where cpu >= $pct | sort-by --reverse cpu
}

topn 5
```

A custom command returns the value of its last expression,
or a value explicitly supplied to `return`.
Use `print` for terminal messages; multiple `echo` statements do not
concatenate into the return value.

```nu
# Describes whether a number is big or small.
def evaluate [n: int] {
  let word = if $n > 5 { 'big' } else { 'small' }
  $"The number ($n) is ($word)."
}

evaluate 1 # The number 1 is small.
evaluate 9 # The number 9 is big.
```

Parameter types are optional but improve documentation and error messages.
Parameters are separated by spaces when calling a command:

```nu
# Computes the sum of two numbers.
def sum [
  n1: number # first number
  n2: number # second number
] {
  $n1 + $n2
}

sum 1 2 # 3
```

Pipeline input is available as `$in`.
For example:

```nu
# Doubles a number supplied through the pipeline.
def double []: number -> number { $in * 2 }

3 | double # 6
```

Named parameters (flags) start with `--`.
An optional short name follows in parentheses.
A flag with no value type is a Boolean switch;
a typed flag that is not provided has the value `null` unless a default is set.

```nu
# Prints a message, optionally in a named ANSI color.
def log-color [text: string, --color (-c): string] {
  if $color == null {
    print $text
  } else {
    print $"(ansi $color)($text)(ansi reset)"
  }
}

log-color 'Giraffes are cool!' --color yellow
```

Custom commands can accept closures.
Use `do` to call a closure directly, or pass it to a command such as `each`:

```nu
# Applies a closure to each value in a list.
def map [values: list, code: closure] {
  $values | each $code
}

map [Mark Tami Amanda Jeremy] {|name| $"Hello, ($name)!" }
```

A rest parameter starts with `...` and collects additional positional arguments
into a list. It must be last, but its name does not have to be `rest`.
The type annotation describes each additional argument:

```nu
# Formats the sum of any number of integer arguments.
def labelled-sum [label: string, ...values: int] {
  $"($label) = ($values | math sum)"
}

labelled-sum 'sum of scores' 3 7 19 # sum of scores = 29
```

Help works just as it does for built-in commands: `help labelled-sum`.
Comments immediately before a definition and after parameter declarations
supply documentation.

Define a subcommand by quoting a name containing spaces:

```nu
# Parent command for the rmv examples.
def rmv [] { help rmv }

# Doubles a number.
def 'rmv double' [n: number] { $n * 2 }

# Increments a number.
def 'rmv increment' [n: number] { $n + 1 }

rmv double (rmv increment 3) # 8
```

Put definitions in `config.nu`, or load them from another Nu file with `source`.
Use `def --env` when a command must preserve environment changes in its caller,
for example a command that uses `cd`.
See [Custom Commands](https://www.nushell.sh/book/custom_commands.html).

## Variables

Variables are distinct from environment variables.
`let` creates an immutable binding; `mut` creates a mutable one.
`const` defines a value evaluated at parse time, which can be useful
for imports and other parser keywords.

```nu
let greeting = 'Hello'
mut count = 1
$count += 1
$"($greeting), count is ($count)" # Hello, count is 2
```

Variables are scoped to the block in which they are defined.
An immutable binding can be shadowed with another `let` binding.
To assign a pipeline result, use `let names = (ls | get name)`.
Parentheses also allow pipeline results to be passed as command arguments.

To concatenate values as text, use interpolation:

```nu
let v1 = 'Nu'
let v2 = 'shell'
let v3 = $"($v1)($v2)"
$v3 # Nushell
```

## Environment Variables

Environment variables are stored in the `$env` record.
They are passed to external programs, with values converted as needed
for the operating system's string-based environment.
Nushell's own configuration record is not exported as a normal environment variable.

```nu
$env.GITHUB_USER = 'mvolkmann'
$env.GITHUB_USER
```

An assignment changes the environment in the current scope.
Persist it by adding it to your startup configuration.
Use `hide-env NAME` to remove a variable from the current environment.

To list variables as rows with names and values:

```nu
$env | transpose name value
```

To set a variable for just one external command, use an environment prefix,
for example `NODE_ENV=production node app.js`.

## open Command

The `open` command reads a file or SQLite database
and parses recognized formats into structured data.
No other databases are currently supported.
For example, CSV becomes a table and a JSON object becomes a record.
The available conversions depend on the commands loaded into the shell.

Consider a file named `scores.csv` containing:

```text
Name,Score
Mark,19
Tami,21
Amanda,17
Jeremy,15
```

`open scores.csv` displays the rows as a table.
To extract information from a JSON file:

```nu
open package.json | get scripts | transpose name command
```

This returns one row per script, with the columns `name` and `command`.

Use `table --index false` to hide row indexes for a particular display.
For plain output without table headings, extract and format the values instead,
for example `open scores.csv | get Name | str join (char newline)`.

Plain text is not automatically rendered with `bat` syntax highlighting.
If you want that presentation, install and run `bat` separately.
`open --raw` skips format parsing; for text processing,
`open --raw file.txt | decode utf-8` explicitly decodes the bytes as text.

For example, suppose `languages.txt` contains:

```text
Go|2012|Rob Pike
Java|1995|James Gosling
JavaScript|1995|Brendan Eich
Python|1991|Guido van Rossum
Ruby|1995|Yukihiro Matsumoto
Rust|2010|Graydon Hoare
TypeScript|2012|Anders Hejlsberg
```

Parse the lines into columns, convert the year to an integer, and sort:

```nu
open --raw languages.txt | decode utf-8 | lines |
  split column '|' Language Year Creator |
  into int Year | sort-by Year
```

If a file extension does not match its format,
read the raw data and parse it explicitly:

```nu
open --raw really-json.txt | decode utf-8 | from json
```

Use `http get` instead of `open` to read a URL:

```nu
http get https://jsonplaceholder.typicode.com/todos |
  where userId == 2 and completed == true | sort-by title
```

### SQLite support

Supposes we have a SQLite database in the file `todos.db`
which contains the table `todos`.
Entering `open todos.db` might display the following:

```text
╭─────────────────┬─────────────────────────────────────────╮
│                 │ ╭─#─┬─id──┬──description──┬─completed─╮ │
│ todos           │ │ 0 │ 107 │ buy milk      │         1 │ │
│                 │ │ 1 │ 121 │ cut the grass │         0 │ │
│                 │ │ 2 │ 161 │ foo           │         1 │ │
│                 │ │ 3 │ 162 │ bar           │         1 │ │
│                 │ │ 4 │ 163 │ baz           │         1 │ │
│                 │ ╰─#─┴─id──┴──description──┴─completed─╯ │
│                 │ ╭─#─┬─name──┬─seq─╮                     │
│ sqlite_sequence │ │ 0 │ todos │ 163 │                     │
│                 │ ╰─#─┴─name──┴─seq─╯                     │
╰─────────────────┴─────────────────────────────────────────╯
```

open todos.db | query db "select description from todos where completed=1"

```text
╭─#─┬─description─╮
│ 0 │ buy milk    │
│ 1 │ foo         │
│ 2 │ bar         │
│ 3 │ baz         │
╰─#─┴─description─╯
```

## Table Commands

The following are some useful commands for working with tables and lists.
Use `help command-name` for their full signatures and examples.

| Command                      | Description                                                |
| ---------------------------- | ---------------------------------------------------------- |
| `all`                        | tests whether all rows satisfy a closure                   |
| `any`                        | tests whether at least one row satisfies a closure         |
| `append`, `prepend`          | adds values at the end or beginning                        |
| `columns`                    | returns column names                                       |
| `compact`                    | removes null values; can check specified table columns     |
| `drop n`                     | removes the last `n` rows                                  |
| `drop column n`              | removes the last `n` columns                               |
| `each`                       | transforms rows with a closure                             |
| `enumerate`                  | pairs each item with its index                             |
| `every n`                    | returns every `n`th row; `--skip` skips those rows instead |
| `first`, `last`              | returns one item, or a list when a count is supplied       |
| `flatten`                    | removes a level of nesting                                 |
| `format pattern`             | formats rows as strings using named fields                 |
| `get`                        | extracts values by cell path                               |
| `group-by`                   | groups rows by a column or closure                         |
| `headers`                    | promotes the first row's values to column names            |
| `histogram`                  | computes counts, percentages, and frequency bars           |
| `insert`, `update`, `upsert` | adds, changes, or adds-or-changes fields                   |
| `is-empty`                   | tests whether the input is empty                           |
| `length`                     | counts rows or items                                       |
| `merge`                      | combines columns from records or tables                    |
| `move`                       | reorders columns                                           |
| `parse`                      | extracts fields from strings using a pattern               |
| `reject`                     | removes columns or rows                                    |
| `rename`                     | renames columns                                            |
| `reverse`                    | reverses row order                                         |
| `roll up`, `roll down`       | cycles rows using `--by n`                                 |
| `rotate`                     | rotates a table; `--ccw` selects counterclockwise rotation |
| `select`                     | chooses columns or rows                                    |
| `shuffle`                    | randomly reorders rows                                     |
| `skip`                       | skips a number of rows                                     |
| `skip until`, `skip while`   | skips rows based on a condition                            |
| `slice`                      | selects a range of rows                                    |
| `sort-by`                    | sorts by one or more columns                               |
| `split column`               | splits strings into named columns                          |
| `table`                      | renders structured data as text                            |
| `take`                       | takes a number of rows                                     |
| `take until`, `take while`   | takes rows based on a condition                            |
| `transpose`                  | exchanges rows and columns                                 |
| `uniq`                       | removes duplicate rows or items                            |
| `where`                      | filters rows using a condition                             |
| `wrap`                       | puts values into a named column                            |

For example, list the three largest TypeScript files in the current directory:

```nu
ls *.ts | select name size | sort-by --reverse size | first 3
```

Add `| select 1` to return the second selected row as a one-row table.
Use `| get 1` if you want the row as a record instead.
Glob examples assume matching files exist.

### `all` and `any` Commands

These commands return Boolean results rather than filtered tables:

```nu
let scores = [[Name Score]; [Mark 19] [Tami 21]]
$scores | all {|row| $row.Score >= 20 } # false
$scores | any {|row| $row.Score >= 20 } # true
```

### `append` and `prepend` Commands

Append or prepend records or another table:

```nu
let primaryColors = [
  [name red green blue];
  [red 255 0 0]
  [green 0 255 0]
  [blue 0 0 255]
]
let purple = {name: purple, red: 255, green: 0, blue: 255}
let white = {name: white, red: 255, green: 255, blue: 255}
$primaryColors | append $purple | prepend $white
```

This produces five rows. No extra `flatten` workaround is needed.

### `each` Command

`each` transforms each input item with a closure:

```nu
ls *.nu | each {|file| $"($file.name) is ($file.size)." } |
  str join (char newline)
```

Use `for` when the purpose is side effects such as printing,
rather than collecting a list of results:

```nu
for name in [Mark Tami] {
  print $"Hello, ($name)!"
}
```

### `is-empty` Command

```nu
[[Name Score]; [Mark 19] [Tami 21]] | is-empty # false
[] | is-empty # true
[[Name Score]; [Mark 19]] | where Score > 30 | is-empty # true
```

### `flatten` Command

`flatten` can also bring fields from nested records into the outer table:

```nu
[{name: Mark, address: {city: Somewhere, state: MO}}] | flatten
```

The result has columns `name`, `city`, and `state`.

### `format pattern` Command

Format each row into a string by naming its fields inside braces:

```nu
ls | format pattern '{name} is a {size} {type} and was modified {modified}.'
```

For more control over formatting, use `each` with string interpolation.

### `from` Command

`from` subcommands parse serialized data.
For example:

```nu
let data = "Name,Score\nMark,19\nTami,21\n"
$data | from csv
```

For a file with a recognized extension, `open scores.csv` performs this conversion.

### `get` Command

`get` extracts a value using a cell path.
A column from a table is a list; a field from a record can be a scalar.
For example:

```nu
ls *.ts | sort-by --reverse size | first 3 | get name
```

Cell paths can access nested fields and list indexes, for example `$person.address.city`.
Use `columns` to list column names.
To select a column by its position, first look up its name:

```nu
let data = [[name score]; [Mark 19] [Tami 21]]
let columnIndex = 1
$data | select ($data | columns | get $columnIndex)
```

### `group-by` Command

Group rows by a column or a closure.
By default, `group-by` produces a record whose keys are group names
and whose values are tables of matching rows:

```nu
ls | group-by type
```

The keys might include `file` and `dir`.
A key is absent if no rows belong to that group.
Use an optional cell path to handle that case:

```nu
let groups = (ls | group-by type)
$groups.file? | default []
```

To group by filename extension:

```nu
ls | group-by {|file| $file.name | path parse | get extension }
```

### `histogram` Command

`histogram` counts occurrences and displays relative frequencies:

```nu
let data = [
  [name color];
  [Mark yellow]
  [Tami blue]
  [Amanda green]
  [Jeremy yellow]
  [Sally blue]
  [Sam yellow]
]
$data | get color | histogram | sort-by value | select value count percentage
```

The counts are blue: 2, green: 1, and yellow: 3.
Their percentages are 33.33%, 16.67%, and 50.00% of the six observations.
The complete output also includes `quantile` and a text `frequency` bar.

Use `histogram --percentage-type relative` to calculate percentages
relative to the largest count instead of the total.

### `split` and `headers` Commands

Use `split row`, `split column`, and `headers` to parse delimited text:

```nu
let data = 'name red green blue|red 255 0 0|green 0 255 0|blue 0 0 255'
$data | split row '|' | split column ' ' | headers |
  into int red green blue
```

The conversion makes the numeric columns integers instead of strings.

### Matching Rows

Use `where` with `=~` to filter rows with a regular expression:

```nu
ls | where type == file | where name =~ '^c.*\.nu$'
```

This lists files whose names begin with `c` and end with `.nu`.
Single quotes preserve the regex backslash.

The `match` keyword is for pattern-matching control flow, not table filtering:

```nu
let status = 'done'
match $status {
  'done' => 'Finished'
  'pending' => 'Waiting'
  _ => 'Unknown'
}
```

### `merge` Command

`merge` combines record fields or combines table rows by position:

```nu
let scores = [[name score]; [Mark 19] [Tami 21]]
let colors = [[color flavor]; [yellow vanilla] [blue chocolate]]
$scores | merge $colors
```

The result has `name`, `score`, `color`, and `flavor` columns.
Mark gets yellow/vanilla and Tami gets blue/chocolate.
Use `join` when matching rows by a key instead of their positions.

### `move` Command

Move columns before or after another column:

```nu
ls | move size type --after modified
```

This places `size` and then `type` after `modified`.

### `parse` Command

`parse` extracts fields from text, producing a table of matches:

```nu
'123 Some St., St. Charles, MO 63304' |
  parse '{street}, {city}, {state} {zip}'
```

This returns one row with `street`, `city`, `state`, and `zip` columns.
An input with no match produces no rows.

### `transpose` Command

`transpose` exchanges rows and columns.
It is especially useful for converting a record into name/value rows:

```nu
{name: Mark, score: 19} | transpose field value
```

The result has two rows: `name`/`Mark` and `score`/`19`.
For a table, each original row becomes a column.

### `split column` Command

Split a string into columns, optionally specifying column names:

```nu
'red,green,blue' | split column ',' first second third
```

This produces one row with the columns `first`, `second`, and `third`.

### `to` Command

Use `to` subcommands to serialize structured data.
For example:

```nu
let scores = [[Name Score]; [Mark 19] [Tami 21]]
$scores | to json --raw
# [{"Name":"Mark","Score":19},{"Name":"Tami","Score":21}]
```

`to json` is pretty-printed by default; `--raw` produces compact output.
Other formats include `to csv`, `to html`, `to md`, `to toml`, `to xml`, and `to yaml`.
Each format requires an appropriate input shape.
For example, a record is suitable for a TOML document,
while XML conversion uses a specific tag/attribute/content structure.

## Plugins

Plugins add commands to Nushell.
They are separate executables and must be compatible with your Nu version.
Installing Nu with Cargo does not install the plugins automatically.

For example, to install the query plugin from Cargo
(use a compatible `--version` if your Nu installation is older):

```sh
cargo install nu_plugin_query --locked
```

Then, in Nushell, register the executable:

```nu
plugin add ~/.cargo/bin/nu_plugin_query
```

Adjust the path for your installation; on Windows the executable has an `.exe` suffix.
After registration, load its commands in a separate command:

```nu
plugin use query
```

Registration persists in Nushell's plugin registry.
`plugin use` makes a registered plugin's commands available in the current scope.
You can put `plugin use query` in `config.nu` after registering it
so it is available in future interactive sessions.
Use `plugin list` to inspect registered plugins.

Plugin discovery can use the `NU_PLUGIN_DIRS` constant.
It does not use the old TOML `plugin_dirs` setting.
See [Plugins](https://www.nushell.sh/book/plugins.html) for installation details
and links to available plugins.

The `start` command is built in; no `nu_plugin_start` is needed.
For example, `start demo.html` opens a file using its associated application.
The old `nu_plugin_chart` examples are not part of the current standard plugin set.
For frequency bars, use the built-in `histogram` command described above.

## Default Shell

To make Nushell your default shell on Linux or macOS,
first locate its executable, for example with `which nu`.
Add the executable's absolute path to `/etc/shells` using an editor with
administrator privileges, for example `sudo vim /etc/shells`.
Then run `chsh -s` with that same absolute path.
For example, if Homebrew installed Nu there:

```sh
chsh -s /opt/homebrew/bin/nu
```

Use your actual path; Cargo installations commonly use `$HOME/.cargo/bin/nu`.
If using tmux, also review `default-shell` in `~/.tmux.conf`.

Nu is not a POSIX shell and cannot source Bash or Zsh startup files.
Translate any environment setup you still need into Nu syntax.
See [Default Shell](https://www.nushell.sh/book/default_shell.html).

## Scripts

Nushell scripts use the `.nu` extension.
Execute one with `nu script.nu`.
To run it in the current scope and make its definitions available,
use `source script.nu`.
Comments begin with `#`.

Scripts do not automatically load your interactive `config.nu`.
Define or import their dependencies explicitly.
A `main` command can define the script's command-line parameters:

```nu
# Greets the person named on the command line.
def main [name: string] {
  print $"Hello, ($name)!"
}
```

Save this as `greet.nu` and run `nu greet.nu Mark`.

Conditionals use `if`, `else if`, and `else`:

```nu
let temperature = 80
if $temperature <= 32 {
  'cold'
} else if $temperature >= 80 {
  'hot'
} else {
  'warm'
}
```

Use `each` to transform a sequence or `for` to perform actions for each item.
`seq 1 4` produces integers, and `1..4` is an alternative range expression:

```nu
1..4 | each {|n| $n | into string } | str join (char newline)
```

This returns one string containing the numbers on separate lines.

For a numbered report, combine `enumerate`, `each`, and `str join`:

```nu
# Formats a list as numbered lines.
def report [values: list<string>] {
  $values | enumerate | each {|entry|
    $"($entry.index + 1)) ($entry.item)"
  } | str join (char newline)
}

let names = [Mark Tami Amanda Jeremy]
report $names
# 1) Mark
# 2) Tami
# 3) Amanda
# 4) Jeremy
```

For additional examples, see [Nu Scripts](https://github.com/nushell/nu_scripts).

## VS Code

The [Nushell extension](https://marketplace.visualstudio.com/items?itemName=TheNuProjectContributors.vscode-nushell-lang)
provides support for editing `.nu` files.
Use Nu syntax in configuration files too; they no longer need TOML highlighting.

## Comparison to Bash

The following table shows Nu equivalents of some common Bash operations.

| Bash                   | Nushell                | Description                                        |
| ---------------------- | ---------------------- | -------------------------------------------------- |
| `man command`          | `help command`         | gets help for a Nu command                         |
| `$PATH`                | `$env.PATH`            | directories searched for executables; a list in Nu |
| `cat file.txt`         | `open --raw file.txt`  | reads without extension-based parsing              |
| `command > file.txt`   | `command o> file.txt`  | redirects standard output                          |
| `command >> file.txt`  | `command o>> file.txt` | appends standard output                            |
| `mkdir -p foo/bar/baz` | `mkdir foo/bar/baz`    | creates missing parent directories                 |
| `export NAME=value`    | `$env.NAME = 'value'`  | sets an environment variable                       |
| `cmd1 && cmd2`         | `cmd1; cmd2`           | sequences commands; Nu stops on an unhandled error |

Nu does not use `&&` and `||` as shell command separators.
A nonzero external exit status is an error under the normal error-handling behavior.
Use `try`/`catch` to handle errors or `complete` to inspect an external command's
exit code and captured output explicitly.
A command returning the Boolean value `false` is not itself an error.

For structured data, `save` is often more convenient than redirection:
`ls | save listing.json` serializes the table as JSON.
For a timestamp text file, use `date now | into string | save timestamp.txt`.
See [Coming from Bash](https://www.nushell.sh/book/coming_from_bash.html).

## Per Directory Environment Variables

Interactive Nushell supports hooks that run when environment variables change.
These hooks do not run in noninteractive scripts.
A hook on `$env.PWD` can update settings when you change directories.
The old `.nu-env` files and `autoenv trust` command are no longer the mechanism.

For example, add this to `config.nu` to choose `NODE_ENV`
based on two project directories:

```nu
$env.config.hooks.env_change.PWD = (
  $env.config.hooks.env_change.PWD? | default [] | append {|before, after|
    let development = ($env.HOME | path join 'projects' 'airline-reservations')
    let production = ($env.HOME | path join 'projects' 'bank-accounts')
    if $after == $development {
      $env.NODE_ENV = 'development'
    } else if $after == $production {
      $env.NODE_ENV = 'production'
    } else {
      hide-env --ignore-errors NODE_ENV
    }
  }
)
```

This example takes ownership of `NODE_ENV`: it sets it for exact directory matches
and removes it elsewhere. It does not preserve a previously assigned value
or match project subdirectories. The hook runs on directory changes,
not just because the configuration was loaded.
See [Hooks](https://www.nushell.sh/book/hooks.html) for more details.

For project-specific environment files and restoration when leaving a directory,
see the official [direnv integration recipe](https://www.nushell.sh/cookbook/direnv.html).

## Additional Shells

For switching among remembered directories, use the standard library's directory stack:

```nu
use std/dirs
```

Add the import to `config.nu` to make these commands available in new sessions.

| Command           | Description                                       |
| ----------------- | ------------------------------------------------- |
| `dirs`            | lists the directory stack and active entry        |
| `dirs add path`   | adds a directory and switches to it               |
| `dirs drop`       | removes the current entry and switches to another |
| `dirs next`       | switches to the next entry, wrapping around       |
| `dirs prev`       | switches to the previous entry, wrapping around   |
| `dirs goto index` | switches to the entry at a given index            |

For familiar shell-style names, import the aliases instead:

```nu
use std/dirs shells-aliases *
```

These include `shells`, `enter`, `n`, `p`, `g`, and `dexit`.
They manage a directory stack, not separate processes or file-content shells.
`exit` exits the Nu process; it is not the directory-stack removal command.
See [Directory Stack](https://www.nushell.sh/book/directory_stack.html).

## Searching and Completion

Fuzzy completion is supported:

```nu
$env.config.completions.algorithm = 'fuzzy'
```

For filtering structured data or lines of text, try the built-in `find`,
`where`, `str contains`, and regex operators.
For example, `open --raw notes.txt | decode utf-8 | lines | find Nushell`.
For recursive text searches across files,
[ripgrep](https://github.com/BurntSushi/ripgrep) also works as an external command.
