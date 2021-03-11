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

Unlike most shells where only strings are used for command input and output,
Nushell supports many primitive and structured data types.
Primitive types include boolean, number, decimal,
string, date, file size, and path.
Structured data types include list, table (object), binary data, and block.
Details about these data types can be found at {% aTargetBlank
"https://www.nushell.sh/book/types_of_data.html", "Types of data" %}.

Color coding of commands is applied while they are typed.
When the command is invalid, all the text is red.

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
For help on a specific command, enter `help {command-name}`.

For detailed documentation, see the
{% aTargetBlank "https://www.nushell.sh/book/", "Book" %}
link in the top nav of the website.

Nushell has great command recall and completion like the Fish and zsh shells.

## Configuration

The configuration for Nushell is stored in a TOML file
whose path can be obtained by entering `config path`.
To edit this file with Vim, enter `vim $(config path)`.

For details on configuration options, see {% aTargetBlank
"https://www.nushell.sh/book/configuration.html", "Configuration" %}.

The color used to output data of each type can be customized
by adding a `[color_config]` section to the config file.
For example:

```toml
[color_config]
primitive_filesize = "ub"
primitive_path = "yb"
```

The command `config set path $nu.path` writes the value of `$nu.path`
into the `config.toml` file. TODO: Why is that useful?
Similarly, the command `config set env $nu.env`
writes all the current environment variables into the `config.toml` file.
TODO: Why is that useful?

To specify commands to run each time a new Nushell session is started,
add a `startup` list in the config file where the commands are
inside square brackets, delimited by quotes, and separated by commas.
For example:

```toml
startup = [
  "alias cdjs = cd $nu.env.JS_DIR",
  "alias cdrust = cd $nu.env.RUST_DIR",
  "alias cls = clear"
]
```

TODO: Using the "cd" aliases above currently causes Nushell to crash.
See {% aTargetBlank "https://github.com/nushell/nushell/issues/3138",
"this issue" %}.

To output the value of each key in the config file,
enter `config`.
To output the value of a specific key in the config file,
enter `config | get {key}`.

## Environment Variables

To set the value of an environment variable,
enter `let-env NAME = value`.
To get the value of an environment variable, use `$nu.env.NAME`
which can be passed to the `echo` command to print the value.

## Common Commands

The list of directories in the path of the external shell
are automatically used by the nu shell.

To see a nicely formatted list of directories in your path,
enter `echo $nu.path` or `config | get path`.

To see a nicely formatted list of environment variables,
enter `echo $nu.env | pivot` or `config | get env | pivot`.

To get the value of a single environment variable,
enter `config | get env.{name}`.

To clear the screen, enter `clear`.

The nu shell displays most data in tables.
Commands that produce tables include `ls`, `ps`, and `sys`.
When a column displays `[table n rows]`,
use the `get` filter to display those rows (examples below).
SQL-like filters like `where`, `sort-by`, and `reverse`
can be used to modify the output.
For example:

```bash
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
```

To define a function, enter `def {name} [params] { commands }`.

For example, this command has no parameters.

```bash
def top [] { ps | where cpu > 5 | sort-by cpu | reverse }
```

Here is a version that has a CPU percentage parameter:

```bash
def topn [pct:int] { ps | where cpu > $pct | sort-by cpu | reverse }
```

To drop n columns from the end of a table, add `| drop n`.
Omitting the number drops one column.

To set a variable, enter `let name = value`.

To set a variable to the result of a command,
enter `let name = $(command)`.

To concatenate two string variables,
enter `let v3 = echo [$v1 $v2] | str collect`.

To iterate over a range of integers, use
`seq start end | each { ... }`.

## Aliases

To create an alias for a command, enter `alias {name} = {value}`

To make aliases be available in each new Nushell session,
add them to the `startup` list in the config file
as show in the "Configuration" section.

## VS Code

There is a VS Code extension for Nushell that provides syntax highlighting.
See {% aTargetBlank
"https://marketplace.visualstudio.com/items?itemName=TheNuProjectContributors.vscode-nushell-lang",
"vscode-nushell-lang" %}.

## Questions

TODO: Is it possible to change the nu shell prompt?
