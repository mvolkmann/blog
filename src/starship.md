---
eleventyNavigation:
  key: Starship
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://starship.rs", "Starship" %}
is a highly customizable tool for customizing shell prompts.
It works in nearly all shells including Bash, Fish, Nushell, Powershell, and Zsh.
This removes the need to customize the prompt in shell-specific ways.
It can be configured in one place for all shells,
which is great for users that utilize multiple shells.

Starship is implemented in Rust, so it is very fast.

## Installing

One way to install Starship is to install Rust
and enter `cargo install starship`.
It can also be installed via a `curl` command,
using Scoop in Windows, or using Homebrew in macOS.
For more details on installation, see {% aTargetBlank
"https://starship.rs/guide/#🚀-installation", "Installation" %}.

Starship uses "Nerd Font" characters, so a font must be installed.
Browse {% aTargetBlank "https://www.nerdfonts.com", "nerdfonts.com" %}
and download a font such as "FiraCode Nerd Font".
To install the font in macOS, double-click the file
`Fira Code Regular Nerd Font Complete.ttf`
and press the "Install Font" button.

Terminal apps must then be configured to use the font.

- <b>macOS Terminal app</b>  
  SelectPreferences...Profiles...Text,pressthe"Change..."buttoninthe"Font"sectiontoopenthe"Fonts"dialog,andselect"FiraCodeNerdFont".
- <b>macOS iTerm2 app</b>  
  SelectPreferences...Profiles...Textandselect"FiraCodeNerdFont"fromtheFontdrop-down.Alsoconsidercheckingthe"Useligatures"checkbox.Anewterminalwindowmustbeopenedforthechangestotakeeffect.
- <b>VS Code</b>  
  SelectPreferences...Settings,enter"Editor:FontFamily",andenter"FiraCodeNerdFont".

## Enabling

To enable the use of Starship in each shell:

- for Bash, edit `.bashrc` and add  
  `eval "$(starship init bash)"`

- for Fish, edit `.config/fish/config.fish` and add  
  `starship init fish | source`

- for Nushell, edit `$(config path)` and add  
  `prompt = "starship prompt"`

- for Powershell, edit `Microsoft.PowerShell_profile.ps1` and add  
  `Invoke-Expression (&starship init powershell)`

- for Zsh, edit `.zshrc` and add  
  `eval "$(starship init zsh)"`

- for other shells, see {% aTargetBlank
  "https://starship.rs/guide/#🚀-installation", "Getting Started" %}

## Configuring

To configure Starship, create the file `~/.config/starship.toml`.
Changes to this file take effect immediately.

Many string values in the configuration file can contain the syntax
`[value](style)` where `value` is literal text and variable values to display,
and `style` specifies a color and style.

For example:

```toml
format = "$git_branch$git_status$directory$character"
# Nushell controls the color of commands typed after the prompt.

# Displays text, typically a single character,
# based on the status of the previous command.
[character]
success_symbol = "[▶](bold green)" # normal prompt
error_symbol = "[✗](bold red)" # used if previous command failed
# error_symbol does not work in Nushell

# Displays current directory.
[directory]
format = "[$path]($style)"
style = "yellow"
truncate_to_repo = false
truncation_length = 3 # parent directories to show; default is 3
truncation_symbol = "…/"

# Displays current Git branch when in a directory of a Git repository.
[git_branch]
format = "[$symbol](green)[$branch]($style)"
style = "italic green"
symbol = ""

# Displays status of Git repository when in a directory of a Git repository.
[git_status]
format = "[$all_status$ahead_behind]($style)"
ahead = "⇡ $count "
behind = "⇣ $count "
deleted = "🗑 $count "
diverged = " $count "
stashed = "📦 $count "
modified = "פֿ $count "
staged = '[ $count ](green)'
renamed = " $count "
untracked = "🤷 ‍$count "
style = "bold red"
```

The top `format` setting that defines the prompt
can contain newline characters for a multi-line prompt.
Alternatively it can be delimited by triple quotes (single or double)
to actually specify a multi-line prompt with multiple lines.
For example:

```toml
format = '''
$directory $git_branch$git_status
$custom$character'''
```

The prompt can contain icons.
To see available icons, browse {% aTargetBlank
"https://www.nerdfonts.com/cheat-sheet", "Nerd Fonts Cheatsheet" %}.

The prompt defined above displays an icon at the beginning
to indicate the current shell.
It uses the environment variable `SHELL_ICON` which
must be defined in the configuration script of each shell.
For example:

- For Bash, edit `.bash_profile` and add `export SHELL_ICON=🚀`.
- For Fish, edit `~/.config/fish/config.fish` and add `set -x SHELL_ICON 🐠`.
- For Nushell, edit `$(config path)` and add `SHELL_ICON = "🦀"`
  in the `[env]` section.

## Custom Commands

Custom commands display the output of a given shell command.
To define a custom command, add a `[command.{name}]` section in `starship.toml`.
For example:

```toml
# Indicates when in bash shell.
[custom.bash]
command = "echo 🚀"
when = '[ "$STARSHIP_SHELL" == "bash" ]'

# Indicates when in fish shell.
[custom.fish]
command = "echo 🐠"
when = 'test "$STARSHIP_SHELL" = "fish"'

# Indicates when in Nushell.
[custom.nu]
command = "echo 🦀"
shell = '/usr/local/bin/bash'
when = 'test "$STARSHIP_SHELL" = ""' # not set in Nushell
```

The `command` specified for each of these is only executed
if the `when` command returns `0` indicating success.
The `shell` option specifies the shell to use when executing the `when` command
and defaults to the current shell.
To run all the custom commands in the order they are defined,
add `$command` to the prompt string.
To run a specific custom command, add `${custom.name}` to the prompt string
where `name` is the name of a custom command.

For more details on configuration, see {% aTargetBlank
"https://starship.rs/config/#prompt", "Configuration" %}.
