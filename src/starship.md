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
For example, in the macOS app iTerm2
select Preferences...Profiles...Text and
select "FiraCode Nerd Font" from the Font drop-down.
A new terminal window must be opened for the changes to take effect.

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
`[value](style)` where `value` is literal text and variable values to display
and `style` specifies a color and style.

For example:

```toml
format = "$env_var$git_branch$git_status$directory$character"
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

[env_var]
format = "$env_value"
variable = "SHELL_ICON"
default = "?"

[git_branch]
format = "[$symbol](green)[$branch]($style)"
style = "italic green"
symbol = ""

[git_status]
format = "[$all_status$ahead_behind]($style) "
style = "bold red"
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

For more details on configuration, see {% aTargetBlank
"https://starship.rs/config/#prompt", "Configuration" %}.
