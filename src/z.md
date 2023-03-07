---
eleventyNavigation:
  key: z
layout: topic-layout.njk
---

## Overview

You might be using the terminal all wrong ...
or at least not as efficiently as you could.
Here are ten tips to improve your terminal usage.

## Changing Directory

If your work involves switching between projects during a typical day
then you likely use the `cd` command quite a bit.
A great time saver is to define aliases in your shell configuration file
to quickly move to your most commonly used directories.
For example, if you use the zsh shell then you could add the following in your `~/.zshrc` file

```bash
export DOCUMENTS_DIR=$HOME/Documents

# This directory holds on the files related to my blog
# that is implemented using the Eleventy static site generator.
export BLOG_DIR=$DOCUMENTS_DIR/blog

# This directory contains subdirectories related to software development.
export DEV_DIR=$DOCUMENTS_DIR/dev

# This directory contains subdirectories for projects.
export PROJECTS_DIR=$DOCUMENTS_DIR/projects

# This directory contains subdirectories for various programming languages.
export LANG_DIR=$DEV_DIR/lang

# This directory contains example code using the Swift programming language.
export SWIFT_DIR=$LANG_DIR/swift

alias cdblog="cd $BLOG_DIR"
alias cddev="cd $DEV_DIR"
alias cdjs="cd $JS_DIR"
alias cdprojects="cd $PROJECTS_DIR"
alias cdswift="cd $SWIFT_DIR"
```

## Simplifying Command-line Git

If you sometimes work with Git from the command-line,
defining the following aliases and shell functions can make this easier.
Use these when you are in or below the root directory of a git repository.

```bash
# This lists all the local branches in the current git repository.
alias br="git branch"

# This prompts for a commit message using Vim.
# Diffs for all the modified files are displayed inside Vim
# so they can be verified and serve as the basis for a good commit message.
# After a message is entered and saved (:wq),
# it commits all the modified files.
alias ci="git commit -av"

# This creates a new branch off of the current branch
# with the name specified after `cob`.
# For example, `cob feature-compute-score`.
alias cob="git checkout -b"

# This checks out the branch specified after `co`.
# For example, `co feature-compute-score`.
alias co="git checkout"

# This lists all the commits on the current branch from newest to oldest.
# For each commit, the SHA, author, date, and commit message are output.
alias log="git log"

# This deletes the local AND remote branches with a given name.
# For example, `cob feature-compute-score`.
alias rmb="$HOME/bin/rmb" # defined below

alias status="git status"

# This cd's up to root directory of current git repository.
function cdgitroot() {
  cd `git rev-parse --git-dir`
  cd ..
}

# This pulls the latest changes from the remote branch
# down to the corresponding local branch.
function pull() {
  git pull origin $(git rev-parse --abbrev-ref HEAD)
}

# This pushes the latest changes on the local branch
# up to the corresponding remote branch.
function push() {
  git push origin $(git rev-parse --abbrev-ref HEAD)
}
```

Here is the shell script `rmb` that must be in a directory
listed in the `PATH` environment variable.

```bash
#!/usr/bin/env bash
# Removes a given git branch, both local and remote.

if [ $# -ne 1 ]; then
  echo usage: rmb {branch-name}
  exit 1
fi

git branch -d $1
git branch -rd origin/$1
```

## Avoid Accidents

The `cp` (copy), `mv` (move), and `rm` (remove) commands can result in
loss of data if a file is accidentally replaced or removed.
To avoid this, define the following aliases that
shadow those commands with a version that prompts for permission
before overwriting or deleting a file.

```bash
# Ask for confirmation before overwriting or deleting files.
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"
```

## Kill Process

If you frequently run servers that listen on a specific port,
it's very convenient to have an easy way to kill any process
that is listening on a given port.
I do this with the following alias:

```zsh
alias klp="kill-listening-process"
```

Ths script `kill-listening-process` will differ somewhat
based on the operating system. For macOS it is:

```bash
#!/usr/bin/env bash
# This kills the process listening on a given port.

if [[ $# -ne 1 ]]; then
  echo usage: kill-listening-process {port}
  exit 1
fi

port=$1
pid=$(lsof -n -iTCP:$port -sTCP:LISTEN -t)

if [[ $pid ]]; then
  kill $pid
  echo killed process $pid
else
  echo no process is listening on port $port
fi
```

## Searching For Files

There are multiple ways to search for files in and below the current directory
that have a specific file extension and contain given text.
One way is to use the `find` command as follows:

```bash
find . -name '*.js' | xargs grep 'some text'
```

This has several issues. The syntax is hard to remember, it is somewhat slow,
the output doesn't indicate the line numbers where matches were found,
and the output is not color-coded.

A better alternative is to use {% aTargetBlank
"https://github.com/BurntSushi/ripgrep", "ripgrep" %}.
This page contains installation instructions for Linux, macOS, and Windows.

Ripgrep is implemented in Rust and is very fast.

The equivalent of the `find` command above is `rg --type=js 'some text'

The paths to files that contain the text are displayed in purple.
Matching line numbers are displayed in green.
Text on the matching lines is displayed in white,
except the matching text which is displayed in red.

## Starship

{% aTargetBlank "https://starship.rs/", "Starship" %} is a
highly customizable tool for customizing shell prompts.
It works in nearly all shells including
Bash, Fish, Nushell, Powershell, and Zsh.
This removes the need to customize the prompt in shell-specific ways.

Starship can be configured in one place for all shells,
which is great for users that utilize multiple shells.

To use Starship with zsh, add the following line in `~/.zshrc`:

```bash
eval "$(starship init zsh)"
```

Configure Starship by creating the file `~/.config/starship.toml`.
Here is my configuration. Note the use of unicode characters.

```toml
format = "$shell$custom$git_branch$git_status$directory$character "

# Displays text, typically a single character,
# based on the status of the previous command.
[character]
success_symbol = "[▶](green)" # normal prompt
error_symbol = "[✗](bold red)" # used if previous command failed

# Displays current directory.
[directory]
format = "[$path]($style)"
style = "yellow"
truncate_to_repo = false
truncation_length = 3 # parent directories to show; default is 3
truncation_symbol = "…/"

# Displays current Git branch when in a directory of a Git repository.
[git_branch]
format = "[$symbol ](green)[$branch ]($style)"
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

[shell]
disabled = false
bash_indicator = "🚀"
fish_indicator = "🐠"
nu_indicator = "🦀"
zsh_indicator = "🧙"
```

For more detail, see {% aTargetBlank
"https://mvolkmann.github.io/blog/topics/#/blog/starship/", "Starship" %}.

## Other Shells

{% aTargetBlank "https://mvolkmann.github.io/blog/topics/#/blog/nushell/",
"Nushell" %} is "a new type of shell".
"The goal of this project is to take the Unix philosophy of shells,
where pipes connect simple commands together,
and bring it to the modern style of development."

The output of many commands is a table
and there are many commands for manipulating tables.

TODO: Get more content from https://www.nushell.sh/.

## Vim and Nvim

TODO

## Use Warp

There are many options for terminal applications.
In macOS you can use the built-in Terminal app.
A popular alternative is {% aTargetBlank "https://iterm2.com", "iTerm2" %}.
Many developers using iTerm2 in conjunction with {% aTargetBlank
"https://mvolkmann.github.io/blog/topics/#/blog/tmux/", "tmux" %}
in order to gain features such as multiple tabs that each contain split panes
where each pane hosts a separate shell session.

{% aTargetBlank "https://mvolkmann.github.io/blog/topics/#/blog/warp/",
"Warp" %} is a fairly new option, currently only supported on macOS,
that provides most of the features of iTerm2 and tmux.
It also adds many features not found in those tools such as
command "blocks", better command editing capability, workflows,
a command palette, provided and custom themes,
permalinks to commands and their output, and "AI Command Search".

AI Command Search helps with discovering the shell command needed
to perform a task described in English.
For example, searching for "delete remote git branch"
suggests `git push origin --delete <branch>`.

Warp makes it easy to create a separate tab
for each category of work being done.
This allows you to focus on a task at hand.
To switch tasks, just click a different tab.

For example, perhaps you editing your blog and working on two projects.
Create a separate tab for each of these whose names are "blog"
and the project names.
Inside each tab create multiple panes by splitting the initially pane
horizontally, vertically, or both.
In the panes of a project tab you can run a web server, an API server,
and a database server. Other panes can be used for executing shell commands.

Warp supports the Bash, Zsh, and Fish shells.
It does not currently support Nushell.
