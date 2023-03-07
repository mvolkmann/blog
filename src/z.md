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
