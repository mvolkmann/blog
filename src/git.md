---
eleventyNavigation:
  key: Git
layout: topic-layout.njk
---

## Overview

This page documents tips on using Git for version control.
At the moment it is very incomplete.

## Ignoring files

Each repository can have a `.gitignore` file that lists the
files and directories that should be committed to the repository.

A global `.gitignore` file can be added to your home directory
to prevent committing common files such as `.env`
and common directories such as `node_modules`.

## Tagging

To list existing tags in a repository, enter `git tag`.

To add a new tag, enter `git tag -a {tag}`.
It is common for tag names to use semantic versioning.
For example, `v{major}.{minor}.{patch}`.

To push a tag to the remote repository, enter `git push origin {tag}`.

To see details about a specific tag, enter `git show {tag}`.

## Rebase

Rebasing in git is a scary topic for many, including me.
So I decided to document the steps and verify them to remove the fear.

The main benefit of rebasing over just merging is
producing a cleaner git history so the changes made
are more clear when reviewing the history.

The `git rebase` command can be used to:

- combine multiple commits into one (squash)
- split one commit into multiple commits (scary)
- reorder commits (scary)
- incorporate changes made in one branch into the current one
  so the changes appear to have been made on the current branch (scary)

Let's walk through the entire flow from creating a feature branch,
to working on it over a period of time,
to merging the changes back to the main branch.
During the time you work on your feature branch,
other developers (or even you) may have
merged other feature branches back to main.

- Switch to the main branch with `git checkout main`

- Update your local main branch from the remote with `git pull origin`

- Create a feature branch with `git checkout -b my-feat`

- Make changes to files in the feature branch,
  including adding and deleting files.

- Periodically commit changes to your feature branch
  with `git add .` and `git commit -av`

- Periodically rebase your feature branch from main with the following:

  - `git checkout main`
  - `git pull`
  - `git checkout my-feat`
  - `git rebase -i main`

  The `-i` flag makes it interactive which allows you
  to squash (combine) commits into a single commit.

  This updates my-feat by setting it to the current state of main and
  then replaying all the changes made on my-feat onto this copy of main.

- Merge the feature branch to main with the following:

  - `git checkout main`
  - `git pull`
  - `git merge my-feat`

- Delete the local feature branch with `git branch -d my-feat`

- Delete the remote feature branch with `git branch -rd origin/my-feat`
