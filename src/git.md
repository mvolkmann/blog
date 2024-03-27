---
eleventyNavigation:
  key: Git
layout: topic-layout.njk
---

## Overview

This page documents tips on using Git for version control.
At the moment it is very incomplete.

## Viewing Commits

To see all the commits that have been made on the current branch:

- `git log` to output the full commit hash, author,
  timestamp, and comment for each commit
- `git log --oneline` to see only the first 8 characters of the hash
  and the comment for each commit

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

Rebasing in git is a scary topic for many.

The main benefit of rebasing over just merging is
producing a cleaner git history so the changes made
are more clear when reviewing the history.

The `git rebase` command can be used to:

- incorporate changes made in branch A (such as main)
  into the current branch B (such as a feature branch)
  by starting with a copy of A and
  replaying changes made in B on top of it
- combine multiple commits into one (squash)
- split one commit into multiple commits (questionable)
- reorder commits (questionable)

Rebasing should be avoided for feature branches
that are being modified by multiple developers.

Let's walk through the entire flow from creating a feature branch,
to working on it over a period of time,
to merging the changes back to the main branch.
During the time you work on your feature branch,
other developers (or even you) may have
merged other feature branches back to main.

- Switch to the main branch with `git checkout main`.

- Update your local main branch from the remote with `git pull origin`.

- Create a feature branch with `git checkout -b my-feat`.

- Make changes to files in the feature branch,
  including adding and deleting files.

- Periodically commit changes to the feature branch
  with `git add .` and `git commit -av`.

  The `a` commits all changes and the `v` flag shows the diffs
  in the editor where you will compose a commit message,
  which helps you write a good one.

- When work on the feature branch is finished, or periodically,
  rebase the main branch only the feature branch with the following:

  - `git checkout main`
  - `git pull`
  - `git checkout my-feat`
  - `git rebase -i main`

  The `-i` flag (for "interacive") causes git to open a text editor
  where you can make changes to commits. It uses Vim by default.
  To configure git to use a different editor such as nano,
  enter `git config --global core.editor "nano"`.

  If there are any conflicts, resolve them in each reported file,
  enter `git add {file}` for each file, and run `git rebase --continue`.

  To modify the comment on a commit, change "pick" to "reword" (or "r")
  and edit the comment.

  To squash (combine) a set of commits into a single commit,
  pick the earliest one to retain and change "pick" to "squash" (or "s")
  for any number of consecutive commits that follow it.
  There is no need to edit the commit message of the retained commit
  because you will be prompted for a new commit message
  for the squashed commit.

  To delete a commit as if it never happened,
  change "pick" to "drop" (or just "d").

  Save the changes and quit the editor to start the rebase process.

  This updates my-feat by setting it to a copy of main and then
  replaying all the changes made in my-feat onto this copy of main.

- Test the code on my-feat to verify that
  everything is still working correctly.

- Merge the feature branch to main with the following:

  - `git checkout main`
  - `git pull`
  - `git merge my-feat`

- Delete the local feature branch with `git branch -d my-feat`

- Delete the remote feature branch with `git branch -rd origin/my-feat`
