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
