---
eleventyNavigation:
  key: Git
layout: topic-layout.njk
---

## Overview

This page documents tips on using Git for version control.
At the moment it is very incomplete.

## Tagging

To list existing tags in a repository, enter `git tag`.

To add a new tag, enter `git tag -a {tag}`.
It is common for tag names to use semantic versioning.
For example, `v{major}.{minor}.{patch}`.

To push a tag to the remote repository, enter `git push origin {tag}`.

To see details about a specific tag, enter `git show {tag}`.
