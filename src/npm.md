---
eleventyNavigation:
  key: npm
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://docs.npmjs.com", "npm" %}
is short for "Node Package Manager".

This page documents selected features of npm
and is far from comprehensive.

## Linking

npm supports testing local changes to
a package that has been published to npm.
For example, suppose you are developing an application
that uses the package "foo".
To test changes to the code in that package
from your application:

1. Clone the foo Git repository, creating a local repository.
1. Make changes in the local repository.
1. Enter `npm build`.
1. `cd` to the root directory of the foo local repository.
1. Enter `npm link`.
1. `cd` to the directory of your app.
1. Enter `npm link foo`.
1. Test your application.
1. Continue making changes in the foo local repository,
   entering `npm build` after each set of changes,
   and retesting your app.

When finished testing:

1. If appropriate, create a pull request to the foo repository
   containing your changes.
1. `cd` to the root directory of the foo local repository.
1. Enter `npm unlink`.
1. `cd` to the directory of your app.
1. Enter `npm unlink foo`.
