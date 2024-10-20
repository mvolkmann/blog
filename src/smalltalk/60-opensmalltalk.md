---
eleventyNavigation:
  key: OpenSmalltalk
  order: 60
  parent: Smalltalk
layout: topic-layout.njk
---

<a href="https://opensmalltalk.org" target="_blank">OpenSmalltalk</a>
is a "cross-platform virtual machine for Squeak, Pharo, Cuis, and Newspeak".

Many variants of the VM can be built with options such as:

- with or without just-in-time (JIT) compilation

- green threads ("light-weight processes above a single-threaded VM) or
  multi-threaded VM ("shares the VM betwwen any number of native threads")

- with or without Spur

  Spur provides "a partial read barrier for efficient support of
  live object-oriented programming."
  It is currently used by Squeak, Pharo, and Cuis.

- with or without Sista

  Sista "adds support for adaptive optimization that does speculative inlining".
  As of October 2024 it was still in development.

To build a VM:

- `git clone https://github.com/OpenSmalltalk/opensmalltalk-vm.git`
- `./scripts/updateSCCSVersions`
- run one of the scripts in the `scripts` directory such as
  ``
- For ARM-based Macs

  - cd to the `building/macos64ARMv8` directory.
  - enter 'xcodebuild -runFirstLaunch`
  - enter './makespur`

    This will run for a long time (~10 minutes) and produces a large amount of output.
    There will be many warnings about issues such as unused variables.
    I got 39 warnings and 1 error.
    The error was "call to undeclared function 'literalBeforeInlineCacheTagAt'".
    Where is the generated VM placed?
