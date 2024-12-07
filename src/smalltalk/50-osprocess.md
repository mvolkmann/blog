---
eleventyNavigation:
  key: OSProcess
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

The `OSProcess` package provides methods for querying and interacting with
the operating system processes in the Smalltalk virtual machine is running.

To use this, install the package with `Feature require: 'OSProcess'`.

To execute a shell command, evaluate `OSProcess command: 'some-shell-command'`.
TODO: How can you get the text written to stdout and stderr?

To determine the current operating system,
send the class messages `isRiscOS`, `isUnix`, `isUnixMac`, and `isWindows`
to the `OSProcess` class.

To get the operating system version, evaluate `OSProcess osVersion`.

To get the initials of the current user, evaluate `OSProcess authorInitials`.

To get the current working directory:

```smalltalk
process := ThisOSProcess thisOSProcess.
cwd := process getCwd.
```

TODO: There are many more classes and methods in the OSProcess package that should be documented here.
