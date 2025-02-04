---
eleventyNavigation:
  key: OSProcess
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

The `OSProcess` package provides methods for querying and interacting with
the operating system processes in the Smalltalk virtual machine is running.

To use this, install the package with `Feature require: #OSProcess`.

To execute a shell command, evaluate `OSProcess command: 'some-shell-command'`.
TODO: How can you get the text written to stdout and stderr?

To determine the current operating system,
send the class messages `isRiscOS`, `isUnix`, `isUnixMac`, and `isWindows`
to the `OSProcess` class.

To get the operating system version, evaluate `OSProcess osVersion`.

To get the initials of the current user, evaluate `OSProcess authorInitials`.

To get the current working directory:

```smalltalk
process := OSProcess thisOSProcess.
cwd := process getCwd.
```

To get the value of an environment variable:

```smalltalk
process := OSProcess thisOSProcess.
value := process environmentAt: 'HOME'.
```

Another way to do this is to add a class method like the following that
uses FFI to any class (perhaps `Object` so it can be called from anywhere):

```smalltalk
getenv: aString
    "Answer value of environment variable with the given name of nil if not found."

    <cdecl: char* 'getenv' (char*) module: 'libSystem.dylib'>
    ^self externalCallFailed
```

TODO: There are many more classes and methods in the OSProcess package that should be documented here.

Yet one more way to get the value of an environment variable
is to use the package
<a href="https://codeberg.org/auverlot/Cuis-Smalltalk-Environment"
target="_blank">Environment</a>.
This package defines methods to:

- create, get, set, and unset environment variables
- get the id and group id of the current process
- execute a shell command
- determine if the last executed shell command was successful
