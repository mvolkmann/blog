---
eleventyNavigation:
  key: FFI
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

The Foreign Function Interface (FFI) enables
calling code written in other programming languages.

TODO: Does this work with any languages other than C?

See <a href="https://itchyeyes.net/articles/cuis-smalltalk.html"
target="_blank">Using the FFI</a>.

TODO: Add detail on this.

## Example

Let's create a shared library containing simple C functions.

Here is the file `mylib.c`:

```c
#include <stdio.h>

void hello() {
    printf("Hello from a shared library!\n");
}

int add(int a, int b) {
  return a + b;
}
```

To build a shared library from this, enter the command
`clang -dynamiclib -o libmylib.dylib mylib.c`.
