---
eleventyNavigation:
  key: Calling C
  parent: Swift
layout: topic-layout.njk
---

## Overview

Swift code can all functions written in C.
This page describes the steps to create a SwiftUI app
that includes C code and calls its functions.

## Steps

1. Create a new Xcode project where the interface is "SwiftUI"
   and the language is "Swift".
1. Select File ... New ... File (or press cmd-n).
1. In the "Source" category, select "C File".
1. Enter a name for the file without a file extension.
1. Check the "Also create a header file" checkbox.
1. Click the "Next" button.
1. Click the "Create" button.
1. In the dialog that appears, click the "Create Bridging Header" button.
1. In the generated header file, enter the signatures
   of the functions you will define.
   For example:

   ```c
   long factorial(int n);
   ```

1. In the generated C file, enter the function definitions.
   For example:

   ```c
   long factorial(int n) {
     if (n == 0 || n == 1) return 1;
     return n * factorial(n-1);
   }
   ```

1. In the generated `SwiftUICallsC-Bridging-Header.h` file,
   add import directives for each of your C header files.
   For example:

   ```c
   #import "factorial.h"
   ```

1. Call the C functions in Swift source files.
   For example, `ContentView.swift` could contain the following:

   ```swift
   Text("Factorial is \(factorial(5)).") // 120
   ```
