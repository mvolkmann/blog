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

## Libraries

To use a C library from Swift:

1. Build a version of the C library that is compatible with iOS.
1. Create a group within the Xcode project named "include".
1. Copy the `.h` files for each library into this directory.
1. Create a group within the Xcode project named "lib".
1. Copy the `.a` files for each library into this directory.
1. Select the top entry in the project navigator.
1. Select the project.
1. Select the "Build Settings" tab.
1. Open the "Search" section.
1. Open the "Header Search Paths" entry.
1. In the "Release" section, add the string "include".
1. Open the "Library Search Paths" entry.
1. In the "Release" section, add the string "lib".

## Invoking Lua

For details on the Lua programming language, see
{% aTargetBlank "/blog/topics/#/blog/lua/", "Lua" %}.
This contains details on starting the Lua virtual machine from C
and deciding which standard libraries should be loaded.
For example, but not loading the "os" library,
Lua code is unable to read or write files.

1. Build a version of Lua that is compatible with iOS.
   See {% aTargetBlank "https://github.com/apotocki/lua-iosx", "lua-iosx" %}.
   Enter the following commands to generate
   the directory `frameworks/lua.xcframework`:
   - `git clone -b 5.4.4 https://github.com/apotocki/lua-iosx`
   - `cd lua-iosx`
   - `scripts/build.sh`
1. Create a "Frameworks" group.
1. Drag the `lua-ios/frameworks/lua.xcframework` directory into the group.
1. In the dialog that appears,
   check the "Copy items if needed" checkbox
   and click the "Finish" button.

It is now possible to call functions from the Lua C API
from functions defined in C source files.
Those C functions can be called from Swift code.

For an example of embedding the Lua interpreter in a SwiftUI application, see
{% aTargetBlank "/blog/topics/#/blog/swift/CallingC", "Swift Calling C" %} and
the GitHub respository {% aTargetBlank
"https://github.com/mvolkmann/SwiftUICallsC", "SwiftUICallsC" %}.

TODO: In iOS can you read a Lua file that is in the Files app?
