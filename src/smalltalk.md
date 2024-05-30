---
eleventyNavigation:
  key: Smalltalk
layout: topic-layout.njk
---

<style>
  .logo {
    --size: 8rem;
    max-height: var(--size);
    max-width: var(--size); 
    margin-left: 2rem;
  }

  .row {
    display: flex;
    align-items: center;
    margin-bottom: 1rem;
  }
</style>

<figure style="width: 30%">
  <img alt="Smalltalk Byte magazine cover" style="border: 0"
    src="/blog/assets/smalltalk-byte-cover.jpg?v={{pkg.version}}">
</figure>

## Overview

<a href="https://en.wikipedia.org/wiki/Smalltalk" target="_blank">Smalltalk</a>
"is a purely object oriented programming language (OOP)."

## Smalltalk Pros

- It has a small, consistently applied syntax.
- It has a great development environment consisting of tools such as
  System Browser, Workspace, Transcript, Debugger, Hierarchy Browser,
  Method Finder and more
- Everything is an object.
- It provides automatic version control.
- It provides extreme polymorphism.
  Any kind of object can be passed to a method as long as it
  responds to the messages that will be sent to it.
- It has a great web app. framework (Seaside) and a great CMS framework (Pier).

## Smalltalk Cons

- It isn't as popular as many other programming languages.

  - Schools generally don't teach it.
  - Few jobs using it are available.
  - IT press doesn't talk about it.
  - It's difficult to convince others to use it.

- It doesn't minimize compile-time errors as much as
  statically typed languages such as C++, C# and Java.
  However, it does do incremental compiling when methods are saved,
  so it finds syntax errors before runtime, unlike most scripting languages.

- All the code for a project is stored in one big image file (often over 30 MB).

- The syntax is fairly different from most programming languages.

  - no dots or parentheses used in method calls
  - conditional and iterating constructs are method calls instead of keywords
  - keyword messages are a departure from positional arguments
  - method cascading (sending multiple messages to the same object) is a new concept

- Performance may be an issue.

- Classes are not in a namespace, so all class names must be unique.

  Using class name prefixes is recommended.
  This is important for using Squeak packages and Monticello.
  Squeak has a prefix registry in the wiki.

## Implementations

There are many Smalltalk implementations.
The most popular include:

- <div class="row">
    <a href="https://squeak.org" target="_blank">Squeak</a>
    <img alt="Squeak Smalltalk log" class="logo" style="border: 0"
      src="/blog/assets/squeak-smalltalk-logo.svg?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://pharo.org" target="_blank">Pharo</a>
    <img alt="Pharo Smalltalk log" class="logo" style="border: 0"
      src="/blog/assets/pharo-smalltalk-logo.png?v={{pkg.version}}">
  </div>

- <div class="row">
    <a href="https://cuis.st" target="_blank">Cuis</a>
    <img alt="Cuis Smalltalk log" class="logo" style="border: 0"
      src="/blog/assets/cuis-smalltalk-logo.png?v={{pkg.version}}">
  </div>

## Installing Cuis Smalltalk

Download a zip from from the
<a href="https://github.com/Cuis-Smalltalk/Cuis6-2"
target="_blank">Cuis Smalltalk Git repository</a> and unzip it.

Run the appropriate start script based on your operating system.

- for Windows, open a Command Prompt and run `RunCuisOnWindows.bat`
- for Linux, open a Terminal and run `RunCuisOnLinux.sh`
- for Mac

  1. Double-click CuisVM.app which will fail because the app is not verified.
  1. Open the Settings app.
  1. Select "Privacy & Security".
  1. Scroll down to the "Security" section.
  1. Click the "Allow" button for CuisVM.app.
  1. Double-click a Smalltalk image file such as `CuisImage/Cuis6.2.image`.
  1. The following main app window will open.

     <img alt="Cuis Smalltalk log" class="logo" style="width: 400px"
       src="/blog/assets/cuis-smalltalk-startup.png?v={{pkg.version}}">

## Getting Started

1. Click on the desktop and select Open...Workspace.
1. Enter `Transcript show: 'Hello World!'`
1. If no Transcript window is open, one one by
   clicking on the desktop and selecting Open...Transcript.
1. Right-click inside the Workspace window and select "Do it" or press cmd-d.
1. The output will appear in the Transcript window.
1. To clear the output in the Transcript,
   right-click in it and select "Clear Transcript".
