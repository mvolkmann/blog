---
eleventyNavigation:
  key: Codeium
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="Raycast logo" style="border: 0"
    src="/blog/assets/raycast-logo.png?v={{pkg.version}}">
</figure>

## Overview

<a href="https://codeium.com" target="_blank">Codeium</a>
is an AI assistant for software developers.
It is widely seen as the best free alternative to GitHub Copilot.

Codeium supports a large number of IDEs and code editors (over 40) including
emacs, JetBrains variants, Jupyter Notebooks, NeoVim,
Sublime, Vim, Visual Studio, and VS Code.

JetBrains variants include Android Studio, CLion, GoLand,
IntelliJ, PhpStorm, PyCharm, WebStorm, and more

Codeium supports a large number of
programming languages and syntaxes (over 70) including
C, CSS, C++, C#, Dart, Dockerfile, Elixir, Erlang, F#, Go, Gradle, Groovy,
Haskell, HTML, Java, JavaScript, Julia, JSON, Kotlin, Lua, OCaml, PHP, Prolog,
Python, Ruby, Rust, shell, SQL, Swift, Svelte, TypeScript, Vue, Yaml, Zig,
and more.

## Use in VS Code

The Codeium VS Code extension adds
many commands to the command palette including:

- Codeium: Command
- Codeium: Copy Status to Clipboard
- Codeium: Explain Problem
- Codeium: Explain Selected Code Block
- Codeium: Focus on Chat View
- Codeium: Focus on Search View
- Codeium: Open Chat (cmd-shift-a)
- Codeium: Open Chat in Editor
- Codeium: Open Profile
- Codeium: Open Search
- Codeium: Open University
- Codeium: Provide Authentication Token
- Codeium: Refactor Selected Code Block
- Codeium: Submit ++
- Codeium: Toggle Autocomplete Enabled for Current Language

## Code Suggestions

One way to get a code suggestion is to
enter a comment describing what you want to do.
Codeium will suggest the code to do it.

## Function Operations

Codeium displays a line above functions that contains
"Codeium: Refactor | Explain | Generate JSDoc | X".

"Refactor" opens a text area where you can describe
the desired change such as "make this shorter".
The suggest change will appear above the current code
with "Accept" and "Reject" buttons. Click one of them.

"Explain" generates an explanation of what the code does
in the Codeium Chat left sidebar.

"Generate JSDoc" places it in the Codeium chat left sidebar.
Click the "Apply Diff" button to copy it into the code above the function.
Click "Accept" above the inserted code to accept it.
