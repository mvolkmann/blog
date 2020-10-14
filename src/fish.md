---
eleventyNavigation:
  key: fish Shell
layout: topic-layout.njk
---

Nearly all developers spend time using terminal windows, entering commands,
such as git, and automating tasks by writing and running scripts.
How would you like to use a shell that is more friendly
and provides a simpler, more consistent syntax for scripts?

Fish (the friendly interactive shell) is a \*nix command-line shell
that offers an alternative to shells like bash and zsh.

At a high level, fish provides the following benefits:

- colorful prompts
- auto-suggestions while typing
- auto-completion of commands, their switches (aka options),
  file paths, variable names, git branches, and more
- easy access to web-based help
- error messages that explain the problem and suggest a solution
- ability to customize colors and the prompt using a web UI
- highly consistent and simple scripting language where
  everything is done with commands, not special syntax to be memorized
- commands never create subshells, so changes they make
  (such as setting variables) are visible in the invoking shell
- variables can be set so they are shared with
  all current and future shells (see "universal variables")

This article explains the fish shell in enough detail for you to
determine whether you might prefer it over other shells.
Really learning a shell is similar to learning a new programming language.
Similar topics are covered here such as variables, functions,
string operations, and so on.
If writing scripts in other shells has felt tedious in the past,
this is your chance to learn a shell that makes scripting easier!

See my Object Computing (OCI) article {% aTargetBlank
"https://objectcomputing.com/resources/publications/sett/november-2017-sett-article-fish-shell",
"The fish Shell" %}.
