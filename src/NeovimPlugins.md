---
eleventyNavigation:
  key: Neovim Plugins
layout: topic-layout.njk
---

## Overview

This article describes how to implement and share a custom plugin.

## Resources

- {% aTargetBlank "https://www.youtube.com/watch?v=PdaObkGazoU&t=924s", "Writing Plugins - It's Never Been Easier" %} by DevOnDuty
- {% aTargetBlank "https://www.youtube.com/watch?v=9gUatBHuXE0", "Automatically Execute *Anything* in Nvim" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=HlfjpstqXwE", "Execute **anything** in neovim (now customizable)" %}

## Basics

There are four ways to trigger a Lua function in Neovim.
Begin by defining a function in any buffer such as:

```lua
function Greet() print("Hello!") end
```

The first way to run the function is to source the file and use the `lua` command.
The `%` represents the current buffer.

```bash
:so %
:lua Greet()
```

The second way to run the function is to create a user command.
Add the following after the function definition inside the buffer.
The last argument is a table of options.

```lua
vim.api.nvim_create_user_command("Greet", Greet, {})
```

Now source the buffer and use the new command.

```bash
:so %
:Greet
```

The third way to run the function is to create an autocommand
and cause the action that triggers it.
Add the following after the function definition inside the buffer.

```lua
vim.api.nvim_create_autocmd("event-name", { callback = Greet })
```

To get a list of supported event names, ...

This can be triggered multiple times.
To see all the output, enter `:messages`.

The fourth way to run the function is to define a key mapping
and type the key sequence.
Add the following after the function definition inside the buffer.
"n" is for normal mode.
TODO: Is "<leader>g" already in use?

```lua
vim.keymap.set("n", "<leader>g", Greet)
```

## Buiding a Plugin

To build a Neovim plugin that can be shared with others:

1. Create a directory for the plugin.
   It is common for Neovim plugin repositories to have names that
   end in ".nvim". For example, "greet.nvim"
1. Create a `README.md` file in this directory that
   describes the functionality of the plugin and the steps to install it.
1. Create a `lua` directory inside it.
1. Create the file `{plugin-name}.lua` inside the `lua` directory.
1. In this file, define and return a Lua module. For example:

   ```lua
   local M = {}
   M.greet = function() print("Hello!") end
   return M
   ```

1. Create a GitHub repository for the plugin and push this directory to it.

## Installing a Plugin

There are several popular plugin managers for Neovim.
One that is highly recommended is {% aTargetBlank
"https://github.com/folke/lazy.nvim", "lazy.nvim" %}.
The instructions here assume that this plugin manager is being used.

Create the file `~/.config/nvim/lua/user/plugins/greet.lua`
containing the following:

```lua
return {
  -- "{github-account}/{repository-name}"
  "mvolkmann/greet.nvim"
}
```

In Neovim, enter `:Lazy sync`

## Testing a Plugin

A single command can be entered inside Neovim to
load a plugin and exercise one of the functions it defines.
For example, to verify that the "greet.nvim" plugin was installed,
enter the following in Neovim:

```text
:lua require("greet").greet("World")`
```

This should display "Hello, World!" in the message area at the bottom.

## LUA_PATH

To see the places that the Lua `require` function looks for `.lua` files,
enter `lua` to start it in interactive mode and
then enter `print(package.path)`.
By default this will include the following:

- `/usr/local/share/lua/5.4/?.lua`
- `/usr/local/share/lua/5.4/?/init.lua`
- `/usr/local/lib/lua/5.4/?.lua`
- `/usr/local/lib/lua/5.4/?/init.lua`
- `/?.lua`
- `/?/init.lua`

To add more paths to the beginning of this list,
define the environment variable `LUA_PATH`.
For example, when using zsh, add the following in `~/.zshrc`:

```bash
export LUA_PATH="${HOME}/lua/?.lua;;"
```

The second semicolon at the end is replaced by
the current value of `package.path`.

For me this adds `/Users/volkmannm/lua/?.lua;` to the beginning.
This makes it so searches for `.lua` files
begins in the `lua` subdirectory of my home directory.

To automatically require files from this directory
on startup of Neovim, add calls to the `require` function
in `~/.config/nvim/lua/user/init.lua`.
