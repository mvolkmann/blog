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
- {% aTargetBlank "https://www.barbarianmeetscoding.com/notes/neovim-plugins/",
  "Neovim Plugins - Enhancing your Neovim editor with awesome plugins" %}

## Basics

There are four ways to trigger a Lua function in Neovim.
Begin by defining a function in any buffer such as:

```lua
function Greet(name)
  name = name or "World"
  print("Hello, " .. name .. "!")
end
```

The first way to run the function is to source the current buffer
and use the `lua` command to call the function.

```bash
:so
:lua Greet("Mark")
```

By default the `source` command sources the current buffer
which can also be specified by `:so %`.

The second way to run the function is to create a user command.
This requires changing the definition of the function
to accept an options table as shown below.
This contains the keys `args` and `fargs`.
The value of the `args` key is a single string containing
the values of all arguments passed to the command.
The value of the `fargs` key is a sequence table
containing the values of all arguments.

```lua
function Greet(opts)
  -- Also see vim.inspect(value).
  local name = #opts.fargs > 0 and opts.args or "World"
  name = name:gsub('"', '') -- removes double quotes
  print("Hello, " .. name .. "!")
end
```

Add the following after the function definition inside the buffer.
The last argument is a table of options and is required.

```lua
vim.api.nvim_create_user_command("Greet", Greet, {})
```

Source the buffer again and then use the new command.
For example:

```bash
:so
:Greet
:Greet "Mark"
```

The third way to run the function is to create an autocommand
and cause the action that triggers it.
For this approach we can return to a simpler version of the `Greet` function.
The event in this example is writing any buffer whose file name ends in `.lua`.

```lua
function Greet(name)
  name = name or "World"
  print("Hello, " .. name .. "!")
end

vim.api.nvim_create_autocmd("BufWritePost", {
  -- Placing the autocmd inside an augroup allows us to clear
  -- existing autocmds in the group every time this file is sourced.
  -- We need to do this so sourcing this file multiple times
  -- doesn't register multiple callbacks to run when the event occurs.
  group = vim.api.nvim_create_augroup("autocmd", { clear = true }),
  pattern = "*.lua",
  callback = function() Greet("Mark") end
})
```

To see a list of the supported events, enter `:h events`.
The supported event names include:

- `BufAdd`
- `BufDelete`
- `BufEnter`
- `BufFilePost`
- `BufFilePre`
- `BufHidden`
- `BufLeave`
- `BufModifiedSet`
- `BufNewFile`
- `BufNew`
- `BufReadCmd`
- `BufReadPre`
- `BufRead` of `BufReadPost`
- `BufUnload`
- `BufWinEnter`
- `BufWinLeave`
- `BufWipeout`
- `BufWriteCmd`
- `BufWritePost`: after a buffer has been written
- `BufWrite` or `BufWritePre`
- `ChanInfo`
- `ChanOpen`
- `ChanUndefined`
- `CmdLineChange`
- `CmdLineEnter`
- `CmdLineLeave`
- `CmdwinEnter`
- `CmdwinLeave`
- `ColorSchemePre`
- `ColorScheme`
- `CompleteChanged`
- `CompleteDonePre`
- `CompleteDone`
- `CursorHoldI`: when no key has been pressed for some amount of time in insert mode
- `CursorHold`: when no key has been pressed for some amount of time in normal mode
- `CursorMovedI`: when cursor is moved in insert mode
- `CursorMoved`: when cursor is moved in normal or visual mode
- `DiffChangedPre`
- `DiffChanged`
- `DiffUpdated`
- `ExitPre`
- `FileAppendedCmd`
- `FileAppendedPost`
- `FileAppendedPre`
- `FileChangedRO`
- `FileChangedShellPost`
- `FileChangedShell`
- `FileReadCmd`
- `FileReadPost`
- `FileReadPre`
- `FileType`
- `FileWriteCmd`
- `FileWritePost`
- `FileWritePre`
- `FocusGained`
- `FocusLost`
- `FuncUndefined`
- `InsertChange`
- `InsertCharPre`
- `InsertEnter`
- `InsertLeavePre`
- `InsertLeave`
- `MenuPopup`
- `ModeChanged`
- `OptionSet`
- `QuickFixCmdPost`
- `QuickFixCmdPre`
- `QuitPre`
- `RecordingEnter`
- `RecordingLeave`
- `RemoteReply`
- `SearchWrapped`
- `SessionLoadPost`
- `ShellCmdPost`
- `ShellFilterPost`
- `Signal`
- `SourceCmd`
- `SourcePost`
- `SourcePre`
- `SpellFileMissing`
- `StdinReadPost`
- `StdinReadPre`
- `SwapExists`
- `Syntax`
- `TabCosed`
- `TabEnter`
- `TabLeave`
- `TabNewEntered`
- `TabNew`
- `TermClose`
- `TermEnter`
- `TermLeave`
- `TermOpen`
- `TermResponse`
- `TextChangedI`
- `TextChangedP`
- `TextChangedT`
- `TextChanged`
- `TextYankPost`
- `UIEnter`
- `UILeave`
- `UserGettingBored`
- `User`
- `VimEnter`
- `VimLeavePre`
- `VimLeave`
- `VimResized`
- `VimResume`
- `VimSuspend`
- `WinClosed`
- `WinEnter`
- `WinNew`
- `WinResized`
- `WinScrolled`
- `Winleave`
- `nvim_buf_attach` `nvim_buf_changedtick_event`
- `nvim_buf_detach_event`
- `nvim_buf_lines_event`
- `nvim_error_event`

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

## Building a Plugin

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

   -- It is a convention for plugins to have a `setup` function
   -- that can be called to configure it.
   -- The plugin managers Lazy and Packer both assume this.
   M.setup = function()
     -- Define user commands here.
     -- Define key mappings here.
   end

   return M
   ```

1. Create a GitHub repository for the plugin and push this directory to it.

## Installing a Plugin

When `nvim` is started it runs all `.lua` files found in the
`~/.config/nvim/lua/plugins` and `~/.config/nvim/lua/user/plugins` directories.
A popular approach for configuring plugins is to create the file
`~/.config/nvim/lua/user/plugins/{plugin-name}.lua`
for each plugin to be configured.

For example, to configure a plugin named "greet",
create the file `~/.config/nvim/lua/user/plugins/greet.lua`
containing the following:

```lua
return {
  -- "{github-account}/{repository-name}"
  "mvolkmann/greet.nvim"
}
```

There are several popular plugin managers for Neovim.
One that is highly recommended is {% aTargetBlank
"https://github.com/folke/lazy.nvim", "lazy.nvim" %}.
If this is being used, enter `:Lazy sync` to install
any missing plugins that were configured,
update any that have a new version,
and remove any that are no longer being used.

## Testing a Plugin

A single command can be entered inside Neovim to
load a plugin and exercise one of the functions it defines.
For example, to verify that the "greet.nvim" plugin was installed,
enter the following in Neovim:

```text
:lua require("greet").greet("World")`
```

This should display "Hello, World!" in the message area at the bottom.

I configured the key mapping <leader>x
to write and source the current buffer.
This is useful when developing a Neovim plugin.

## Debugging

When debugging a plugin it is helpful to print values of variables.
The function `vim.print` performs pretty-printing of all kinds of values
including tables.
The function `vim.inspect` is similar,
but returns a pretty-printed string rather than printing anything.

## Auto Commands

Read ":help autocmd"

## autorun Example

See an example Neovim plugin in {% aTargetBlank
"https://github.com/mvolkmann/autorun.nvim", "GitHub" %}.
This opens a new buffer in a vertical split,
prompts for a file matching pattern (ex. `*.lua`), and
prompts for a command to run if any matching files are saved
(ex. `lua demo.lua`).
Each time the command is run, the contents of the new buffer
are replaced anything the command writes to stdout or stderr.
This is great for debugging apps that have command-line output.

To configure use of this plugin, create the file
`~/.config/nvim/lua/user/plugins/autorun.lua` containing the following:

```lua
return {
  "mvolkmann/autorun.nvim",
  lazy = false, -- load on startup, not just when required
  config = true -- require the plugin and call its setup function
}
```

Setting `config` to `true` is the equivalent of the following:

```lua
  config = function()
    require("autorun").setup()
  end
```

## Library/Plugin Caching

Lua caches all libraries loaded by the `require` function.
Additional calls to `require` for a loaded library
will find it in the cache and return it without reloading it.
To force a library to reload, perhaps because its code has changed,
remove it from the cache and then call `require` as follows:

```lua
package.loaded.{plugin-name} = nil
require "plugin-name"
```

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

## Neovim API

The {% aTargetBlank "https://neovim.io/doc/user/api.html", "Neovim API" %}
provides functions in many categories.
Each of these are summarized in the following subsections.

## Treesitter Playground

{% aTargetBlank "https://github.com/nvim-treesitter/nvim-treesitter",
"Treesitter" %} parses source code into an AST.

For help on Treesitter, enter `:h nvim-treesitter`.

The plugin `nvim-treesitter/playground` displays ASTs generated by Treesitter.
This is helpful when developing plugins that act on specific AST nodes.

The instructions below assume that the Treesitter plugin is already configured.

To use the playground plugin:

1. Create the file `~/.config/nvim/lua/user/plugins/playground.lua`
   containing the following:

   ```lua
   return {
     "nvim-treesitter/playground",
     lazy = false -- load on startup, not just when required
   }
   ```

1. Modify the file `~/.config/nvim/lua/user/plugins/treesitter.lua`
   to contain the following. Note the `playground` value.

   ```lua
    local treesitter = {
      "nvim-treesitter/nvim-treesitter",
      opts = function()
        require 'nvim-treesitter.configs'.setup {
          incremental_selection = {
            enable = true,
            keymaps = {
              init_selection = "<leader>sw",    -- select word
              node_incremental = "<leader>sn",  -- incremental select node
              scope_incremental = "<leader>ss", -- incremental select scope
              node_decremental = "<leader>su"   -- incremental select undo
            }
          },
          playground = {
            enable = true,
            disable = {},
            updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
            persist_queries = false, -- Whether the query persists across vim sessions
            keybindings = {
              toggle_query_editor = 'o',
              toggle_hl_groups = 'i',
              toggle_injected_languages = 't',
              toggle_anonymous_nodes = 'a',
              toggle_language_display = 'I',
              focus_language = 'f',
              unfocus_language = 'F',
              update = 'R',
              goto_node = '<cr>',
              show_help = '?',
            },
          },
          query_linter = {
            enable = true,
            use_virtual_text = true,
            lint_events = {"BufWrite", "CursorHold"},
          }
        }
      end
    }
   ```

1. Restart `nvim`.
1. Enter `:TSInstall query`
1. Open any source file.
1. Enter `:TSPlaygroundToggle` to toggle display of the AST
   for code in the current buffer.
   This will open a new vertical split and display the AST there.
1. Move the cursor to any AST node
   to highlight the corresponding source code token.
1. Move the cursor to any source code token
   to highlight the corresponding AST node.
1. With focus in the AST buffer, press `o` to toggle display of
   a query editor buffer below the AST.
1. Enter a query like `(comment) @comment` and exit insert mode.
1. Matching nodes in the AST and matching tokens in the source code
   will be highlighted.

The following key mappings can be used when focus is in the AST buffer:

| Key    | Operation                                                         |
| ------ | ----------------------------------------------------------------- |
| `o`    | toggles display of query editor buffer below the AST buffer       |
| `a`    | toggles display of anonymous nodes such as keywords and operators |
| `i`    | toggles display of highlight groups                               |
| `I`    | toggles display of the language to which each node belongs        |
| `<cr>` | moves cursor to corresponding source code token                   |

## Plugin Using Treesitter

See an example Neovim plugin that uses Treesitter in {% aTargetBlank
"https://github.com/mvolkmann/todo-quickfix.nvim", "GitHub" %}.
This parses the source code in the current buffer,
populates the quickfix list will all comment lines that contain "TODO",
and opens the quickfix list.

To configure use of this plugin, create the file
`~/.config/nvim/lua/user/plugins/todo-quickfix.lua` containing the following:

```lua
return {
  "mvolkmann/todo-quickfix.nvim",
  lazy = false, -- load on startup, not just when required
  config = true -- require the plugin and call its setup function
}
```

Open a source file containing TODO comments and enter `:TodoQF`.

## Highlight Groups

A highlight group associate a name with a
foreground color, background color, and style such a bold.

To see a list of defined highlight groups, enter `:hi`.

## Neovim API

The {% aTargetBlank "https://neovim.io/doc/user/api.html", "Neovim API" %}
provides functions in many categories.
Each of these are summarized in the following subsections.

To see help for a given function, enter `:h {function-name}`.

### Autocmd Functions

| Function                                                                                                                    | Description                  |
| --------------------------------------------------------------------------------------------------------------------------- | ---------------------------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_clear_autocmds()" target="_blank">nvim_clear_autocmds(\*opts)</a>         |                              |
| <a href="https://neovim.io/doc/user/api.html#nvim_create_augroup()" target="_blank">nvim_create_augroup(name, \*opts)</a>   | crates an auto-command group |
| <a href="https://neovim.io/doc/user/api.html#nvim_create_autocmd()" target="_blank">nvim_create_autocmd(event, \*opts)</a>  |                              |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_augroup_by_id()" target="_blank">nvim_del_augroup_by_id(id)</a>       |                              |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_augroup_by_name()" target="_blank">nvim_del_augroup_by_name(name)</a> |                              |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_autocmd()" target="_blank">nvim_del_autocmd(id)</a>                   |                              |
| <a href="https://neovim.io/doc/user/api.html#nvim_create_autocmd()" target="_blank">nvim_create_autocmd()</a>               |                              |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_autocmds()" target="_blank">nvim_get_autocmds(\*opts)</a>             |                              |

### Buffer Functions

| Function                                                                                                                                                             | Description                 |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_attach()" target="_blank">nvim_buf_attach(buffer, send_buffer, opts)</a>                                       |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_call()" target="_blank">nvim_buf_call(buffer, fun)</a>                                                         |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_del_keymap()" target="_blank">nvim_buf_del_keymap(buffer, mode, lhs)</a>                                       |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_del_mark()" target="_blank">nvim_buf_del_mark(buffer, name)</a>                                                |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_del_var()" target="_blank">nvim_buf_del_var(buffer, name)</a>                                                  |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_delete()" target="_blank">nvim_buf_delete(buffer, opts)</a>                                                    |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_detach()" target="_blank">nvim_buf_detach(buffer)</a>                                                          |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_changedtick()" target="_blank">nvim_buf_get_changedtick(buffer)</a>                                        |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_keymap()" target="_blank">nvim_buf_get_keymap(buffer, mode)</a>                                            |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_lines()" target="_blank">nvim_buf_get_lines(buffer, start, end, strick_indexing)</a>                       |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_mark()" target="_blank">nvim_buf_get_mark(buffer, name)</a>                                                |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_name()" target="_blank">nvim_buf_get_name(buffer)</a>                                                      |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_offset()" target="_blank">nvim_buf_get_offset(buffer, index)</a>                                           |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_text()" target="_blank">nvim_buf_get_text(buffer, start_row, start_col, end_row, end_col, opts)</a>        |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_var()" target="_blank">nvim_buf_get_var(buffer, name)</a>                                                  |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_is_loaded()" target="_blank">nvim_buf_is_loaded(buffer)</a>                                                    |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_is_valid()" target="_blank">nvim_buf_is_valid(buffer)</a>                                                      |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_line_count()" target="_blank">nvim_buf_line_count(buffer)</a>                                                  |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_keymap()" target="_blank">nvim_buf_set_keymap(buffer, mode, lhs, rhs, \*opt)</a>                           |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_lines()" target="_blank">nvim_buf_set_lines(buffer, start, end, strict_indexing, replacement)</a>          | adds text in a given buffer |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_mark()" target="_blank">nvim_buf_set_mark(buffer, name, line, col, opts)</a>                               |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_name()" target="_blank">nvim_buf_set_name(buffer, name)</a>                                                |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_text()" target="_blank">nvim_buf_set_text(buffer, start_row, start_col, end_row, end_col, replacement)</a> |                             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_var()" target="_blank">nvim_buf_set_var(buffer, name, value)</a>                                           |                             |

### Command Functions

| Function                                                                                                                                     | Description                                             |
| -------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_create_user_command()" target="_blank">nvim_buf_create_user_command(buffer, name)</a>  | creates a global user command                           |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_del_user_command()" target="_blank">nvim_buf_del_user_command(buffer, name)</a>        |                                                         |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_commands()" target="_blank">nvim_buf_get_commands(buffer, \*opts)</a>              |                                                         |
| <a href="https://neovim.io/doc/user/api.html#nvim_cmd()" target="_blank">nvim_cmd(*cmd, *opts)</a>                                           |                                                         |
| <a href="https://neovim.io/doc/user/api.html#nvim_create_user_command()" target="_blank">nvim_create_user_command(name, command, \*opts)</a> | creates a user command; `command` can be a Lua function |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_user_command()" target="_blank">nvim_del_user_command(name)</a>                        |                                                         |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_commands()" target="_blank">nvim_get_commands(\*opts)</a>                              |                                                         |
| <a href="https://neovim.io/doc/user/api.html#nvim_parse_cmd()" target="_blank">nvim_parse_cmd(str, opts)</a>                                 |                                                         |

### Extmark Functions

| Function                                                                                                                                                             | Description |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_add_highlight()" target="_blank">nvim_buf_add_highlight(buffer, ns_id, hl_group, line, col_start, col_end)</a> |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_clear_namespace()" target="_blank">nvim_buf_clear_namespace(buffer, ns_id, line_start, line_end)</a>           |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_del_extmark()" target="_blank">nvim_buf_del_extmark(buffer, ns_id, id)</a>                                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_extmark_by_id()" target="_blank">nvim_buf_get_extmark_by_id(buffer, ns_id, id, opts)</a>                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_extmark()" target="_blank">nvim_buf_set_extmark(buffer, ns_id, line, col, \*opts)</a>                      |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_create_namespace()" target="_blank">nvim_create_namespace(name)</a>                                                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_namespaces()" target="_blank">nvim_get_namespaces()</a>                                                        |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_decoration_provider()" target="_blank">nvim_set_decoration_provider(ns_id, \*opts)</a>                         |             |

### Global Functions

| Function                                                                                                                                                | Description |
| ------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------- |
| <a href="https://neovim.io/doc/user/api.html#nvim__get_runtime()" target="_blank">nvim\_\_get_runtime(pat, all, \*opts)</a>                             |             |
| <a href="https://neovim.io/doc/user/api.html#nvim__id()" target="_blank">nvim\_\_id(obj)</a>                                                            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim__id_array()" target="_blank">nvim\_\_id_array(arr)</a>                                                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim__id_dictionary()" target="_blank">nvim\_\_id_dictionary(dct)</a>                                      |             |
| <a href="https://neovim.io/doc/user/api.html#nvim__id_float()" target="_blank">nvim\_\_id_float(flt)</a>                                                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim__inspect_cell()" target="_blank">nvim\_\_inspect_cell(grid, row, col)</a>                             |             |
| <a href="https://neovim.io/doc/user/api.html#nvim__stats()" target="_blank">nvim\_\_stats()</a>                                                         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_call_atomic()" target="_blank">nvim_call_atomic(calls)</a>                                            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_chan_send()" target="_blank">nvim_chan_send(chan, data)</a>                                           |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_create_buf()" target="_blank">nvim_create_buf(listed, scratch)</a>                                    |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_current_line()" target="_blank">nvim_del_current_line()</a>                                       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_keymap()" target="_blank">nvim_del_keymap(mode, lhs)</a>                                          |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_mark()" target="_blank">nvim_del_mark(name)</a>                                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_del_var()" target="_blank">nvim_del_var(name)</a>                                                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_echo()" target="_blank">nvim_echo(chunks, history, \*opts)</a>                                        |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_err_write()" target="_blank">nvim_err_write(str)</a>                                                  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_err_writeln()" target="_blank">nvim_err_writeln(str)</a>                                              |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_eval_statusline()" target="_blank">nvim_eval_statusline(str, \*opts)</a>                              |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_exec_lua()" target="_blank">nvim_exec_lua(code, args)</a>                                             |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_feedkeys()" target="_blank">nvim_feedkeys(keys, mode, escape_ks)</a>                                  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_api_info()" target="_blank">nvim_get_api_info()</a>                                               |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_chan_info()" target="_blank">nvim_get_chan_info(chan)</a>                                         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_context()" target="_blank">nvim_get_context(\*opts)</a>                                           |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_current_buf()" target="_blank">nvim_get_current_buf()</a>                                         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_current_tabpage()" target="_blank">nvim_get_current_tabpage()</a>                                 |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_current_win()" target="_blank">nvim_get_current_win()</a>                                         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_hl()" target="_blank">nvim_get_hl(ns_id, \*opts)</a>                                              |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_hl_id_by_name()" target="_blank">nvim_get_hl_id_by_name(name)</a>                                 |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_keymap()" target="_blank">nvim_get_keymap(mode)</a>                                               |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_mark()" target="_blank">nvim_get_mark(name, opts)</a>                                             |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_mode()" target="_blank">nvim_get_mode()</a>                                                       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_proc()" target="_blank">nvim_get_proc(pid)</a>                                                    |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_proc_children()" target="_blank">nvim_get_proc_children(pid)</a>                                  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_runtime_file()" target="_blank">nvim_get_runtime_file(name, all)</a>                              |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_var()" target="_blank">nvim_get_var(name)</a>                                                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_vvar()" target="_blank">nvim_get_vvar(name)</a>                                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_input()" target="_blank">nvim_input(keys)</a>                                                         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_input_mouse()" target="_blank">nvim_input_mouse(button, action, modifier, grid, row, col)</a>         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_list_bufs()" target="_blank">nvim_list_bufs()</a>                                                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_list_chans()" target="_blank">nvim_list_chans()</a>                                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_list_runtime_paths()" target="_blank">nvim_list_runtime_paths()</a>                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_list_tabpages()" target="_blank">nvim_list_tabpages()</a>                                             |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_list_uis()" target="_blank">nvim_list_uis()</a>                                                       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_list_wins()" target="_blank">nvim_list_wins()</a>                                                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_load_context(dict)" target="_blank">nvim_load_context(dict)</a>                                       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_notify()" target="_blank">nvim_notify(msg, log_level, opts)</a>                                       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_open_term()" target="_blank">nvim_open_term(buffer, opts)</a>                                         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_out_write()" target="_blank">nvim_out_write(str)</a>                                                  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_paste()" target="_blank">nvim_paste(data, crlf, phase)</a>                                            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_put()" target="_blank">nvim_put(lines, type, after, follow)</a>                                       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_replace_termcodes()" target="_blank">nvim_replace_termcodes(str, from_part, do_lt)</a>                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_select_popupmenu_item()" target="_blank">nvim_select_popupmenu_item(item, insert, finish, opts)</a>   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_client_info()" target="_blank">nvim_set_client_info(name, version, type, methods, attributes)</a> |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_current_buf()" target="_blank">nvim_set_current_buf(buffer)</a>                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_current_line()" target="_blank">nvim_set_current_line(line)</a>                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_current_tabpage()" target="_blank">nvim_set_current_tabpage(tabpage)</a>                          |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_current_win()" target="_blank">nvim_set_current_win(window)</a>                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_hl()" target="_blank">nvim_set_hl(ns_id, name, \*val)</a>                                         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_hl_ns()" target="_blank">nvim_set_hl_ns(ns_id)</a>                                                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_hl_ns_fast()" target="_blank">nvim_set_hl_ns_fast(ns_id)</a>                                      |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_keymap()" target="_blank">nvim_set_keymap(mode, lhs, rhs, \*opts)</a>                             |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_var()" target="_blank">nvim_set_var(name, value)</a>                                              |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_set_vvar()" target="_blank">nvim_set_vvar(name, value)</a>                                            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_strwidth()" target="_blank">nvim_strwidth(text)</a>                                                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_subscribe()" target="_blank">nvim_subscribe(event)</a>                                                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_unsubscribe()" target="_blank">nvim_unsubscribe(event)</a>                                            |             |

### Options Functions

| Function                                                                                                                             | Description |
| ------------------------------------------------------------------------------------------------------------------------------------ | ----------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_get_option()" target="_blank">nvim_buf_get_option(buffer, name)</a>            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_buf_set_option()" target="_blank">nvim_buf_set_option(buffer, name, value)</a>     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_all_options_info()" target="_blank">nvim_get_all_options_info()</a>            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_option()" target="_blank">nvim_get_option(name)</a>                            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_option_info2()" target="_blank">nvim_get_option_info2(name, \*opts)</a>        |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_get_option_value()" target="_blank">nvim_get_option_value(name, value, \*opts)</a> |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_option()" target="_blank">nvim_win_get_option(window, name)</a>            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_set_option()" target="_blank">nvim_win_set_option(window, name, value)</a>     |             |

### Tabpage Functions

| Function | Description |
| -----------------------------------------------------------------------------------------------------
| <a href="https://neovim.io/doc/user/api.html#nvim_tabpage_del_var()" target="_blank">nvim_tabpage_del_var(tabpage, name)</a> | |
| <a href="https://neovim.io/doc/user/api.html#nvim_tabpage_get_number()" target="_blank">nvim_tabpage_get_number(tabpage)</a> | |
| <a href="https://neovim.io/doc/user/api.html#nvim_tabpage_get_var()" target="_blank">nvim_tabpage_get_var(tabpage, name)</a> | |
| <a href="https://neovim.io/doc/user/api.html#nvim_tabpage_get_win()" target="_blank">nvim_tabpage_get_win(tabpage)</a> | |
| <a href="https://neovim.io/doc/user/api.html#nvim_tabpage_is_valid()" target="_blank">nvim_tabpage_is_valid(tabpage)</a> | |
| <a href="https://neovim.io/doc/user/api.html#nvim_tabpage_list_wins()" target="_blank">nvim_tabpage_list_wins(tabpage)</a> | |
| <a href="https://neovim.io/doc/user/api.html#nvim_tabpage_set_var()" target="_blank">nvim_tabpage_set_var(tabpage, name, value)</a> | |

### UI Functions

"pum" is an acronym for "popup menu".

| Function                                                                                                                                   | Description |
| ------------------------------------------------------------------------------------------------------------------------------------------ | ----------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_atttach()" target="_blank">nvim_ui_atttach(width, height, options)</a>                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_detach()" target="_blank">nvim_ui_detach()</a>                                        |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_pum_set_bounds()" target="_blank">nvim_ui_pum_set_bounds(width, height, row, col)</a> |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_pum_set_height()" target="_blank">nvim_ui_pum_set_height(height)</a>                  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_set_focus()" target="_blank">nvim_ui_set_focus(gained)</a>                            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_set_option()" target="_blank">nvim_ui_set_option(name, value)</a>                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_try_resize()" target="_blank">nvim_ui_try_resize(width, height)</a>                   |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_ui_try_resize_grid()" target="_blank">nvim_ui_try_resize_grid(grid, width, height)</a>   |             |

### Vimscript Functions

| Function                                                                                                                                | Description |
| --------------------------------------------------------------------------------------------------------------------------------------- | ----------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_call_dict_function()" target="_blank">nvim_call_dict_function(dict, fn, args)</a>     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_call_function()" target="_blank">nvim_call_function(fn, args)</a>                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_command()" target="_blank">nvim_command(command)</a>                                  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_eval()" target="_blank">nvim_eval(expr)</a>                                           |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_exec2()" target="_blank">nvim_exec2(src, \*opts)</a>                                  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_parse_expression()" target="_blank">nvim_parse_expression(expr, flags, highlight)</a> |             |

### Window Functions

| Function                                                                                                                    | Description |
| --------------------------------------------------------------------------------------------------------------------------- | ----------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_call()" target="_blank">nvim_win_call(window, fun)</a>                |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_close()" target="_blank">nvim_win_close(window, force)</a>            |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_del_var()" target="_blank">nvim_win_del_var(window, name)</a>         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_buf()" target="_blank">nvim_win_get_buf(window)</a>               |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_cursor()" target="_blank">nvim_win_get_cursor(window)</a>         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_height()" target="_blank">nvim_win_get_height(window)</a>         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_position()" target="_blank">nvim_win_get_position(window)</a>     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_tabpage()" target="_blank">nvim_win_get_tabpage(window)</a>       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_var()" target="_blank">nvim_win_get_var(window, name)</a>         |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_width()" target="_blank">nvim_win_get_width(window)</a>           |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_hide()" target="_blank">nvim_win_hide(window)</a>                     |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_is_valid()" target="_blank">nvim_win_is_valid(window)</a>             |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_set_buf()" target="_blank">nvim_win_set_buf(window, buffer)</a>       |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_set_cursor()" target="_blank">nvim_win_set_cursor(window, pos)</a>    |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_set_height()" target="_blank">nvim_win_set_height(window, height)</a> |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_set_hl_ns()" target="_blank">nvim_win_set_hl_ns(window, ns_id)</a>    |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_set_var()" target="_blank">nvim_win_set_var(window, name, value)</a>  |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_set_width()" target="_blank">nvim_win_set_width(window, width)</a>    |             |

### Win_Config Functions

| Function                                                                                                                      | Description |
| ----------------------------------------------------------------------------------------------------------------------------- | ----------- |
| <a href="https://neovim.io/doc/user/api.html#nvim_open_win()" target="_blank">nvim_open_win(buffer, enter, \*config)</a>      |             |
| <a href="https://neovim.io/doc/user/api.html#nvim_win_get_config()" target="_blank">nvim_win_get_config(window, \*config)</a> |             |
