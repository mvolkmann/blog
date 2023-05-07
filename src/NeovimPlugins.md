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

## Neovim API

The {% aTargetBlank "https://neovim.io/doc/user/api.html", "Neovim API" %}
provides functions in many categories.
Each of these are summarized in the following subsections.

### Events

### Autocmd Functions

| Function                           | Description |
| ---------------------------------- | ----------- |
| `nvim_clear_autocmds(*opts)`       |             |
| `nvim_create_augroup(name, *opts)` |             |
| `nvim_create_autocm(event, *opts)` |             |
| `nvim_del_augroup_by_id(id)`       |             |
| `nvim_del_augroup_by_name(name)`   |             |
| `nvim_del_autocmd(id)`             |             |
| `nvim_create_autocmd()`            |             |
| `nvim_get_autocmds(*opts)`         |             |

### Buffer Functions

| Function                                                                         | Description |
| -------------------------------------------------------------------------------- | ----------- |
| `nvim_buf_attach(buffer, send_buffer, opts)`                                     |             |
| `nvim_buf_call(buffer, fun)`                                                     |             |
| `nvim_buf_del_keymap(buffer, mode, lhs)`                                         |             |
| `nvim_buf_del_mark(buffer, name)`                                                |             |
| `nvim_buf_del_var(buffer, name)`                                                 |             |
| `nvim_buf_delete(buffer, opts)`                                                  |             |
| `nvim_buf_detach(buffer)`                                                        |             |
| `nvim_buf_get_changedtick(buffer)`                                               |             |
| `nvim_buf_get_keymap(buffer, mode)`                                              |             |
| `nvim_buf_get_lines(buffer, start, end, strick_indexing)`                        |             |
| `nvim_buf_get_mark(buffer, name)`                                                |             |
| `nvim_buf_get_name(buffer)`                                                      |             |
| `nvim_buf_get_offset(buffer, index)`                                             |             |
| `nvim_buf_get_text(buffer, start_row, start_col, end_row, end_col, opts)`        |             |
| `nvim_buf_get_var(buffer, name)`                                                 |             |
| `nvim_buf_is_loaded(buffer)`                                                     |             |
| `nvim_buf_is_valid(buffer)`                                                      |             |
| `nvim_buf_line_count(buffer)`                                                    |             |
| `nvim_buf_set_keymap(buffer, mode, lhs, rhs, *opt)`                              |             |
| `nvim_buf_set_lines(buffer, start, end, strict_indexing, replacement)`           |             |
| `nvim_buf_set_mark(buffer, name, line, col, opts)`                               |             |
| `nvim_buf_set_name(buffer, name)`                                                |             |
| `nvim_buf_set_text(buffer, start_row, start_col, end_row, end_col, replacement)` |             |
| `nvim_buf_set_var(buffer, name, value)`                                          |             |

### Command Functions

| Function                                         | Description |
| ------------------------------------------------ | ----------- |
| `nvim_buf_create_user_command(buffer, name)`     |             |
| `nvim_buf_del_user_command(buffer, name)`        |             |
| `nvim_buf_get_commands(buffer, *opts)`           |             |
| `nvim_cmd(*cmd, *opts)`                          |             |
| `nvim_create_user_command(name, command, *opts)` |             |
| `nvim_del_user_command(name)`                    |             |
| `nvim_get_commands(*opts)`                       |             |
| `nvim_parse_cmd(str, opts)`                      |             |

### Extmark Functions

| Function                                                                    | Description |
| --------------------------------------------------------------------------- | ----------- |
| `nvim_buf_add_highlight(buffer, ns_id, hl_group, line, col_start, col_end)` |             |
| `nvim_buf_clear_namespace(buffer, ns_id, line_start, line_end)`             |             |
| `nvim_buf_del_extmark(buffer, ns_id, id)`                                   |             |
| `nvim_buf_get_extmark_by_id(buffer, ns_id, id, opts)                        |             |
| `nvim_buf_set_extmark(buffer, ns_id, line, col, *opts)`                     |             |
| `nvim_create_namespace(name)`                                               |             |
| `nvim_get_namespaces()`                                                     |             |
| `nvim_set_decoration_provider(ns_id, *opts)`                                |             |

### Global Functions

| Function                                                         | Description |
| ---------------------------------------------------------------- | ----------- |
| `nvim__get_runtime(pat, all, *opts)`                             |             |
| `nvim__id(obj)`                                                  |             |
| `nvim__id_array(arr)`                                            |             |
| `nvim__id_dictionary(dct)`                                       |             |
| `nvim__id_float(flt)`                                            |             |
| `nvim__inspect_cell(grid, row, col)`                             |             |
| `nvim__stats()`                                                  |             |
| `nvim_call_atomic(calls)`                                        |             |
| `nvim_chan_send(chan, data)`                                     |             |
| `nvim_create_buf(listed, scratch)`                               |             |
| `nvim_del_current_line()`                                        |             |
| `nvim_del_keymap(mode, lhs)`                                     |             |
| `nvim_del_mark(name)`                                            |             |
| `nvim_del_var(name)`                                             |             |
| `nvim_echo(chunks, history, *opts)`                              |             |
| `nvim_err_write(str)`                                            |             |
| `nvim_err_writeln(str)`                                          |             |
| `nvim_eval_statusline(str, *opts)`                               |             |
| `nvim_exec_lua(code, args)`                                      |             |
| `nvim_feedkeys(keys, mode, escape_ks)`                           |             |
| `nvim_get_api_info()`                                            |             |
| `nvim_get_chan_info(chan)`                                       |             |
| `nvim_get_context(*opts)`                                        |             |
| `nvim_get_current_tabpage()`                                     |             |
| `nvim_get_current_win()`                                         |             |
| `nvim_get_hl(ns_id, *opts)`                                      |             |
| `nvim_get_hl_id_by_name(name)`                                   |             |
| `nvim_get_keymap(mode)`                                          |             |
| `nvim_get_mark(name, opts)`                                      |             |
| `nvim_get_mode()`                                                |             |
| `nvim_get_proc(pid)`                                             |             |
| `nvim_get_proc_children(pid)`                                    |             |
| `nvim_get_runtime_file(name, all)`                               |             |
| `nvim_get_var(name)`                                             |             |
| `nvim_get_vvar(name)`                                            |             |
| `nvim_input(keys)`                                               |             |
| `nvim_input_mouse(button, action, modifier, grid, row, col)`     |             |
| `nvim_list_bufs()`                                               |             |
| `nvim_list_chans()`                                              |             |
| `nvim_list_runtime_paths()`                                      |             |
| `nvim_list_tabpages()`                                           |             |
| `nvim_list_uis`                                                  |             |
| `nvim_list_wins`                                                 |             |
| `nvim_load_context(dict)`                                        |             |
| `nvim_notify(msg, log_level, opts)`                              |             |
| `nvim_open_term(buffer, opts)`                                   |             |
| `nvim_out_write(str)`                                            |             |
| `nvim_paste(data, crlf, phase)`                                  |             |
| `nvim_put(lines, type, after, follow)`                           |             |
| `nvim_replace_termcodes(str, from_part, do_lt)`                  |             |
| `nvim_select_popupmenu_item(item, insert, finish, opts)`         |             |
| `nvim_set_client_info(name, version, type, methods, attributes)` |             |
| `nvim_set_current_buf(buffer)`                                   |             |
| `nvim_set_current_line(line)`                                    |             |
| `nvim_set_current_tabpage(tabpage)`                              |             |
| `nvim_set_current_win(window)`                                   |             |
| `nvim_set_hl(ns_id, name, *val)`                                 |             |
| `nvim_set_hl_ns(ns_id)`                                          |             |
| `nvim_set_hl_ns_fast(ns_id)`                                     |             |
| `nvim_set_keymap(mode, lhs, rhs, *opts)`                         |             |
| `nvim_set_var(name, value)`                                      |             |
| `nvim_set_vvar(name, value)`                                     |             |
| `nvim_strwidth(text)`                                            |             |
| `nvim_subscribe(event)`                                          |             |
| `nvim_unsubscribe(event)`                                        |             |

### Options Functions

| Function                                    | Description |
| ------------------------------------------- | ----------- |
| `nvim_buf_get_option(buffer, name)`         |             |
| `nvim_buf_set_option(buffer, name, value)`  |             |
| `nvim_get_all_options_info()`               |             |
| `nvim_get_option(name)`                     |             |
| `nvim_get_option_info2(name, *opts)`        |             |
| `nvim_get_option_value(name, value, *opts)` |             |
| `nvim_win_get_option(window, name)`         |             |
| `nvim_win_set_option(window, name, value)`  |             |

### Tabpage Functions

| Function | Description |
| -------- | ----------- |
| ``       |             |

### UI Functions

"pum" is an acronym for "popup menu".

| Function                                          | Description |
| ------------------------------------------------- | ----------- |
| `nvim_ui_atttach(width, height, options)`         |             |
| `nvim_ui_detach()`                                |             |
| `nvim_ui_pum_set_bounds(width, height, row, col)` |             |
| `nvim_ui_pum_set_height(height)`                  |             |
| `nvim_ui_set_focus(gained)`                       |             |
| `nvim_ui_set_option(name, value)`                 |             |
| `nvim_ui_try_resize(width, height)`               |             |
| `nvim_ui_try_resize_grid(grid, width, height)`    |             |

### Vimscript Functions

| Function                                        | Description |
| ----------------------------------------------- | ----------- |
| `nvim_call_dict_function(dict, fn, args)`       |             |
| `nvim_call_function(fn, args)`                  |             |
| `nvim_command(command)`                         |             |
| `nvim_eval(expr)`                               |             |
| `nvim_exec2(src, *opts)`                        |             |
| `nvim_parse_expression(expr, flags, highlight)` |             |

### Window Functions

| Function | Description |
| -------- | ----------- |
| ``       |             |
