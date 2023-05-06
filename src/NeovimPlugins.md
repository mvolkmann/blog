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
