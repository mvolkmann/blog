---
eleventyNavigation:
  key: neovim
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://neovim.io", "Neovim" %} is a modern rewrite of
the Vim text editor.

## Installing

In macos, neovim can be installed using Homebrew.
by entering `brew install neovim`.

If you are already in the habit of using Vim,
it's a good idea to add an alias from "vim" to "nvim"
in your shell configuration file.
For zsh, edit `~/.zshrc` and add `alias vim="nvim"`.

## Configuring

Neovim is configured using Vimscript or the Lua programming language.

Create the file `~/.config/nvim/init.lua`.

Some suggested content for this file is the following:

```lua
require "plugins"

vim.g.mapleader = " "

vim.opt.relativenumber= true

-- This causes the yank command to copy to the system clipboard
-- so the copied text can be pasted into another app.
vim.opt.clipboard = 'unnamed'
```

To see the directories that neovim searches for packages,
enter `:help rtp` or `:set packpath?`.
One directory should be `~/.config/nvim` and
another should be `!/.config/nvim/after`.

## Creating Files and Directories

Enter `%` to create a new file.
You will be prompted for the name.

Enter `d` to create a new directory.
You will be prompted for the name.

## Sourcing Files

When editing a configuration file, to execute it enter `:source` or `:so`.
This is useful after making changes to configuration files
so you don't need to exit and restart nvim
in order for the changes to take effect.

## Plugin Manager

The most popular plugin manager for neovim is
{% aTargetBlank "https://github.com/wbthomason/packer.nvim", "packer" %}.
To install this:

- Copy a `git clone` command from the link above.
- Paste that command into a terminal window and execute it.
- Create the directory `~/.config/nvim/lua`.
- Create the file `plugins.lua` in that directory containing the following:

  ```lua
  return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'
  end)
  ```

- Edit `~/.config/nvim/init.lua`.

  - Add `require "plugins"`.
  - Add the following to cause the `:PackerCompile` command
    to run automatically when `plugins.lua` is updated.

    ```
    vim.cmd([[
      augroup packer_user_config
        autocmd!
        autocmd BufWritePost plugins.lua source <afile> | PackerCompile
      augroup end
    ]])
    ```

- To manually update Packer, enter `:PackerSync`.

## Telescope

{% aTargetBlank "https://github.com/nvim-telescope/telescope.nvim",
"Telescope" %} is a fuzzy finder for neovim.

To install it:

- Add the following in `~/.config/nvim/lua/plugins.lua`:

  ```lua
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  ```

- Enter `:PluginSync`

- Create the directory `~/.config/nvim/after/plugin`.
- Create the file `telescope.lua` in that directory with the following content:

  ```lua
  local builtin = require('telescope.builtin')
  vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
  vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
  vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
  vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
  ```

## Treesitter
