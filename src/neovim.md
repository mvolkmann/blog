---
eleventyNavigation:
  key: neovim
layout: topic-layout.njk
---

## Overview

<img alt="neovim logo" style="width: 40%"
    src="/blog/assets/neovim.png?v={{pkg.version}}"
    title="neovim logo">

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
TODO: Copy more from your init.lua file!

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

## Plugins

Some popular neovim plugins include:

- {% aTargetBlank "https://github.com/dense-analysis/ale",
  "Asynchronous Lint Engine" %} (ALE)
- {% aTargetBlank "https://github.com/sindrets/diffview.nvim",
  "diffview.nvim" %} for cycling through git diffs
- {% aTargetBlank "https://github.com/tpope/vim-fugitive", "fugitive.vim" %}
  for Git integration
- {% aTargetBlank "https://github.com/phaazon/hop.nvim", "Hop" %}
  EasyMotion-like plugin for jumping to
  anywhere in a file with minimal keystrokes.
- {% aTargetBlank "https://github.com/VonHeikemen/lsp-zero.nvim", "LSP Zero" %}
- {% aTargetBlank "https://github.com/nvim-lualine/lualine.nvim",
  "lualine.nvim" %} for configuring the neovim status line
- {% aTargetBlank "https://github.com/preservim/nerdcommenter",
  "NERD Commenter" %} to simplify entering code comments
- {% aTargetBlank "https://github.com/EdenEast/nightfox.nvim", "Nightfox" %}
  theme with support for LSP, Treesitter, and more
- {% aTargetBlank "https://github.com/nvim-tree/nvim-tree.lua",
  "nvim-tree.lua" %} file explorer for neovim
- {% aTargetBlank "https://github.com/rose-pine/neovim",
  "Rosé Pine for Neovim" %} color theme
- {% aTargetBlank "https://github.com/nvim-telescope/telescope.nvim",
  "Telescope" %} fuzzy finder
- {% aTargetBlank "https://github.com/nvim-treesitter/nvim-treesitter",
  "nvim-treesitter" %} neovim interface to {% aTargetBlank
  "https://github.com/tree-sitter/tree-sitter", "Treesitter" %}
  which is a parser generator tool and an incremental parsing library.

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

## LSP Zero

{% aTargetBlank "https://github.com/VonHeikemen/lsp-zero.nvim", "LSP Zero" %}
bundles all the code require to get
{% aTargetBlank "https://github.com/hrsh7th/nvim-cmp", "nvim-cmp" %}
(a popular autocompletion plugin) and
{% aTargetBlank "https://github.com/neovim/nvim-lspconfig", "nvim-lspconfig" %}
(configuration for the Nvim LSP client that is bundled with neovim)
to work together.
It also uses {% aTargetBlank "https://github.com/williamboman/mason.nvim",
"mason.nvim" %} (another package manager)
to allow you to install language servers from inside neovim.

To install and configure LSP Zero:

1. Add the following in `~/config/nvim/lua/plugins.lua`:

   ```lua
   use {
     'VonHeikemen/lsp-zero.nvim',
     branch = 'v2.x',
     requires = {
       -- LSP Support
       {'neovim/nvim-lspconfig'},             -- Required
       {                                      -- Optional
         'williamboman/mason.nvim',
         run = function()
           pcall(vim.cmd, 'MasonUpdate')
         end,
       },
       {'williamboman/mason-lspconfig.nvim'}, -- Optional

       -- Autocompletion
       {'hrsh7th/nvim-cmp'},     -- Required
       {'hrsh7th/cmp-nvim-lsp'}, -- Required
       {'L3MON4D3/LuaSnip'},     -- Required
     }
   }
   ```

1. Create the file `~/.config/nvim/after/plugin/lsp.lua`
   containing the following:

   ```lua
   local lsp = require('lsp-zero')

   lsp.preset("recommended")

   lsp.on_attach(function(client, bufnr)
      lsp.default_keymaps({buffer = bufnr})
   end)

   -- Configure the Lua language server for neovim.
   require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

   lsp.ensure_installed {
     "eslint",
     "lua_ls",
     "tsserver"
   }

   lsp.setup()
   ```

1. Enter `:so` to source the current file.

1. Enter `:PackerSync`

1. To install additional language servers, enter ":Mason".
   This displays a long list of available language servers.
   To install one, move the cursor to its name and press "i".

To use LSP Zero, open any file whose file extension
maps to an install language server.
Completions will appear while typing.
To select a completion, ...

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
