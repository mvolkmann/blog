---
eleventyNavigation:
  key: Neovim
layout: topic-layout.njk
---

## Overview

<img alt="Neovim logo" style="width: 40%"
    src="/blog/assets/neovim.png?v={{pkg.version}}"
    title="Neovim logo">

{% aTargetBlank "https://neovim.io", "Neovim" %} is a free, open source, modern
fork of the {% aTargetBlank "https://www.vim.org", "Vim" %} text editor.
It provides built-in Language Server Protocol (LSP) support,
asynchronous I/O, and support for Lua scripting.

The Neovim project started in 2014 and was officially released in November 2015.

The source for Neovim is in a public {% aTargetBlank
"https://github.com/neovim/neovim", "GitHub repository" %}.
As of April 2023 there were 963 contributors and
the code was 44% VimScript, 31% C, and 23% Lua.

## Installing

In macos, Neovim can be installed using Homebrew.
by entering `brew install neovim`.

If you are already in the habit of using Vim,
it's a good idea to add an alias from "vim" to "nvim"
in your shell configuration file.
For zsh, edit `~/.zshrc` and add `alias vim="nvim"`.

## Configuring

Neovim can be configured using Vimscript, the Lua programming language,
or both.

The easiest way to get started is to use {% aTargetBlank
"https://github.com/nvim-lua/kickstart.nvim", "kickstart.nvim" %}.
The following steps bypass this and configure Neovim in a more personalized way.

Create the file `~/.config/nvim/init.lua`.

Some suggested content for this file is the following.
This assumes the use of the {% aTargetBlank
"https://github.com/wbthomason/packer.nvim", "Packer" %} plugin manager.

```lua
-- TODO: Does this only look for "plugins.lua" in the "lua" subdirectory?
-- TODO: Can "lua/plugins.lua" be moved to the same directory as this file?
require "plugins"

-- TODO: Improve color of line numbers.
-- TODO: Configure cmd-s to save.

vim.g.mapleader = " " -- many people use comman instead of space

-- This causes the yank(copy) and delete(cut) commands to copy to the
-- system clipboard so the copied text can be pasted into another app.
vim.opt.clipboard = 'unnamed'

local indent = 2
vim.opt.shiftwidth=indent -- indent code with two spaces
vim.opt.softtabstop=indent -- tabs take two spaces
vim.opt.tabstop=indent -- tabs take two spaces

vim.opt.colorcolumn = "80" -- displays a vertical strip at column 80 (not 81)
vim.opt.expandtab = true -- replace tabs with spaces
vim.opt.hlsearch = true -- highlights all search matches, not just first
vim.opt.incsearch = true -- performs incremental searching.
vim.opt.number = true -- shows line numbers
vim.opt.relativenumber= true -- shows relative line numbers
vim.opt.shiftround = true --round indent to multiples of shiftwidth
vim.opt.smartindent = true -- pressing tab key in insert mode insert spaces
vim.opt.smarttab = true -- pressing tab key in insert mode insert spaces
vim.opt.termguicolors = true -- uses 24-bit colors
vim.opt.wrap = false -- prevents line wrapping at end of window or pane

-- Key mappings
-- These are supposed to map cmd-s to save, but I can't get them to work.
vim.keymap.set('n', '<D-s>', ":w<kEnter>")
vim.keymap.set('i', '<D-s', "<Esc>:w<kEnter>i")

-- This automatically runs the `:PackerCompile` command
-- every time the `~/.config/nvim/lua/plugins.lua` file is updated.
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])
```

To see the directories that Neovim searches for packages,
enter `:help rtp` or `:set packpath?`.
One directory should be `~/.config/nvim` and
another should be `!/.config/nvim/after`.

To check for configuration issues, enter `:checkhealth`.

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

Some popular Neovim plugins include:

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
  "lualine.nvim" %} for configuring the Neovim status line
- {% aTargetBlank "https://github.com/preservim/nerdcommenter",
  "NERD Commenter" %} to simplify entering code comments
- {% aTargetBlank "https://github.com/EdenEast/nightfox.nvim", "Nightfox" %}
  theme with support for LSP, Treesitter, and more
- {% aTargetBlank "https://github.com/nvim-tree/nvim-tree.lua",
  "nvim-tree.lua" %} file explorer for Neovim
- {% aTargetBlank "https://github.com/rose-pine/neovim",
  "Rosé Pine for Neovim" %} color theme
- {% aTargetBlank "https://github.com/nvim-telescope/telescope.nvim",
  "Telescope" %} fuzzy finder
- {% aTargetBlank "https://github.com/nvim-treesitter/nvim-treesitter",
  "nvim-treesitter" %} Neovim interface to {% aTargetBlank
  "https://github.com/tree-sitter/tree-sitter", "Treesitter" %}
  which is a parser generator tool and an incremental parsing library.

## Plugin Manager

The most popular plugin manager for Neovim is
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

- Describe the desired plugins in `~/config/nvim/lua/plugins.lua`
  by calling the `use` function for each.

- To install new plugins added in `plugins.lua`,
  update previously installed plugins,
  and delete plugins no longer described in `plugins.lua`,
  enter `:PackerSync`.

- To list the installed plugins, enter `:PackerStatus`.

## LSP Zero

The Language Server Protocol (LSP) was designed by Microsoft for use in VS Code.
It can be used by other text editors such as Neovim.

{% aTargetBlank "https://github.com/VonHeikemen/lsp-zero.nvim", "LSP Zero" %}
is a Neovim plugin that bundles all the code require to get
{% aTargetBlank "https://github.com/hrsh7th/nvim-cmp", "nvim-cmp" %}
(a popular autocompletion plugin) and
{% aTargetBlank "https://github.com/neovim/nvim-lspconfig", "nvim-lspconfig" %}
(configuration for the Nvim LSP client that is bundled with Neovim)
to work together.
It also uses {% aTargetBlank "https://github.com/williamboman/mason.nvim",
"mason.nvim" %} (another package manager)
to allow you to install language servers from inside Neovim.

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

   -- Configure the Lua language server for Neovim.
   require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

   lsp.ensure_installed {
     "eslint",
     "lua_ls",
     "tsserver"
   }

   lsp.setup()

   local cmp = require('cmp')
   local cmp_action = lsp.cmp_action()

   cmp.setup({
     -- This configures LSP key mappings.
     mapping = {
       -- This enables selecting a suggested completion by
       -- pressing the return key in addition to the default of ctrl-y.
       ['<cr>'] = cmp.mapping.confirm({select = true})
     }
   })
   ```

1. Enter `:so` to source the current file.

1. Enter `:PackerSync`

1. Enter ":Mason" to open a window that displays
   a list of the installed language servers and
   a list of the available language servers that have not been installed.

   By default, all known language servers are displayed
   regardless of their capabilities.
   To filter the lists based on capabilities,
   type a number from 1 to 5 where 1 shows All,
   2 shows only LSPs, 3 shows only DAPs (Debug Adapter Protocol),
   4 shows only Linters, and 5 shows only Formatters.

   - To toggle between showing help and showing lists of language servers,
     enter "g?".
   - To install a language server, move the cursor to its name and press "i".
   - To uninstall a language server, move the cursor to its name and press "X".
   - To close the Mason window, press esc.

To use LSP Zero, open any file whose file extension
maps to an install language server.
Completions will appear while typing.
To accept the first suggestion, press ctrl-y.
To accept another suggestion,
press the down arrow key to move to it and press ctrl-y.

### Key Bindings

LSP Zero defines the following key bindings that all being with the leader key:

- K: displays information about the symbol under the cursor in a floating window
- gd: jumps to the definition of the symbol under the cursor
- gD: jumps to the declaration of the symbol under the cursor
  (some servers don't implement this)
- gi: lists all implementations for the symbol under the cursor
  in the quickfix window
- go: jumps to the definition of the type of the symbol under the cursor
- gr: lists all references to the symbol under the cursor in the quickfix window
- gs: displays signature information about the symbol under the cursor
  in a floating window

- <F2>: renames all references to the symbol under the cursor
- <F3>: format code in current buffer
- <F4>: selects a code action available at the current cursor position

- gl: show diagnostics in a floating window
- [d: moves to the previous diagnostic in the current buffer
- ]d: moves to the next diagnostic in the current buffer

- <Ctrl-y>: confirms selection (TODO: Can this be configured to use the return key?)
- <Ctrl-e>: cancels completion
- <Down>: navigates to next suggested completion
- <Up>: navigates to previous suggested completion
- <Ctrl-n>: if completion menu is visible, go to next item; otherwise trigger completion menu
- <Ctrl-p>: if completion menu is visible, go to previous item; otherwise trigger completion menu
- <Ctrl-d>: scrolls downs the documentation window
- <Ctrl-u>: scrolls up the documentation window

From the docs,
"By default lsp-zero will not create a keybinding if it is taken.
You can force lsp-zero's bindings by adding ..."

```lua
lsp.default_keymaps({
  buffer = bufnr,
  preserve_mappings = false
})
```

## Lua Snip

{% aTargetBlank "https://github.com/L3MON4D3/LuaSnip", "LuaSnip" %} is a
snippet manager for Neovim.

To install it:

- Add the following in `~/.config/nvim/lua/plugins.lua`:

  ```lua
    use({
      "L3MON4D3/LuaSnip",
      -- use latest release.
      tag = "v<CurrentMajor>.*",
      -- install optional jsregexp package
      run = "make install_jsregexp"
    })
  ```

- Enter `:PluginSync`

- Create the file `~/.config/nvim/lua/after/plugin/luasnip.lua`
  with the following content:

  ```lua

  ```

## Telescope

{% aTargetBlank "https://github.com/nvim-telescope/telescope.nvim",
"Telescope" %} is a fuzzy finder for Neovim.

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
  local builtin = require "telescope.builtin"

  -- This finds buffers by their name.
  vim.keymap.set('n', '<leader>fb', builtin.buffers, {desc = '[F]ind [B]uffers'})

  -- This may not be particularly useful.
  vim.keymap.set('n', '<leader>fd', builtin.diagnostics, { desc = '[F]ind in [D]iagnostics' })

  -- This finds files by their name.
  vim.keymap.set('n', '<leader>ff', builtin.find_files, {desc = '[F]ind [F]iles'})

  -- This finds files by their content.
  vim.keymap.set('n', '<leader>fg', builtin.live_grep, {desc = '[F]ind by [G]rep'})

  -- This finds Git controlled files by their name.
  -- It avoids excluded files such as those under a `node_modules` directory.
  vim.keymap.set('n', '<leader>fG', builtin.git_files, {desc = '[F]ind in [G]it'})

  -- This finds help files by their content.
  vim.keymap.set('n', '<leader>fh', builtin.help_tags, {desc = '[F]ind in [H]elp'})

  -- This fines files whose content includes the word under the cursor.
  vim.keymap.set('n', '<leader>fw', builtin.grep_string, { desc = '[F]ind current [W]ord' })

  -- Project Search (from ThePrimeagen)
  vim.keymap.set('n', '<leader>ps', function()
    builtin.grep_string({search = vim.fn.input("Grep> ")})
  end)
  ```

## Treesitter

{% aTargetBlank "https://github.com/tree-sitter/tree-sitter", "Tree-sitter" %}
is a parser generator and incremental parser implemented in Rust and C
that can be embedded in applications like text editors.

Grammars can be supplied to enable Tree-sitter to
parse source files for any programming language.

Tree-sitter builds a syntax tree from a single source file
that can contain syntax errors.
It can then efficiently update the syntax tree when the source file is modified
without re-parsing the entire source file.
This allows it to run after every keystroke in a text editor.

The Neovim plugin {% aTargetBlank
"https://github.com/nvim-treesitter/nvim-treesitter", "nvim-treesitter" %}
integrates Tree-sitter with Neovim and provides functionality such as
syntax highlighting based on the tokens that Treesitter reports.

## Unorganized Content

You need periods (all operators?) to be white. Make your own theme or override specific things?
