---
eleventyNavigation:
  key: Neovim
layout: topic-layout.njk
---

## Overview

<img alt="Neovim logo" style="width: 40%"
    src="/blog/assets/neovim.png?v={{pkg.version}}"
    title="Neovim logo">

{% aTargetBlank "https://neovim.io", "Neovim" %}, or nvim for short,
is a free, open source, modern fork of the
{% aTargetBlank "https://www.vim.org", "Vim" %} text editor.
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

## Functionality Needed

Vanilla Neovim lacks many features that are frequently desired.
There are plugins available to add all the important features.
Some of the most popular plugins for Neovim
are summarized in the table below.

| Feature             | Popular Plugins                             |
| ------------------- | ------------------------------------------- |
| auto pairs          | nvim-autopairs                              |
| better navigation   | Hop                                         |
| better status line  | heirline, lualine                           |
| code formatting     | null-ls.nvim                                |
| color themes        | many; want Tree-sitter support              |
| completions         | cmp-buffer, cmp-luasnip, cmp-path, nvim-cmp |
| commenting          | Comment.nvim                                |
| debugger            | nvim-dap, nvim-dap-ui                       |
| file explorer       | neo-tree.nvim, nvim-tree.lua                |
| fuzzy finder        | telescope.nvim, telescope-fzf-native.nvim   |
| Git support         | fugitive.vim, gitsigns.nvim                 |
| icons               | nvim-web-devicons                           |
| keymap display      | which-key.nvim                              |
| linting             | null-ls.nvim                                |
| LSP client          | nvim-lspconfig                              |
| LSP servers         | LSP Zero                                    |
| notifications       | nvim-notify                                 |
| package manager     | mason.nvim                                  |
| plugin manager      | lazy.nvim, packer.nvim                      |
| snippets            | LuaSnip, friendly-snippets                  |
| split panes         | smart-splits.nvim                           |
| syntax highlighting | nvim-treesitter, nvim-colorizer.lua         |
| syntax parsing      | nvim-treesitter                             |
| terminal            | toggleterm.nvim                             |

Configuring all of these is a daunting task.
For these reason, prebuilt Neovim configurations are popular.
There are many to choose from, but the most popular seem to be:

- {% aTargetBlank "https://astronvim.com", "AstroNvim" %},
- {% aTargetBlank "https://www.lunarvim.org", "LunarVim" %}, and
- {% aTargetBlank "https://github.com/NvChad/NvChad", "NvChad" %}.

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

## Packer

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

By default Packer stores downloaded plugins in
`~/.local/share/nvim/site/pack/packer/start`.
The local Git repository for each installed plugin
is a directory in the `start` directory.
To see the version of a plugin that was installed,
TODO: Look at what?

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

As of April 2023, no LSPs for Swift appear in the available list.
{% aTargetBlank "https://github.com/apple/sourcekit-lsp", "SourceKit-LSP" %}
is an LSP for Swift, but it does not appear to work with Neovim yet.
The documentation {% aTargetBlank
"https://github.com/apple/sourcekit-lsp/blob/main/Editors/README.md", "here" %}
claims that SourceKit-LSP can be configured to work with Neovim,
but it does not work for me.

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

## Tree-sitter

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
A common motivation for installing Tree-sitter is to get the best
syntax highlighting from a theme that is compatible with Tree-sitter.

To install and configure Tree-sitter:

1. Add the following in `~/.config/nvim/lua/plugins.lua`:

   ```lua
   use("nvim-treesitter/nvim-treesitter", {run = ":TSUpdate"})
   ```

1. Enter `:PackerSync`

1. Create the file `~/.config/nvim/after/plugin/treesitter.lua`
   containing the following:

   ```lua
   require "nvim-treesitter.configs".setup {
     -- A list of parser names, or "all" (the five listed parsers should always be installed)
     ensure_installed = { "c", "javascript", "lua", "query", "rust", "swift", "typescript", "vim", "vimdoc" },

     -- Install parsers synchronously (only applied to `ensure_installed`)
     sync_install = false,

     -- Automatically install missing parsers when entering buffer
     -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
     auto_install = true,

     -- List of parsers to ignore installing (for "all")
     ignore_install = { },

     highlight = {
       enable = true

       -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
       -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
       -- Using this option may slow down your editor, and you may see some duplicate highlights.
       -- Instead of true it can also be a list of languages
       additional_vim_regex_highlighting = false,
     },
   }
   ```

1. Enter `:so` to source this file.

## LuaSnips

{% aTargetBlank "https://github.com/L3MON4D3/LuaSnip", "LuaSnips" %}
supports text snippets in Neovim.

TODO: Document how to install, configure, and use this.

## Premade Configurations

The three most popular Neovim premade configurations are
{% aTargetBlank "https://astronvim.com", "AstroNvim" %},
{% aTargetBlank "https://www.lunarvim.org", "LunarVim" %}, and
{% aTargetBlank "https://github.com/NvChad/NvChad", "NvChad" %}.

## AstroNvim

{% aTargetBlank "https://astronvim.com", "AstroNvim" %} is
"an aesthetic and feature-rich neovim config
that is extensible and easy to use with a great set of plugins".

For a list of plugins used by AstroNvim by default, see {% aTargetBlank
"https://astronvim.com/acknowledgements", "Acknowledgements" %}.

There are several optional commands that
AstroNvim will use if they are installed.
These include:

- {% aTargetBlank "https://github.com/BurntSushi/ripgrep", "ripgrep" %} - for live grep Telescope search (<leader>fw)
- {% aTargetBlank "https://github.com/jesseduffield/lazygit", "lazygit" %} - Git UI (<leader>tl or <leader>gg)
- {% aTargetBlank "https://github.com/dundee/gdu", "gdu" %} - disk usage (<leader>tu)
- {% aTargetBlank "https://github.com/ClementTsang/bottom", "bottom" %}- process viewer (<leader>tt)
- {% aTargetBlank "https://www.python.org", "Python" %}- for the Python REPL (<leader>tp)
- {% aTargetBlank "https://nodejs.org/en", "Node" %}- needed by many LSPs and for the Node REPL (<leader>tn)

### Installing

To install
{% aTargetBlank "https://github.com/AstroNvim/AstroNvim", "AstroNvim" %},

1. Make a backup copy of your `~/.config/nvim` directory if you have one.
1. `cd` to your `~/.config/nvim` directory.
1. Delete all the files and directories inside it.
1. Enter `git clone --depth 1 https://github.com/AstroNvim/AstroNvim ~/.config/nvim`
1. Enter `brew install lua-language-server`.
1. Enter `nvim`. On first launch this will install many things.
1. Install language parsers by enter `:TSInstall {language-name}`
   for each language.
   For example, use the language names "javascript", "lua", and "swift".
1. Enter `:Lazy sync` to update plugins and remove unused plugins.
1. Enter `:AstroUpdatePackages` to get the latest AstroNvim updates.

To check the status of your installation, enter `:checkhealth`.

### Font

Several parts of AstroNvim attempt to display icons.
This requires using a
{% aTargetBlank "https://www.nerdfonts.com/", "Nerd font" %}.
One that I recommend is "Caskaydia Cove Nerd Font" which is very similar to
the non-Nerd font {% aTargetBlank "https://github.com/microsoft/cascadia-code",
"Cascadia Code" %} from Microsoft.

### Configuration

AstroNvim configuration files are in `~/.config/nvim`.
The main configuration file is `init.lua`.
The `lua` subdirectory contains the directories `astronvim` and `plugins`.

The `astronvim` directory contains:

- `autocmds.lua`
- `bootstrap.lua`
- `health.lua`
- `lazy.lua`
- `mappings.lua` - defines key mappings
- `options.lua`

The `plugins` directory contains a separate config `.lua` file for each plugin.
Initially this includes:

- `alpha.lua`
- `cmp.lua`
- `core.lua`
- `dap.lua`
- `git.lua`
- `heirline.lua`
- `lsp.lua`
- `mason.lua`
- `neo-tree.lua`
- `telescope.lua`
- `treesitter.lua`
- `ui.lua`

{% aTargetBlank "https://github.com/jesseduffield/lazygit#homebrew",
"lazygit" %} is a terminal UI for Git commands.
To install it, enter `brew install lazygit`.

{% aTargetBlank "https://clementtsang.github.io/bottom/", "bottom" %}
is a "graphical process/system monitor for the terminal".
To install it, enter `brew install bottom`.
This installs the command `btm`.

For the best Ruby support, enter `gem install neovim`.

For the best Node.js support, enter `npm install -g neovim`.

For better snippet support,
modify `~/.config/nvim/lua/plugins/config/luasnip.lua` to match the following:

```lua
return function(_, opts)
  local ls = require("luasnip")

  ls.config.set_config({
    history = true,
    updateevents = "TextChanged,TextChangedI"
  })

  if opts then ls.config.setup(opts) end

  vim.tbl_map(
    function(type)
      require("luasnip.loaders.from_" .. type).lazy_load()
    end,
    { "vscode", "snipmate", "lua" }
  )
end
```

### Basics

The {% aTargetBlank "https://astronvim.com/Basic%20Usage/mappings",
"Default Mappings" %} page lists all the key mappings
that AstroNvim provides by default.

The leader key defaults to space.

### File Explorer

Press <leader>e to open the file explorer.
Once in it, press `?' to see the default key mappings.

Navigate down and up with `j` and `k`.
To open a selected directory or file, press the return key.

### Splits

The editing area display multiple buffers
that are displayed in multiple split panes.

To create a horizontal split, enter `<leader>/`.

To create a vertical split, enter `<leader>|`.

To close a split, enter `:clo` or `<leader>c`.
TODO: `<leader>c` does not work, but `<leader>q` does.

To move between splits, enter `ctrl-h` (left),
`ctrl-j` (down), `ctrl-k` (up), and `ctrl-l` (right).

### Find Commands

To find files using the fuzzy finder Telescope:

- Press `<leader>fa` to find AstroNvim configuration files.
- Press `<leader>fb` to find buffers by name.
- Press `<leader>fc` to find files that contain the word under the cursor.
- Press `<leader>fC` to find Vim plugin commands.
- Press `<leader>ff` to find files by name.
- Press `<leader>fh` to find a help file by its name.
- Press `<leader>fk` to find key mappings.
- Press `<leader>fo` to find files opened recently (old files)
- Press `<leader>fr` to find Vim registers (can see their contents)
- Press `<leader>ft` to find a theme (can see previews and select one)
- Press `<leader>fw` to find files by a word in their content.
- Press `<leader>fW` to find files containing multiple consecutive words.

### Go To Commands

To go to the definition of the symbol under the cursor, press `gd`.

### Comments

To toggle commenting of the current line or selected lines, press `<leader>/`.

### Git Commands

To see all the keys mapped to Git commands, press `<leader>g` and pause.

To open a window that displays the Git status of the current project,
press `<leader>gt`. Press `esc` twice to close the window.

To list all the branches and optionally switch to one,
press `<leader>gb`.

To see a side-by-side diff for the current file, press `<leader>gd`.

To list all the commits for the current file, press `<leader>gc`.

To open a lazygit window, press `<leader>gg`.
To close a lazygit window, press `q`.
See the lazygit {% aTargetBlank
"https://github.com/jesseduffield/lazygit/blob/master/docs/keybindings/Keybindings_en.md",
"key bindings" %}.
For more information, watch this {% aTargetBlank
"https://www.youtube.com/watch?v=CPLdltN7wgE", "YouTube video" %}.

### Color Themes

To see all the installed color themes and select one,
enter `:colo` followed by a space and the tab key.
TODO: How do you move the selection to a different one in the list?
Selecting a theme that is compatible with Tree-sitter results in better syntax highlighting.

### Completions

To select a suggested completion from a provided list,
use ctrl-j and ctrl-k to move down and up
and press return to select the highlighted completion.

### Snippets

AstroNvim uses the LuaSnips plugin to support snippets.
Snippet suggests appear when the beginning of their trigger words are typed.
When a list of possible snippets appears,
press ctrl-j and ctrl-k to highlight one,
press enter to select it.
For snippets that have placeholders, type text into each one.
Press tab to jump to the next placeholder
and shift-tab to jump to the previous placeholder.
After entering text for the last placeholder,
press tab one more tab to move the end of the snippet and continue typing.

LuaSnip supports two syntaxes for defining snippets,
the VS Code style and the LuaSnips style.

To create custom snippets:

1. Create the directory `~/.config/nvim/lua/user/snippets`.

1. Create the file `~/.config/nvim/lua/user/init.lua` containing the following:

```lua
return {
  plugins = {
    {
      "L3MON4D3/LuaSnip",
      config = function(plugin, opts)
        require "plugins.configs.luasnip"(plugin, opts) -- include the default astronvim config that calls the setup call
        require("luasnip.loaders.from_vscode").lazy_load { paths = { "./lua/user/snippets" } } -- load snippets paths
      end,
    },
  },
}
```

1. For the VS Code style, create the file
   `~/.config/nvim/lua/user/snippets/package.json` containing the following:

   ```json
   {
     "name": "user snippets",
     "engines": {
       "vscode": "^1.11.0"
     },
     "contributes": {
       "snippets": [
         {
           "language": "javascript",
           "path": "./javascript.json"
         }
       ]
     }
   }
   ```

1. For the VS Code style, create a file like the following
   for each language that needs snippets.
   For JavaScript the file name should be `javascript.json`.

   ```json
   {
     "Log Entry": {
       "prefix": "loge",
       "body": ["console.log('$TM_FILENAME $1: entered');"],
       "description": "Log function entry"
     },
     "Log Variable": {
       "prefix": "logv",
       "body": ["console.log('$TM_FILENAME $1: $2 =', $2);"],
       "description": "Log variable to console"
     }
   }
   ```

   Using `$2` twice in the previous snippet is NOT WORKING
   like it does in VS Code! See {% aTargetBlank
   "https://github.com/L3MON4D3/LuaSnip/issues/857", "issue" %}.

TODO: Describe the LuaSnips syntax for defining snippets.

### Symbol List

To list symbols in a right pane, enter `<leader>lS`".
This lists symbols like variable, function, and type declarations.
Select a symbol name to scroll to it in the source file.
The list of symbols automatically updates
when focus moves to a different buffer.

## LunarVim

## NvChad

## Unorganized Content

You need periods (all operators?) to be white. Make your own theme or override specific things?

I need to change the syntax highlighting for . and : in Lua function calls
to be white because they are hard to see currently.
See <https://vi.stackexchange.com/questions/41763/syntax-highlighting-for-and-in-lua-function-calls>
