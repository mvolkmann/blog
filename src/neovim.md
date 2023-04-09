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

| Feature             | Popular Plugins                                                                                               |
| ------------------- | ------------------------------------------------------------------------------------------------------------- |
| auto pairs          | nvim-autopairs                                                                                                |
| better navigation   | {% aTargetBlank "https://github.com/phaazon/hop.nvim", "Hop" %}                                               |
| better status line  | heirline, lualine                                                                                             |
| code formatting     | null-ls.nvim                                                                                                  |
| color themes        | many; want Tree-sitter support                                                                                |
| completions         | cmp-buffer, cmp-luasnip, cmp-path, nvim-cmp                                                                   |
| commenting          | Comment.nvim                                                                                                  |
| debugger            | nvim-dap, nvim-dap-ui                                                                                         |
| file explorer       | neo-tree.nvim, nvim-tree.lua                                                                                  |
| fuzzy finder        | {% aTargetBlank "https://github.com/nvim-telescope/telescope.nvim", "Telescope" %}, telescope-fzf-native.nvim |
| Git support         | {% aTargetBlank "https://github.com/tpope/vim-fugitive", "fugitive.vim" %}, gitsigns.nvim                     |
| icons               | nvim-web-devicons                                                                                             |
| keymap display      | which-key.nvim                                                                                                |
| linting             | null-ls.nvim                                                                                                  |
| LSP client          | nvim-lspconfig                                                                                                |
| LSP servers         | LSP Zero                                                                                                      |
| notifications       | nvim-notify                                                                                                   |
| package manager     | mason.nvim                                                                                                    |
| plugin manager      | lazy.nvim, packer.nvim                                                                                        |
| snippets            | LuaSnip, friendly-snippets                                                                                    |
| split panes         | smart-splits.nvim                                                                                             |
| syntax highlighting | nvim-treesitter, nvim-colorizer.lua                                                                           |
| syntax parsing      | nvim-treesitter                                                                                               |
| terminal            | toggleterm.nvim                                                                                               |

Configuring all of these is a daunting task.
For these reason, prebuilt Neovim configurations are popular.
There are many to choose from, but the most popular seem to be:

- {% aTargetBlank "https://astronvim.com", "AstroNvim" %},
- {% aTargetBlank "https://www.lunarvim.org", "LunarVim" %}, and
- {% aTargetBlank "https://github.com/NvChad/NvChad", "NvChad" %}.

## Configuring

Neovim can be configured using Vimscript, the Lua programming language,
or both.

Create the file `~/.config/nvim/init.lua`
and add content similar to the following.

```lua
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
-- TODO: These are supposed to map cmd-s to save, but I can't get them to work.
vim.keymap.set('n', '<D-s>', ":w<kEnter>")
vim.keymap.set('i', '<D-s', "<Esc>:w<kEnter>i")
vim.keymap.set('n', '<80><fd>hs', ":w<kEnter>")
vim.keymap.set('i', '<80><fd>hs', "<Esc>:w<kEnter>i")
```

## Sourcing Files

After editing a configuration file, enter `:source` or `:so` to execute it.
This is useful after making changes to configuration files
so you don't need to exit and restart nvim
in order for the changes to take effect.

## Folds

Code folding is a feature of Vim that is also present in Neovim.

- `zm` - fold more (close)
- `zr` - fold less (open)
- `zM` - close all folds
- `zR` - open all folds
- `zp` - peek at folded lines; next keystroke hides again

## Lua Functions

To run a Lua function exposed by a plugin,
enter `:{function-name}({arguments})`.

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

The configuration file for Tree-sitter is
`~/.config/nvim/lua/plugins/treesitter.lua`.
In the `opts` table, add a line like the following
to ensure support for specified languages is installed:

```lua
    ensure_installed = { "javascript", "lua", "typescript" },
```

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

1. Install Neovim. In macOS enter `brew install neovim`.
1. Make a backup copy of your `~/.config/nvim` directory if you have one.
1. `cd` to your `~/.config/nvim` directory.
1. Delete all the files and directories inside it.
1. Enter `git clone --depth 1 https://github.com/AstroNvim/AstroNvim ~/.config/nvim`
1. Enter `brew install lua-language-server`.
1. Enter `nvim`. On first launch this will install many things.
1. Install language parsers by entering `:TSInstall {language-name}`
   for each language.
   For example, use the language names "javascript", "lua", and "swift".
1. Install LSP servers by entering `:LspInstall {server-name}`
   for each server.
   For example, use the server names "eslint", "tsserver", and "lua_ls".
1. Enter `NullLsInstall prettier`.
1. Optionally enter `DapInstall {debug-adapter}`
   for each language-specific Debug Adapter Protocol server.
   (I could not find any of these.)
1. Enter `:Lazy sync` to update plugins and remove unused plugins.
1. Enter `:AstroUpdatePackages` to get the latest AstroNvim updates.
1. If JavaScript will be edited then
   it is likely that Babel will be used parse files.
   This requires setting the environment variable `NODE_ENV`.
   When using zsh, add `export NODE_ENV=development` in `~/.zshrc`.

To check the status of your installation, enter `:checkhealth`.

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

### Key Mappings

The default key mappings provided by AstroNvim are described
{% aTargetBlank "https://astronvim.com/Basic%20Usage/mappings", "here" %}.
Most of these are defined in `~/.config/nvim/lua/astronvim/mappings.lua`

### Font

Several parts of AstroNvim attempt to display icons.
This requires using a
{% aTargetBlank "https://www.nerdfonts.com/", "Nerd font" %}.
One that I recommend is "Caskaydia Cove Nerd Font" which is very similar to
the non-Nerd font {% aTargetBlank "https://github.com/microsoft/cascadia-code",
"Cascadia Code" %} from Microsoft.

### More Configuration

TODO: Should you cover all of these in the sections that follow?

| better navigation | Hop |
| code formatting | null-ls.nvim |
| color themes | many; want Tree-sitter support |
| completions | cmp-buffer, cmp-luasnip, cmp-path, nvim-cmp |
| commenting | Comment.nvim |
| debugger | nvim-dap, nvim-dap-ui |
| file explorer | neo-tree.nvim, nvim-tree.lua |
| keymap display | which-key.nvim |
| linting | null-ls.nvim |
| notifications | nvim-notify |
| package manager | mason.nvim |
| plugin manager | lazy.nvim, packer.nvim |
| snippets | LuaSnip, friendly-snippets |
| split panes | smart-splits.nvim |
| syntax highlighting | nvim-treesitter, nvim-colorizer.lua |
| syntax parsing | nvim-treesitter |
| terminal | toggleterm.nvim |

### Basics

The {% aTargetBlank "https://astronvim.com/Basic%20Usage/mappings",
"Default Mappings" %} page lists all the key mappings
that AstroNvim provides by default.

The leader key defaults to space.

Press `<leader>h` to open the AstroNvim home screen.
This displays a menu of common commands that includes:

- "New File"

  This creates a new, unnamed file.

- "Find File"

  This opens a window where text can be entered to
  find files that contain it in their name.
  Move the cursor to one of the matching files
  and press the return key to open it.

- "Recents"

  This opens a list of recently opened files
  and makes it easy to reopen one of them.

- "Find Word"

  This opens a window where words can be entered to find files that contain them.
  Move the cursor to one of the matching files
  and press the return key to open it.

- "Bookmarks"

  TODO: Learn how to create bookmarks!

- "Last Session".

  TODO: Learn why this is useful and how to return to the current session.

### File Explorer

Press <leader>e to open the file explorer.
Once open, press `?` to see the default key mappings.
Press ctrl-j and ctrl-k to navigate down and up to select a file or directory.

Some of the useful key mappings include:

- # to perform fuzzy filtering
- `a` to add a new file or directory
- `A` to add a new directory
- `H` to toggle display of hidden files (hidden by default)
- `P` to toggle preview display of selected file
- `S` to open selected file in a new horizontal split
- `s` to open selected file in a new vertical split
- `?` to see all file explorer key mappings
- `c` to copy selected file; prompts for new name
- `d` to delete selected file or directory
- `h` or return key to close an expanded directory
- `j` to move down to the next file or directory
- `k` to move up to the next file or directory
- `l` or return key to open the selected file or directory
- `m` to move selected file or directory; prompts for destination directory
- `o` or return key to open selected file or directory
- `r` to rename selected file or directory
- `t` to open selected file in a new tab (new set of files)
- `y` to copy selected file to clipboard
- `p` to paste file from clipboard into selected directory
- `<` to go to previous tab (File, Bufs, or Git)
- `>` to go to next tab (File, Bufs, or Git)

Navigate down and up with `j` and `k`.
To open a selected directory or file, press the return key.

### Splits

The editing area display multiple buffers
that are displayed in multiple split panes.

To create a horizontal split, enter `<leader>/`.

To create a vertical split, enter `<leader>|`.

To close a split, press ctrl-q or enter `:clo` or `<leader>c`.

To move between splits, enter `ctrl-h` (left),
`ctrl-j` (down), `ctrl-k` (up), and `ctrl-l` (right).

To resize the current split, press ctrl and
an arrow key to make it larger in that direction.
In macOS this will likely not work due to default key mappings in System Settings.
The following screenshot shows the key mappings that need to changed or
disabled to allow the AstroNvim default resize key mappings to work.

<img alt="AstroNvim smart-splits keys" style="width: 80%"
    src="/blog/assets/astronvim-smart-splits-keys.png?v={{pkg.version}}"
    title="AstroNvim smart-splits keys">

### Fuzzy Find Commands

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

### Telescope Commands

For symbols:

- Press `gd` to go to the definition of the symbol under the cursor.
- Press `gD` to go to the declaration of the symbol under the cursor.
  Some LSP servers don't implement this.
- Press `gi` to list all implementations for the symbol under the cursor
  in a quickfix window.
- Press `gI` to go to the implementation of the symbol under the cursor.
- Press `gr` to show references to the symbol under the cursor
  in a quickfix window.
  TODO: This doesn't seem to work!
- Press `gs` to display signature information about the symbol under the cursor
  in a floating window.
- Press `gT` to go to the type definition of the symbol under the cursor.
- Press `K` to see the type of the symbol under the cursor.
- Press `ctrl-o` to move backwards through results.

For diagnostic messages:

- Press `gl` to see a full error message when an error is displayed.
- Press `[d` to move to the previous diagnostic in the current buffer.
- Press `]d` to move to the next diagnostic in the current buffer.

For file paths:

- Press `gf` to open the file under the cursor in Neovim.
- Press `gx` to open the file under the cursor in the associated app.

### Comments

To toggle commenting of the current line or selected lines, press `<leader>/`.

### Auto-pairs

AstroNvim uses the {% aTargetBlank "https://github.com/windwp/nvim-autopairs",
"nvim-autopairs" %} plugin.
This automatically closes parentheses, sqaure brackets, and curly braces.
It is not enabled by default.
TODO: How can it be enabled?

### Status Line

AstroNvim uses the {% aTargetBlank "https://github.com/rebelot/heirline.nvim",
"heirline.nvim" %} plugin to render a nice status line
that includes lots of information about the current Git repository.
This is enabled by default.

### Git Commands

To see all the keys mapped to Git commands, press `<leader>g` and pause.

To open a window that displays the Git status of the current project,
press `<leader>gt`. Press `esc` twice to close the window.

To list all the branches and optionally switch to one,
press `<leader>gb`.

- Enter text in the "Git Branches" input to filter the branches.
- Press tab to jump to the "Results" pane that lists the branches.
- Press ctrl-j and ctrl-k to move down and up in the list.
  Repeatedly pressing tab also moves down in the list.
- Press the return key to switch to the selected branch.
- The "Git Branch Preview" pane on the right shows the commits on the branch.

To see a side-by-side diff for the current file, press `<leader>gd`.

To list all the commits for the current file, press `<leader>gc`.

To open a lazygit window, press `<leader>gg`.
To close a lazygit window, press `q`.
See the lazygit {% aTargetBlank
"https://github.com/jesseduffield/lazygit/blob/master/docs/keybindings/Keybindings_en.md",
"key bindings" %}.
For more information, watch this {% aTargetBlank
"https://www.youtube.com/watch?v=CPLdltN7wgE", "YouTube video" %}.

TODO: How can you commit changes and push them from inside AstroNvim?

### Color Themes

To see all the installed color themes and select one,
enter `:colo` followed by a space and the tab key.
TODO: How do you move the selection to a different one in the list?
Selecting a theme that is compatible with Tree-sitter results in better syntax highlighting.

### Completions

AstroNvim provides language-specific code completions.
To select a suggested completion from a provided list,
use ctrl-j and ctrl-k to move down and up
and press return to select the highlighted completion.

- Press `Ctrl-y>` to confirm selection of a completion.
- Press `<Ctrl-e>` to cancel completion.
- Press `<Down>` to navigate to the next suggested completion.
- Press `<Up>` to navigate to the previous suggested completion.
- If a completion menu is visible, press `<Ctrl-n>` to go to next item.
  Otherwise pressing this displays the completion menu.
- If a completion menu is visible, press `<Ctrl-p>` to go to previous item.
  Otherwise pressing this displays the completion menu.

### Documentation Windows

- Press `<Ctrl-d>` to scroll down in the documentation window.
- Press `<Ctrl-u>` to scroll up in the documentation window.

### Syntax Highlighting

AstroNvim provides language-specific syntax highlighting.

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

### Custom Plugins

To install a custom plugin,
create a file in the `~/.config/nvim/lua/plugins` directory
whose name is `{plugin-name}.lua`.
The contents of this file should be similar to those
shown in the "Hop" and "Todo Comments" sections below.

For help on a custom plugin, enter `:h {name}-config`.

### Hop

The {% aTargetBlank "https://github.com/phaazon/hop.nvim", "hop.nvim" %}
plugin is a rewrite of the {% aTargetBlank
"https://github.com/easymotion/vim-easymotion", "EasyMotion" %} Vim plugin
for Neovim.
It provides an interesting way to jump to a specific place within a file
that is currently visible.

To configure Hop, create the file `~/.config/nvim/lua/plugins/hop.lua`
containing the following:

```lua
return {
  "phaazon/hop.nvim",
  branch = 'v2', -- optional but strongly recommended
  config = function()
    require "hop".setup { keys = 'etovxqpdygfblzhckisuran' }
    -- Configure Hop the way you like here. See `:h hop-config`
    vim.keymap.set('n', '<leader>H', ":HopWord<cr>")
  end,
  event = "User AstroFile"
}
```

Enter `:Lazy sync` to install the plugin.
This opens a window that show the status of the install.
When this completes, press `q` to close the window.

To "hop" to a visible word, look at the target word
and enter `:HopWord` or press `<leader>H`.
This replaces the first two characters of every visible word
with a unique pair of letters.
Type the letters for the target word to jump to it.

To "hop" to a visible line, enter `:HopLine`.
This replaces the first two characters of every visible line
with a unique pair of letters.
Type the letters for the target line to jump to it.

The Hop plugin defines more commands, but `HopWord` and `HopLine`
are the most frequently used.

## Todo Comments

The {% aTargetBlank "https://github.com/folke/todo-comments.nvim",
"todo-comments.nvim" %} plugin highlights comments that begin with
"FIX:", "HACK:", "NOTE:", "PERF:", "TODO:", or "WARNING:"
... each with a different background color.
It also defines commands for navigating to these comments.

To install todo-comments, create the file
`~/.config/nvim/lua/plugins/todo-comments.lua` containing the following:

```lua
return {
  "folke/todo-comments.nvim",
  requires = "nvim-lua/plenary.nvim",
  config = function()
    require("todo-comments").setup {
      print("todo-comments setup entered")
      -- Add your configuration comes here or
      -- leave it empty to use the default settings.
    }
  end,
  event = "User AstroFile"
}
```

The comment syntax this plugin looks for is language specific.
The following are examples of comment prefixes that
this plugin recognizes in JavaScript code.

```js
// FIX: Please fix this.
// HACK: I really should not have done this.
// NOTE: For more information, see https://some-tutorial.com.
// PERF: This may cause a performance issue.
// TODO: Please do this.
// WARNING: This may break if invalid input is received.
```

There are multiple ways to display a list of all these kinds of comments
found in all files within the currrent project
(in and below the starting directory).

- To see them in a Telescope window, enter `:TodoTelescope`.
- To see them in a quick fix list, enter `:TodoQuickFix`.

### Emmet

{% aTargetBlank "https://docs.emmet.io", "Emmet" %} is an editor plugin
for quickly entering HTML, XML, and CSS.
It also supports many "actions" that operate on HTML and XML elements.
The most commonly used action is to expand an abbreviation or snippet.

AstroNvim does not ship with Emmett support.
To add it, see {% aTargetBlank "https://github.com/mattn/emmet-vim",
"emmet-vim" %}.

### Symbol List

To list symbols in a right pane, enter `<leader>lS`".
This lists symbols like variable, function, and type declarations.
Select a symbol name to scroll to it in the source file.
The list of symbols automatically updates
when focus moves to a different buffer.
