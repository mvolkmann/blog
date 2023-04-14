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

## Vim Review

All features of Vim are also present in Neovim.
This section reviews a selected subset of them.

### Buffer Scrolling

To scroll the current buffer based on the line under the cursor:

| Key         | Action                   |
| ----------- | ------------------------ |
| `zt`        | moves to top             |
| `z<return>` | also moves to top        |
| `zz`        | moves to vertical center |

### Spell Checking

The commands configure the use of spell checking.

| Command                | Action                  |
| ---------------------- | ----------------------- |
| `:set spell`           | enables spell checking  |
| `:set spelllang=en_us` | sets the language       |
| `:set nospell`         | disables spell checking |

The following key mappings perform actions related to spell checking.

| Key  | Action                                                |
| ---- | ----------------------------------------------------- |
| `]s` | jumps to the next misspelled word                     |
| `[s` | jumps to the previous misspelled word                 |
| `z=` | opens a list of suggested replacements                |
| `zg` | adds the word under the cursor to the dictionary      |
| `zw` | removes the word under the cursor from the dictionary |

The list of suggested replacements appears at the bottom of the window.
Each suggestion is identified by a number or letter
that can be pressed to substitute it.

A major shortcoming of the builtin spell checker is that
it doesn't handle camel-cased words.
For example, it will mark "catDog" as an error.

TODO: See ~/.config/nvim/lua/user/init.lua for configuring this in AstroNvim.

```lua
return {
  polish = function()
    vim.opt.spell = true
    vim.opt.spelllang = "en_us"

    -- TODO: It seems Warp terminal or the font I'm using doesn't
    -- TODO: support undercurl text, but it does support underline text.
    vim.api.nvim_set_hl(
      0, -- global highlight group
      'SpellBad',
      -- { bg = "gray", fg = "red", underline = true }
      { fg = "red", underline = true }
    )
  end
}
```

### Macros

Macros record a series of keystrokes and assign them to a lowercase letter
so they can be replayed any number of times.

To define a macro:

1. Press `q` followed by the lowercase letter to which it will be assigned.
1. Type the keystrokes to be recorded.
1. Press `q` to end recording.

To replay a macro:

1. Move the cursor to where it should be when the macro begins playing.
1. Press `@` followed by the assigned lowercase letter.

To replay the last macro used, press `@@`.

To replay a macro multiple times, type the desired number
followed by `@` and the assigned macro letter.

For example, suppose we want to define a macro named "i" that
adds a hyphen and a space to the beginning of a line
and add a period at the end.

- Move the cursor to the beginning of line that needs these changes.
- Press `qi`.
- Type `i` to go into insert mode.
- Type `-` and a space.
- Press `esc` to exit insert mode.
- Press `$` to move the end of the line.
- Press `a` (append) to enter insert mode after the current position.
- Type `.`
- Press `esc` to exit insert mode.
- Press `0` to move the beginning of the line and
  press `j` to move down to the next line.
  These prepare for executing the macro again on the next line.
- Press `q` to end macro recording.

To execute this macro on the next 15 lines, type `15@i` or `15@@`.

### Quickfix List

The Quickfix list holds, displays, and jumps to the locations of:

- search matches
- compile errors
- test errors
- linting errors

Many Vim commands update the quickfix list, including `vimgrep` and `make`.
The `vimgrep` command takes a regular expression
and an indicator of which files should be searched.
For example, `:vimgrep /help/ %` searches for the word "help"
only in the current buffer and
`:vimgrep /help/ **/*.js` searches for the word "help" in all
JavaScript files in and below the directory from which Vim was started.

The `vimgrep` command is considerably slower than using
{% aTargetBlank "https://github.com/BurntSushi/ripgrep", "ripgrep" %}
which is integrated with the Neovim plugin {% aTargetBlank
"https://github.com/nvim-telescope/telescope.nvim", "Telescope" %}.

Each row in the quickfix list identifies a file, line number, column number,
and some text such as a match or error message.

The commands summarized below are the most frequently used commands
that operate on the quickfix list.

| Command           | Action                                                              |
| ----------------- | ------------------------------------------------------------------- |
| `copen`           | opens the quickfix list window                                      |
| `cclose` or `ccl` | closes the quickfix list window                                     |
| `cnext` or `cn`   | jumps to the location of the **next** item in the quickfix list     |
| `cprev` or `cp`   | jumps to the location of the **previous** item in the quickfix list |
| `cfirst`          | jumps to the location of the **first** item in the quickfix list    |
| `clast`           | jumps to the location of the **last** item in the quickfix list     |
| `cc<n>`           |                                                                     |

To navigate within the quickfix window,
use the normal vim key mappings such as `j` and `k`.

To jump to the location of the selected row, press the `return` key.

For more details on the quickfix list, location lists (which are similar),
and more, see {% aTargetBlank
"https://freshman.tech/vim-quickfix-and-location-list/",
"The quickfix and location lists in Vim" %}.

### Folds

| Key  | Action                                              |
| ---- | --------------------------------------------------- |
| `za` | toggles the fold on the cursor line                 |
| `zA` | toggles all folds on the cursor line (nested folds) |
| `zo` | opens fold on the cursor line                       |
| `zm` | folds more over entire buffer                       |
| `zr` | folds less over entire buffer                       |
| `zM` | closes all folds                                    |
| `zR` | opens all folds                                     |
| `zp` | peeks at folded lines; next keystroke hides again   |

Of these, `za` and `zR` are the most useful.

In AstroNvim press `z` and pause to see
all the key mappings related to folding.

### Sourcing Files

After editing a configuration file, enter `:source` or `:so` to execute it.
This is useful after making changes to configuration files
so you don't need to exit and restart nvim
in order for the changes to take effect.

## Neovim

### Installing Neovim

In macOs, Neovim can be installed using Homebrew.
To install it, enter `brew install neovim`.
To upgrade after it has been installed, enter `brew upgrade neovim`.

If you are already in the habit of using Vim,
it's a good idea to add an alias from "vim" to "nvim"
in your shell configuration file.
For zsh, edit `~/.zshrc` and add `alias vim="nvim"`.

### Missing Functionality

Vanilla Neovim lacks many features that are frequently desired.
There are plugins available to add all the important features.
Some of the most popular plugins for Neovim
are summarized in the table below.

| Feature             | Popular Plugins                                                                                               |
| ------------------- | ------------------------------------------------------------------------------------------------------------- |
| auto pairs          | nvim-autopairs                                                                                                |
| better navigation   | {% aTargetBlank "https://github.com/phaazon/hop.nvim", "hop.nvim" %}                                          |
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

### Configuring Neovim

Neovim can be configured using Vimscript, the Lua programming language,
or both.

To configure using Lua:

- Create the directory `~/.config/nvim/lua/user`.

- If your `.config` directory is in a Git repo (and it probably should be),
  remove the line `lua/user` from `~/.config/nvim/.gitignore`
  so it can be saved.

- In the new directory, create the file `init.lua`,
  the file `options.lua`, the file `mappings.lua`,
  and the directory `plugins`.

- In the `init.lua` file, return a table like the following:
  For example:

  ```lua
  return {
    colorscheme = "astrodark"
  }
  ```

- In the `mappings.lua` file, return a table like the following:
  For example:

  ```lua
  return {
    -- normal mode
    n = {
      ["<leader>-"] = { "<cmd>split<cr>", desc = "Horizontal Split" }
    }
  }
  ```

- In the `options.lua` file, return a table like the following:
  For example:

  ```lua
  return {
    opt = {
      scrolloff = 0,
      wrap = false
    }
  }
  ```

- See the "Custom Plugins" section below to learn about
  the files that should be created in the `plugins` directory.

See the official example of this customization approach at {% aTargetBlank
"https://github.com/AstroNvim/user_example", "AstroNvim/user_example" %}.

TODO: I want to map cmd-s to save, but I can't get these to work.

```lua
vim.keymap.set('n', '<D-s>', ":w<cr>")
vim.keymap.set('i', '<D-s', "<Esc>:w<cr>i")
vim.keymap.set('n', '<80><fd>hs', ":w<cr>")
vim.keymap.set('i', '<80><fd>hs', "<Esc>:w<cr>i")
```

The AstroNvim default settings found in
`~/.config/nvim/lua/astronvim/options.lua` use:

- 2-space indentation (`shiftwidth = 2` and `tabstop = 2`)
- relative line numbers (`relativenumber = true`)
- 24-bit colors (`termguicolors = true`)
- tabs expand to spaces (`expandtab = true`)
- line numbers shown (`number = true`)
- automatic indentation (`smartindent = true`)
- no line wrapping (`wrap = false`)

### Buffers

The contents of opened files are held in buffers.

Editing a buffer and writing it saves the modified content back to its file.

Neovim indicates buffers that have been modified and not written
with a dot after the file name.

Neovim provides the following default key mappings related to buffers:

| Key         | Action                                             |
| ----------- | -------------------------------------------------- |
| `<leader>c` | closes the current buffer                          |
| `[b`        | navigates to the buffer on the right               |
| `]b`        | navigates to the buffer on the left                |
| `<b`        | swaps the current buffer with the one on the left  |
| `>b`        | swaps the current buffer with the one on the right |

### Lua Functions

To run a Lua function exposed by a plugin,
enter `:{function-name}({arguments})`.

### Tree-sitter

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

### Pre-made Configurations

The three most popular Neovim pre-made configurations are
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

- {% aTargetBlank "https://github.com/BurntSushi/ripgrep", "ripgrep" %} - for live grep Telescope search (`<leader>fw`)
- {% aTargetBlank "https://github.com/jesseduffield/lazygit", "lazygit" %} - Git UI (`<leader>gg` for Git Gui or `<leader>tl`)
- {% aTargetBlank "https://github.com/ClementTsang/bottom", "bottom" %}- process viewer (`<leader>tt`)
- {% aTargetBlank "https://www.python.org", "Python" %}- for the Python REPL (`<leader>tp`)
- {% aTargetBlank "https://nodejs.org/en", "Node" %}- needed by many LSPs and for the Node REPL (`<leader>tn`)

### Installing AstroNvim

To install
{% aTargetBlank "https://github.com/AstroNvim/AstroNvim", "AstroNvim" %},

1. Install Neovim. In macOS enter `brew install neovim`.
1. Make a backup copy of your `~/.config/nvim` directory if you have one.
1. Delete the `~/.config/nvim` directory.
1. Enter `git clone --depth 1 https://github.com/AstroNvim/AstroNvim ~/.config/nvim`
1. Enter `brew install lua-language-server`.
1. Enter `nvim`. On first launch this will install many things.
1. Install language parsers by entering `:TSInstall {language-name}`
   for each language.
   For example, use the language names "javascript", "lua", and "swift".
1. Install LSP servers by entering `:LspInstall {server-name}`
   for each server.
   For example, use the server names "eslint", "tsserver", and "lua_ls".
   To see a list of all the available LSP servers for a given file type,
   open a file of that type and enter `:LspInstall`.
1. Enter `:NullLsInstall prettier`.
1. Optionally enter `:DapInstall {debug-adapter}`
   for each language-specific Debug Adapter Protocol server.
   (I could not find any of these.)
1. Enter `:Lazy sync` to update used plugins and remove unused plugins.
1. Enter `:AstroUpdatePackages` to get the latest AstroNvim updates.
1. If JavaScript will be edited then
   it is likely that Babel will be used parse files.
   This requires setting the environment variable `NODE_ENV`.
   When using zsh, add `export NODE_ENV=development` in `~/.zshrc`.

To check the status of your installation, enter `:checkhealth`.

### Updating

To update to the latest version of AstroNvim:

- Enter `:AstroUpdate`
- Enter `:AstroUpdatePackages`
- Enter `:Lazy Sync`

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

The `plugins` directory contains a separate config `.lua` file
for many of the plugins that AstroNvim uses by default.
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

TODO: Should you do this?
For better snippet support, create the file
`~/.config/nvim/lua/user/plugins/luasnip.lua` containing the following:

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

While a primary reason for using Vim is to
keep your hands on the keyboard for maximum efficiency,
many actions can also be accomplished using a mouse or trackpad.
For example, ctrl-clicking a symbol jumps to its definition.

AstroNvim defines `jj` and `jk` to exit insert mode just as the `esc` key does.

AstroNvim uses the {% aTargetBlank "https://github.com/folke/which-key.nvim",
"which-key" %} plugin to display applicable key mappings
at the bottom of the screen when you pause during entry.
This includes default and custom key mappings.

For example, press the leader key (`space` by default) and pause.
All the key mappings that begin with the leader key will be displayed.
The ones in blue require pressing additional keys
that are displayed when you press their key.
These include "Buffers", "Find", "Git", and more.
For example, type `<leader>f` to see all the "Find" key mappings.

### Font

Several parts of AstroNvim attempt to display icons.
This requires using a
{% aTargetBlank "https://www.nerdfonts.com/", "Nerd font" %}.
One that I recommend is "Caskaydia Cove Nerd Font" which is very similar to
the non-Nerd font {% aTargetBlank "https://github.com/microsoft/cascadia-code",
"Cascadia Code" %} from Microsoft.

### Basics

The {% aTargetBlank "https://astronvim.com/Basic%20Usage/mappings",
"Default Mappings" %} page lists all the key mappings
that AstroNvim provides by default.

The leader key defaults to `space`.

To open a new, empty buffer, press `<leader>n`.
To write the buffer to a file in the directory from which Neovim was launched,
enter `:w {file-name}`.

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

  This displays all the current marks in a Telescope window.
  Selecting one and pressing `return` jumps to it.
  See the "Marks" section below for more detail.

- "Last Session".

  This restores the most recent session.
  See the "Sessions" section below for more detail.

### File Explorer

AstroNvim uses the {% aTargetBlank
"https://github.com/nvim-neo-tree/neo-tree.nvim", "neo-tree.nvim" %} plugin.
for the file explorer that appears on the left when `<leader>e` is pressed.

The file explorer contains three tabs.
The "File" tab displays files and directories in and below
the directory from which Neovim was started.
The "Bufs" tab displays a list of all the current buffers.
The "Git" tab displays a list of modified, uncommitted files.
Press the `<` and `>` keys to navigate between these tabs.

Once open, press `?` to see the default key mappings.
Press `j` and `k` to navigate down and up to select a file or directory.

Some of the useful neo-tree key mappings include:

| Key      | Action                                                               |
| -------- | -------------------------------------------------------------------- |
| `?`      | shows all file explorer key mappings                                 |
| `#`      | performs fuzzy filtering; press `esc` to exit                        |
| `return` | opens selected file or directory                                     |
| `a`      | adds a new file or directory                                         |
| `A`      | adds a new directory                                                 |
| `H`      | toggles display of hidden files (hidden by default)                  |
| `S`      | opens selected file in a new horizontal split                        |
| `s`      | opens selected file in a new vertical split                          |
| `c`      | copies selected file; prompts for new name                           |
| `d`      | deletes selected file or directory                                   |
| `j`      | moves down to the next file or directory                             |
| `k`      | moves up to the next file or directory                               |
| `m`      | moves selected file or directory; prompts for destination directory  |
| `O`      | opens selected file using associated app (in macOS image -> Preview) |
| `r`      | renames selected file or directory                                   |
| `t`      | opens selected file in a new tab (new set of files)                  |
| `y`      | copies selected file to clipboard                                    |
| `p`      | pastes file from clipboard into selected directory                   |
| `<`      | navigates to previous tab (File, Bufs, or Git)                       |
| `>`      | navigates to next tab (File, Bufs, or Git)                           |

When `t` is pressed, a new tab is created.
These are represented by numbered buttons starting from 1 in the upper-right.
Clicking these switches to the corresponding tab.
To close a tab, select it and click the red circle containing an "X"
that appears after the last tab number.

The file explorer indicates when there are hidden files in a directory.
To show them, press `H`.

By default neo-tree hides all files and directories
listed in `~/.config/nvim/.gitignore`.
One of the directories listed is `lua/user`
which contains files that configure plugins.
One way to make this directory visible is to
remove the `lua/user` line from the `.gitignore` file.
Another way is to create the file `~/.config/nvim/lua/user/plugins/neo-tree.lua`
with the following content:

```lua
return {
  "nvim-neo-tree/neo-tree.nvim",
  opts = {
    filesystem = {
      filtered_items = {
        always_show = { "user" } -- option #1 targets a specific entry
        -- hide_gitignored = false -- option #2 targets all .gitignore entries
      }
    }
  }
}
```

### Splits

The editing area display multiple buffers
that are displayed in multiple split panes.

| Key            | Action                                                |
| -------------- | ----------------------------------------------------- |
| `<leader>/`    | creates a horizontal split (below)                    |
| `<leader>-`    | same as above using a custom key mapping I added      |
| `<leader>\|`   | creates a vertical split (right)                      |
| `ctrl-q`       | closes current split                                  |
| `:clo`         | closes current split                                  |
| `ctrl-h`       | moves to split on left                                |
| `ctrl-j`       | moves to split below                                  |
| `ctrl-k`       | moves to split above                                  |
| `ctrl-l`       | moves to split on right                               |
| `ctrl-{arrow}` | increases size of current split in direction of arrow |

Moving to a different split includes
moving from the file explorer to the first buffer
and from the first buffer to the file explorer.

In macOS the `ctrl-{arrow}` mappings will likely not work
due to default key mappings in System Settings.
The following screenshot shows the key mappings that need to changed or
disabled to allow the AstroNvim default resize key mappings to work.

<img alt="AstroNvim smart-splits keys" style="width: 80%"
    src="/blog/assets/astronvim-smart-splits-keys.png?v={{pkg.version}}"
    title="AstroNvim smart-splits keys">

### Telescope

The {% aTargetBlank "https://github.com/nvim-telescope/telescope.nvim",
"telescope.nvim" %} plugin "is a highly extendable fuzzy finder over lists".
It provides many commands and default key mappings them.

To operate on the current buffer:

| Key          | Action                                                                           |
| ------------ | -------------------------------------------------------------------------------- |
| `<leader>lf` | formats the contents                                                             |
| `<leader>lS` | toggles display of a right pane containing a list of symbols defined in the file |

Symbols include variables, functions, and type declarations.
Select a symbol name to scroll to it in the buffer.
The list of symbols automatically updates
when focus moves to a different buffer.

To operate on the symbol under the cursor:

| Key          | Action                                                                                |
| ------------ | ------------------------------------------------------------------------------------- |
| `gd`         | goes to the definition of the symbol under the cursor                                 |
| `gD`         | goes to the declaration of the symbol under the cursor                                |
| `gi`         | lists all implementations for the symbol under the cursor in a quickfix window        |
| `gI`         | goes to the implementation of the symbol under the cursor                             |
| `gr`         | shows references to the symbol under the cursor in a quickfix window                  |
| `gs`         | displays signature information about the symbol under the cursor in a floating window |
| `gT`         | goes to the type definition of the symbol under the cursor                            |
| `K`          | shows the type of the symbol under the cursor                                         |
| `<leader>la` | opens a menu of applicable code actions in a Telescope window                         |
| `<leader>lr` | renames the symbol under the cursor; prompts for new name                             |
| `ctrl-o`     | moves backwards through results                                                       |

Some LSP servers to not support all these action.

For diagnostic messages:

| Key  | Action                                                  |
| ---- | ------------------------------------------------------- |
| `[d` | move to the previous diagnostic in the current buffer   |
| `]d` | move to the next diagnostic in the current buffer       |
| `gl` | shows the full error message when an error is displayed |
| `lD` | displays all diagnostics in a Telescope window          |

To close a diagnostic popup, move the cursor.

After pressing `gl`, press it again to move focus into the diagnostic popup.
For focus is in a diagnostic popup, press `q` to close it.

For file paths:

| Key  | Action                                                |
| ---- | ----------------------------------------------------- |
| `gf` | opens the file under the cursor in Neovim             |
| `gx` | opens the file under the cursor in its associated app |

Some telescope commands display a new window containing multiple sections.
The upper-left section contains a text input
used to filter the results displayed in the lower-left section.
Initially the focus will be in this text input and it will be in insert mode.
We will refer to this section as "filter".

The lower-left section displays the filtered results.
We will refer to this section as "results".
To move focus from the the filter section to the results section,
press `tab` or `ctrl-j`.
The selected item displays a `>` to its left and has a gray background.
To select a different item, press `tab` (down),
`ctrl-j` (down), `ctrl-k` (up), or the down and up arrow keys.
To open a selected file, press the `return` key.

The right section displays a preview
of what is selected in the lower-left section.
We will refer to this section as "preview".

Press the `esc` key to exit insert mode.
When this is done you move the cursor left and right in the filter input
by pressing `h` and `l`.
You can also change the selection in the result section
by pressing `j` and `k`.
When not in insert mode, press `?`
to see all the applicable Telescope key mappings.

To close a Telescope window, press `q`.

To configure Telescope so pressing the `esc` key closes
the Telescope window just like pressing `q`, create the file
`~/.config/nvim/lua/user/plugins/telescope.lua` containing the code below.
When in insert mode, it is necessary to press `esc` twice,
once to exit insert mode and once to close the window.

```lua
return {
  "nvim-telescope/telescope.nvim",
  opts = function()
    local actions = require "telescope.actions"
    return {
      defaults = {
        mappings = {
          n = { ["<Esc>"] = actions.close }
        },
      },
    }
  end
}
```

The Telescope fuzzy finder can find many things including
files, buffers, key mappings, help, and more.
The key mappings to initiate fuzzy finder searches include:

| Key          | Action                                             |
| ------------ | -------------------------------------------------- |
| `<leader>fa` | finds AstroNvim configuration files                |
| `<leader>fb` | finds buffers by name                              |
| `<leader>fc` | finds files that contain the word under the cursor |
| `<leader>fC` | finds Vim plugin commands                          |
| `<leader>ff` | finds files by name                                |
| `<leader>fh` | finds a help file by its name                      |
| `<leader>fk` | finds key mappings                                 |
| `<leader>fo` | finds files opened recently (old files)            |
| `<leader>fr` | finds Vim registers (can see their contents)       |
| `<leader>ft` | finds a theme (can see previews and select one)    |
| `<leader>fw` | finds files by a word in their content (live_grep) |
| `<leader>fW` | finds files containing multiple consecutive words  |

In a Telescope window the key mappings include:

| Key         | Action                                                                |
| ----------- | --------------------------------------------------------------------- |
| `esc` `esc` | closes the window                                                     |
| `ctrl-c`    | also closes the window                                                |
| `ctrl-x`    | opens the selected file in a horizontal split (below current buffer)  |
| `ctrl-v`    | opens the selected file in a vertical split (right of current buffer) |
| `ctrl-d`    | scrolls down in file preview                                          |
| `ctrl-u`    | scrolls up in file preview                                            |
| `tab`       | moves focus from "Find Files" input to "Results" section              |
| `tab`       | when in "Results" section, moves down to next item                    |
| `shift-tab` | moves focus from "Results" section to "Find Files" input              |

To see all the Telescope key mappings,
press the `esc` key to exit insert mode and press `?`.
The key mappings will appear at the bottom of the window.

### Comments

To toggle commenting of the current line or selected lines, press `<leader>/`.

### Code Formatting

AstroNvim uses the {% aTargetBlank
"https://github.com/jose-elias-alvarez/null-ls.nvim", "null-ls.vim" %} plugin
to perform code formatting. I think, but have not confirmed,
that `null-ls.vim` provides formatting of Markdown tables.

### Auto-pairs

AstroNvim uses the {% aTargetBlank "https://github.com/windwp/nvim-autopairs",
"nvim-autopairs" %} plugin.
This handles pairs of parentheses, square brackets, and curly braces.
When one of the opening characters (`(`, `[`, or `{`) is typed,
the closing character (`)`, `]`, or `}`) is automatically supplied
and the cursor is placed between them.

### Status Line

AstroNvim uses the {% aTargetBlank "https://github.com/rebelot/heirline.nvim",
"heirline.nvim" %} plugin to render a nice status line
that includes lots of information about the current Git repository.
This is enabled by default.

### Git Integration

When editing a file in a Git repository,
colored vertical lines appear in the gutter to indicate
added lines (green), delete lines (red), and modified lines (orange).

The left side of the status bar at the bottom displays
the number of new, modified, and deleted files.

The file explorer `Git` tab lists the modified files.

To see all the keys mapped to Git commands, press `<leader>g` and pause.

| Key          | Action                                                                                  |
| ------------ | --------------------------------------------------------------------------------------- |
| `<leader>gb` | displays a list of local Git branches in a Telescope window and allows switching to one |
| `<leader>gc` | lists all the commits for the current file                                              |
| `<leader>gd` | displays a side-by-side diff for the current file                                       |
| `<leader>gt` | open a window that displays the Git status of the current project                       |

In the Telescope window displayed by `<leader>gb`,
the "Git Branch Preview" pane on the right shows the commits on the branch.

### Lazygit

A better way to manage Git repositories from inside Neovim is to use
{% aTargetBlank "https://github.com/jesseduffield/lazygit", "Lazygit" %}
which is a terminal UI for executing Git commands.
Lazygit is an alternative to {% aTargetBlank
"https://github.com/tpope/vim-fugitive", "git-fugitive" %}.

To launch lazygit from a terminal window,
cd to a repository directory and enter `lazygit`.

To launch lazygit from within AstroVim, enter `<leader>gg` for Git GUI.
This opens a new window with a left and right side.
To close this window, press `q`.

See the lazygit {% aTargetBlank
"https://github.com/jesseduffield/lazygit/blob/master/docs/keybindings/Keybindings_en.md",
"key bindings" %}.
For more information, watch this {% aTargetBlank
"https://www.youtube.com/watch?v=CPLdltN7wgE", "YouTube video" %}.

See {% aTargetBlank "https://github.com/kdheepak/lazygit.nvim/issues/92",
"issue 92" %}.

The left side contains five numbered sections,
Status (1), Files (2), Local Branches (3), Commits (4), and Stash (5).
To move focus to the next section press the `tab` key or the right arrow key.
To move focus to the previous section
press the left arrow key (`shift-tab` does not work).
To move to a specific section press its number.

To see the key mappings that apply to the section that currently has focus,
press `?` or `x`.

To move down and up within a section,
use the `j` and `k` keys or the down and up arrow keys.

To search in any section, press `/`.

The right side contains information about the item selected on the left side.
To scroll down and up, press `ctrl-d` and `ctrl-u`.
TODO: Is there a way to scroll by full pages?

To execute an arbitrary shell command without leaving lazygit, press `:`.
This opens a dialog where a shell command can be entered.
Press `return` to execute it.
The output temporarily replaces the lazygit UI.
Press `return` again to return to lazygit.

To customize the configuration of lazygit, create the file
`~/.config/nvim/lua/user/plugins/lazygit.lua`.
For example:

```lua
return {
  "kdheepak/lazygit.nvim"
  -- TODO: I want this to configure side-by-side diffs using the delta pager
  -- TODO: which must be installed, but this is not working!
  -- TODO: See https://github.com/jesseduffield/lazygit/blob/master/docs/Custom_Pagers.md.
  --[[
  config = function()
    require("lazygit").setup {
      pager = "delta",
      delta = "side-by-size"
    }
  end,
  event = "User AstroFile" -- need this?
  --]]
}
```

#### Status Section

The "Status" section shows the repository name and the current branch.

| Key      | Action                                                             |
| -------- | ------------------------------------------------------------------ |
| `a`      | shows the log for all branches                                     |
| `u`      | checks for a lazygit update                                        |
| `return` | enables switching to a different repository selected from a dialog |

#### Files Section

This section contains two tabs, "Files" and "Submodules".
The "Files" tab lists all the modified files.

- To see diffs for a file on the right side, select the file.
  For a side-by-side diff:

  - Enter `brew install git-delta` to install the delta pager.
  - Add the following in `~/.gitconfig`
    ```text
    [core]
      pager = delta
    [delta]
      side-by-side = true
    ```

TODO: THIS DOES NOT WORK!
TODO: I can use `delta` to display side-by-side diffs using the `git diff` command,
TODO: but I cannot get it to work with Lazygit.
TODO: See ~/.config/lazygit/config.yaml.
TODO: See https://github.com/jesseduffield/lazygit/issues/2337.

| Key      | Action                                                                            |
| -------- | --------------------------------------------------------------------------------- |
| `space`  | toggle whether the selected file is staged for inclusion in a commit              |
| `a`      | toggle all modified files between being staged and not staged                     |
| `c`      | commits all staged files; prompts whether to commit all if none are staged        |
| `C`      | same as `c`, but opens a Vim window to enter commit message (broken in AstroNvim) |
| `d`      | discards all changes in the selected file; press `d` again to confirm             |
| `D`      | opens a menu of options where one is "hard reset"                                 |
| `f`      | fetches changes from remote branch                                                |
| `i`      | adds file to `.gitignore`                                                         |
| `r`      | refreshes list of files; useful when modified outside of Neovim                   |
| `s`      | stashes all changes; prompts for stash name                                       |
| `S`      | stashes only staged changes; prompts for stash name                               |
| \`       | toggles the file list between flat and tree views                                 |
| `ctrl-w` | toggles hiding lines in right side that only differ by whitespace                 |

When committing changes a dialog will appear where a commit message can be entered.
After entering a commit message, press the `return` key to perform the commit.

The menu that appears when `D` is pressed contains:

- nuke working tree - runs `git reset --hard HEAD && git clean -fd`
- discard unstaged changes
- discard untracked files
- discard staged changes
- soft reset - discards the last commit
- mixed reset - discards the last commit and stages (add)
- hard reset - discards the last commit and stages (add) and all changes

#### Local Branches Section

This section contains three tabs named "Local Branches", "Remotes", and "Tags".
The "Local Branches" tab lists all the local branches.
The currently checked out branch that name is preceded by `*`.
Selecting a local branch shows all of its commits on the right side.

| Key      | Action                                                                             |
| -------- | ---------------------------------------------------------------------------------- |
| `return` | displays all commits selected local branch in this section; press `esc` to restore |
| `space`  | checks out the selected branch                                                     |
| `c`      | creates a new remote branch                                                        |
| `d`      | deletes the selected local branch after confirming                                 |
| `f`      | fetches changes from remote branch                                                 |
| `M`      | merges the selected branch into the checked out branch after confirming            |
| `n`      | creates a new local branch                                                         |
| `o`      | starts process of opening a pull request in a new browser tab (must push first)    |
| `p`      | pulls changes from the corresponding remote branch                                 |
| `P`      | pushes changes to the corresponding remote branch                                  |
| `R`      | renames the selected branch                                                        |
| `r`      | rebases the selected branch into the checked out branch after confirming           |

When attempting to checkout a different branch, if there are
uncommitted changes a dialog will appear that offers to stash the changes.

If a merge results in conflicts, a dialog will appear explaining that.
Press `esc` to abort the merge or `return` to resolve the conflicts.
Each conflict will be described by a pair of "hunks".
To keep one of the hunks, move the cursor to it with `j` and `k`
and press `space`. The other hunk will be discarded.
To keep both hunks, press `b`.
Another dialog will appear after all merge conflicts have been resolved.

To delete a remote branch, switch to the "Remotes" tab in this section,
select a branch, and press `d`.

To create a new remote branch, switch to the "Remotes" tab in this section,
and press `n`.

To create a new tag, switch to the "Tags" tab in this section,
and press `n`.

To delete a tag, switch to the "Tags" tab in this section,
select a tag, and press `d`.

#### Commits Section

This section contains two tabs, "Commits" and "Reflog".
The "Commits" tab lists all the commits on the current local branch.
Selecting a commit shows detail about it on the right side including
the commit comment and a list of the new, modified, and deleted files.

From the git docs, "Reflogs record when the tips of branches and
other references were updated in the local repository.

| Key     | Action                                         |
| ------- | ---------------------------------------------- |
| `d`     | deletes the selected commit                    |
| `o`     | opens the selected commit in a new browser tab |
| `s`     | squashes the selected commit                   |
| `S`     | squashes all commits above the selected commit |
| `T`     | adds a tag to the selected commit              |
| `t`     | reverts the selected commit after confirming   |
| `space` | checks out the selected commit                 |

#### Stash Section

The "Stash" section lists all the current stashes.
Selecting a stash shows the stashed changes on the right side.

| Key      | Action                                                          |
| -------- | --------------------------------------------------------------- |
| `d`      | drops the selected stash after confirming                       |
| `space`  | applies the selected stash without dropping it after confirming |
| `g`      | applies the selected stash and drops it after confirming        |
| `r`      | renames the selected stash                                      |
| \`       | toggles the stash list between flat and tree views              |
| `return` | displays diffs for the stash in the right side                  |

### Color Themes

To see all the installed color themes and select one,
enter `:colo` followed by a space and the tab key.
Press the `tab` key repeatedly to select a theme.
Press the `return` key activate the selected theme.
Selecting a theme that is compatible with Tree-sitter
results in better syntax highlighting.

### Completions

AstroNvim provides language-specific code completions.
To select a suggested completion from a provided list,
use ctrl-j and ctrl-k to move down and up
and press return to select the highlighted completion.

| Key          | Action                                         |
| ------------ | ---------------------------------------------- |
| `ctrl-space` | manually opens a list of suggestions           |
| `ctrl-y`     | confirms selection of a completion             |
| `ctrl-e`     | cancels completion and closes the list         |
| `ctrl-n`     | navigates to the next suggested completion     |
| `ctrl-p`     | navigates to the previous suggested completion |
| down arrow   | navigates to the next suggested completion     |
| up arrow     | navigates to the previous suggested completion |

### Documentation Windows

- Press `<Ctrl-d>` to scroll down in the documentation window.
- Press `<Ctrl-u>` to scroll up in the documentation window.

### Syntax Highlighting

AstroNvim provides language-specific syntax highlighting.

### Snippets

AstroNvim uses the {% aTargetBlank "https://github.com/L3MON4D3/LuaSnip",
"LuaSnip" %} plugin to support snippets.
Snippet suggests appear when the beginning of their trigger words are typed.
When a list of possible snippets appears,
press the `tab` key or `ctrl-j` and `ctrl-k` to highlight one,
and press the `return` key to select it.

For snippets that have placeholders, type text or paste text into each one.
Press tab to jump to the next placeholder
and shift-tab to jump to the previous placeholder.
After entering text for the last placeholder,
press tab one more tab to move the end of the snippet and continue typing.

LuaSnip supports two syntaxes for defining snippets,
the VS Code style and the LuaSnips style.

To create custom snippets:

1. Create the directory `~/.config/nvim/lua/user/snippets`.

1. Create the file `~/.config/nvim/lua/user/plugins/luasnip.lua`
   containing the following:

   ```lua
   return {
     "L3MON4D3/LuaSnip",
     config = function(plugin, opts)
       require "plugins.configs.luasnip" (plugin, opts)
       require("luasnip.loaders.from_vscode").lazy_load {
         paths = { "./lua/user/snippets" }
       }
     end
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
         },
         {
           "language": "markdown",
           "path": "./markdown.json"
         }
       ]
     }
   }
   ```

1. For the VS Code style, create a file like the following
   for each language that needs snippets.
   Placeholders are represented by a `$` followed by a number starting from `1`.
   For example, a snippet with three placeholders
   will contain `$1`, `$2`, and `$3`.
   The final placeholder can be represented by `$0` instead of `$3`,
   but its not clear what advantage that provides.

   For JavaScript the file name should be `javascript.json`.
   For example:

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

   For Markdown the file name should be `markdown.json`.
   For example:

   ```json
   {
     "Anchor Target Blank": {
       "prefix": "atb",
       "body": ["{% aTargetBlank \"$1\", \"$2\" %}"],
       "description": "Markdown 11ty aTargetBlank shortcode"
     },
     "Image": {
       "prefix": "img",
       "body": [
         "<img alt=\"$1\" style=\"width: 50%\"\n  src=\"/blog/assets/$2.png?v={{pkg.version}}\"\n  title=\"$1\">"
       ],
       "description": "Markdown <img>"
     }
   }
   ```

TODO: Describe the LuaSnips syntax for defining snippets.

### Marks

Vim supports marking locations in files and easily returning to them.

| Key                   | Action                                                      |
| --------------------- | ----------------------------------------------------------- |
| `m{lowercase-letter}` | creates a mark that is local to the current file            |
| `m{uppercase-letter}` | creates a global mark                                       |
| `'{letter}`           | jumps to first non-whitespace character on line of the mark |
| <pre>\`{letter}</pre> | jumps to line and column of the mark                        |
| `:marks`              | lists all the marks                                         |
| `<leader>f'`          | lists all the marks in a Telescope window                   |

When marks are displayed in a Telescope window,
select one an press `return` to jump to it.

To delete marks, enter `:delmarks {letter}, {letter}, ...`

There are many special marks that are automatically set.
All of them seem to have limited usefulness.
These are displayed by both `:marks` and `<leader>f'`.

### Sessions

A session records the directory from which AstroNvim was started
and the buffers that were open (but not their order).
There can only be one session per directory.
Once a session has been loaded, the list of open buffers
in the session is automatically updated.

The following key mappings must be used while focus is in a buffer,
not in the file explorer.

| Key          | Action                                                                 |
| ------------ | ---------------------------------------------------------------------- |
| `<leader>Ss` | creates a new session for the starting directory if it doesn't exist   |
| `<leader>Sf` | opens a dialog that lists saved sessions and allows loading to one     |
| `<leader>S.` | loads the session for the current directory if it exists               |
| `<leader>Sl` | loads the last (most recent) session                                   |
| `<leader>Sd` | opens a dialog that lists saved sessions and allows them to be deleted |

In the load dialog, select a session and press `return` to load it.

In the delete dialog, select a session and press `return` to delete it.

To close one of these dialogs, press the `esc` key twice.

### Terminal

To open a floating terminal window, press `F7` or enter `:ToggleTerm`.
On a MacBook keyword, hold the `fn` key and press `F7`.

### Lazy

{% aTargetBlank "https://github.com/folke/lazy.nvim", "lazy.nvim" %}
"is a modern plugin manager for Neovim".
It is the plugin manager used by AstroNvim.
The configuration for lazy.nvim can be found in
`~/.config/nvim/lua/astronvim/lazy.lua`.

To verify that `lazy.nvim` is properly configured,
enter `:checkhealth lazy`.

Enter `:Lazy sync` to update used plugins and remove unused plugins.

### Custom Plugins

When AstroNvim starts it executes all the `.lua` files
in the `~/.config/nvim/lua/plugins` directory
followed by all the `.lua` files
in the `~/.config/nvim/lua/user/plugins` directory.
Each of these files serves to configure a specific plugin.
Any names can be used for these files, but it's standard practice
to name them after the plugin they configure.

Having a separate directory for the configuration files you create,
makes it easier to avoid losing custom configurations
when updating to a new version of AstroNvim.

To install a custom plugin, create a configuration file for it
in the `~/.config/nvim/lua/user/plugins` directory
whose name is `{plugin-name}.lua`.
After doing this it may be necessary to
enter `:Lazy` and press `S` to sync the plugins.
This downloads the latest version of each plugin being used.

The contents of the plugin configuation files
should be similar to those shown in subsections below.

For help on a specific custom plugin, enter `:h {name}-config`.

#### hop.nvim Plugin

The {% aTargetBlank "https://github.com/phaazon/hop.nvim", "hop.nvim" %}
plugin is a rewrite of the {% aTargetBlank
"https://github.com/easymotion/vim-easymotion", "EasyMotion" %} Vim plugin
for Neovim.
It provides an interesting way to jump to a specific place within a file
that is currently visible.

To configure Hop, create the file `~/.config/nvim/lua/user/plugins/hop.lua`
containing the following:

```lua
return {
  "phaazon/hop.nvim",
  branch = 'v2', -- optional but strongly recommended
  config = function()
    require "hop".setup {}
    -- When in normal mode, initiate with a capital "H".
    vim.keymap.set('n', 'H', ":HopWord<cr>")
  end,
  event = "User AstroFile"
}
```

The `event` value specifies when the plugin should be triggered.
This can be a single event or a table of them.
The supported events are:

- `VeryLazy`: triggered after starting Neovim
- `BufEnter *.lua`: triggered after opening a `.lua` file
- `User AstroFile`: triggered after opening any file
- `LspAttach`: triggered after starting LSP servers
- `InsertEnter`: triggered after entering insert mode

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

#### Neoformat Plugin

The {% aTargetBlank "https://github.com/sbdchd/neoformat", "Neoformat" %} plugin
"uses a variety of formatters for many filetypes.
Currently, Neoformat will run a formatter using the current buffer data,
and on success it will update the current buffer with the formatted text.
On a formatter failure, Neoformat will try the next formatter
defined for the filetype."

Prettier is one of the supported formatters.

To install and configure this, create the file
`~/.config/nvim/lua/user/plugins/neoformat.lua` with the following content:

```lua
return {
  "sbdchd/neoformat",
  event = "User AstroFile"
}
```

#### todo-comments.nvim Plugin

The {% aTargetBlank "https://github.com/folke/todo-comments.nvim",
"todo-comments.nvim" %} plugin highlights comments that begin with
"FIX:", "HACK:", "NOTE:", "PERF:", "TODO:", or "WARNING:"
... each with a different background color.
It also defines commands for navigating to these comments.

To install todo-comments, create the file
`~/.config/nvim/lua/user/plugins/todo-comments.lua` containing the following:

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
found in all files within the current project
(in and below the starting directory).

- To see them in a Telescope window, enter `:TodoTelescope`.
- To see them in a quick fix list, enter `:TodoQuickFix`.

#### smartcolumn.nvim Plugin

The {% aTargetBlank "https://github.com/m4xshen/smartcolumn.nvim",
"smartcolumn.nvim" %} plugin displays a vertical line at a given column
only if at least one line in the file extends past that column.

To install smartcolumn.nvim, create the file
`~/.config/nvim/lua/user/plugins/smartcolumn.lua` containing the following:

```lua
return {
  "m4xshen/smartcolumn.nvim",
  opts = {
    -- colorcolumn = 80 -- This is the default.
    -- Don't disable any file types.
    disabled_filetypes = {} -- default is {"help", "text", "markdown"}
  },
  event = "User AstroFile"
}
```

#### Emmet

{% aTargetBlank "https://docs.emmet.io", "Emmet" %} is an editor plugin
for quickly entering HTML, XML, and CSS.
It also supports many "actions" that operate on HTML and XML elements.
The most commonly used action is to expand an abbreviation or snippet.

AstroNvim does not ship with Emmett support.
To add it, see {% aTargetBlank "https://github.com/mattn/emmet-vim",
"emmet-vim" %}.

### AstroNvim Community

The {% aTargetBlank "https://github.com/AstroNvim/astrocommunity",
"AstroNvim Community Repository" %} is a collection of
AstroNvim plugins and their configurations.
These are typically easier to install that configuring them manually.

To add these to your AstroNvim setup, create the file
`~/.config/lua/user/plugins/community.lua` and add content the like the following:

```lua
return {
  "AstroNvim/astrocommunity",
  { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.colorscheme.catppuccin", enabled = true },
  { import = "astrocommunity.colorscheme.nightfox", enabled = true }
}
```
