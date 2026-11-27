---
navigation:
  key: npkill
layout: topic-layout.njk
---

## Overview

[npkill](https://github.com/voidcosmos/npkill#readme)
is a lightweight, interactive command-line tool designed to
help developers find and delete heavy, unused node_modules
and other development leftovers to reclaim hard drive space.
It requires having Node.js installed.

To run it, open a terminal and cd to a directory to be checked.
Then enter `npx npkill` inside any directory.
It will evaluate all the directories inside that directory and below.

## Navigation & Controls

- Move up/down: Use the ↑ / ↓ arrow keys or j / k (vim keys).
- Delete directory: Press the space bar or delete key while on a directory to delete it.
- View details: Press → to view more information about the selected directory and press ← to go back.
- Open parent directory: Press o.
- Quit: Press q or ctrl-c.

## Notes

You can safely delete node_modules with `npkill` because they can
always be re-created later by running `npm install` in that project directory.

Some applications rely on specific node_modules directories.
`npkill` highlights these with a warning symbol (⚠) so you can avoid breaking dependencies.
