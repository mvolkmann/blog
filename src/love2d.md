---
eleventyNavigation:
  key: LÖVE
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://love2d.org/", "LÖVE" %} is
"an *awesome* framework you can use to make 2D games in Lua.
It's free, open-source, and works on Windows, Mac OS X, Linux, Android and iOS."

LÖVE can be downloaded from the previous link.

## Resources

- {% aTargetBlank "https://www.youtube.com/watch?v=3k4CMAaNCuk&t=3309s",
  "Falling in LÖVE with Lua" %}

  This is a great YouTube video on the LÖVE framework.
  Colton Ogden walks through the beginnings of writing a Super Mario game.
  The source code is available at {% aTargetBlank
  "https://github.com/coltonoscopy/cs502019games-track/tree/master/mario",
  "cs502019games-track" %}.

- {% aTargetBlank "https://www.sheepolution.com/learn", "Sheepolution" %}

  "Learn how to make games with LÖVE."
  This provides a series of articles and videos.

- {% aTargetBlank "https://love2d-community.github.io/love2d-book/",
  "love2d-book" %}

  This is a collaborative book about LÖVE.
  Clone the repository and follow the directions to generate a PDF.
 

## Documentation

The LÖVE home page is shown below:

<img alt="LÖVE Home Page" style="width: 100%"
    src="/blog/assets/love2d-home-page.png?v={{pkg.version}}"
    title="LÖVE Home Page">

Clicking the "Wiki" button in the upper-right navigates to the following page:

<img alt="LÖVE Wiki" style="width: 100%"
    src="/blog/assets/love2d-wiki.png?v={{pkg.version}}"
    title="LÖVE Wiki">

Click a module name in the left nav to see documentation
on all of its functions.

## Installing

For macOS:

- Click the "64-bit zipped" link under "macOS" to download `love.app`.
- Drag this file into the "Applications" directory.
- Double-click `love.app` to launch the app.
  This will fail the first time with the message
  "love.app cannot be opened because the developer cannot be verified".
  To fix this, open the Settings app, select "Privacy & Security",
  scroll down to "love.app was blocked ..." and click the "Open Anyway" button.

## Running

When the LÖVE app is run without a `main.lua` file,
a window matching the screenshot below is displayed.

<img alt="LÖVE No Game" style="width: 80%"
    src="/blog/assets/love2d-no-game.png?v={{pkg.version}}"
    title="LÖVE No Game">

## Unorganized Content

TODO: Does Love2D include its own version of Lua
TODO: and ignore the installed version?

To get started creating a game:

- Create a directory for a new game.
- Create a file in this directory named "main.lua".

If using VS Code:

- Install the extension "Love2D Support" from Pixelbyte Studios.
- Click the gear icon and select "Extension Settings".
- Change "Pixelbyte > love2d: Path" to
  "/Applications/love.app/Contents/MacOS/love".
- Open a `main.lua` file in an editor tab.
- Press cmd-l to run the game.

Love2D programs always define the functions
`love.load()`, `love.draw()`, and `love.update(dt)`.
The `love.load()` function performs initial game setup.
The `love.draw()` function specifies what should be
drawn on the screen at any point in time.
The `love.update(dt)` function implements the game logic.

The parameter `dt` in the `love.update` function is short for "delta time".
This is used to make game updates frame rate independent.
It is a floating point number that indicates
the number of seconds requires to display each frame.
This value can vary among devices.
For example, when `dt` is `0.1`, the device displays 10 frames per second.

To configure a game, add a `conf.lua` file to your game project directory.
For example:

```lua
function love.conf(t)
  t.title = "My Game"
  t.version = "11.4" -- version of Love2d
  t.window.width = 1280
  t.window.height = 720
  t.window.resizable = false
end
```

To run a game, use one of these approaches:

- Drag the game directory onto the Love application icon.
  In macOS, drag from the Finder.
- If VS Code has been configured property, press cmd-l.
- Start from a terminal window.

  - Add the following in `~/.zshrc`:
    ```bash
    alias love="/Applications/love.app/Contents/MacOS/love"
    ```
  - Open a new terminal session.
  - `cd` to your project directory.
  - Enter `love .`

When comparing the distance between two points to some value,
compare the square of the distance.
This removes the need to use the `math.sqrt` function
which can hurt game performance.

TODO: See lua/love/love-game/main.lua.

