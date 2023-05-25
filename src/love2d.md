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

LÖVE use LuaJIT which mostly only supports Lua 5.1.
It includes some Lua 5.2 and 5.3 features.

## Resources

- {% aTargetBlank "https://discord.gg/r5JjuFU8",
  "LÖVE Game Framework Discord Channel" %}

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

- The LÖVE {% aTargetBlank "https://love2d.org/wiki/Category:Snippets",
  "Snippets" %} page includes a list of snippets that each provide
  code that be copied for specific functionality.
  For example, it provides code for generating gradients
  and testing whether a point is in a shape.

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

The initial source file must be named `main.lua`.
When the LÖVE app is run without this file,
a window matching the screenshot below is displayed.

<img alt="LÖVE No Game" style="width: 80%"
    src="/blog/assets/love2d-no-game.png?v={{pkg.version}}"
    title="LÖVE No Game">

Note that the kite tail on the balloon says "NO GAME".

## Bundling

Create a shell script named `bundle` in the top directory
of the project containing the following where
`{project-name}` is replaced by the name of the project:

```bash
#!/usr/bin/env zsh
rm -f {project-name}.love
zip -r {project-name}.love .
```

Make the `bundle` script executable by entering `chmod a+x bundle`.

Enter `./bundle` to bundle the LOVE project into a `.love` file.
This file can be double-clicked to run the app locally.

## Auto-Restarts

It is convenient to configure the app to restart
any time a file within the project is modified.
The Lua module {% aTargetBlank "https://github.com/rxi/lurker", "lurker" %}
is perfect for this!

To configure this:

1. Download the file `lurker.lua` into the project directory.
1. Download the file `lume.lua` into the project directory.
1. Add the following lines near the top of `main.lua`:

   ```lua
   local lurker = require "lurker"
   lurker.postswap = function() love.event.quit "restart" end
   ```

1. Add the following line at the end of the `love.update` function:

   ```lua
   lurker.update()
   ```

Now when the app is run it will watch for changes to files within the project
and restart the app when a change is detected.
One issue with this is that every time the game is restarted,
focus will move from the code editor to the game.

## Colors

TODO: Add detail here.

## Fonts

TODO: Add detail here.

## Images

TODO: Add detail here.

## Sounds

TODO: Add detail here.

## Graphics

The `love.graphics` module provides many functions for drawing.
To simplify calling functions from the submodules of the `love` module,
create and use variables like these to refer to the functions:

```lua
local g = love.graphics
local p = love.physics
```

## Frame Rate

To display the frames per second being rendered
add code like the following in the `love.draw` function:

```lua
love.graphics.print("FPS: " .. love.timer.getFPS(), 10, 5)
```

## Cursor

The following code draws a circle at the cursor position
when placed in the `love.draw` function:

```lua
love.graphics.setColor(colors.yellow)
love.graphics.circle("fill", x, y, 10) ]]
```

## Keys

There are multiple ways to process key presses.

One way is to write the function `love.keypressed`.
For example, the following code causes the app
to be restarted when the escape key is pressed.

```lua
function love.keypressed(k)
  if k == "escape" then love.event.quit("restart") end
end
```

Another way is to use the `love.keyboard.isDown` function.
This takes arguments that are the key names to check
and returns a boolean indicating if any of them are down.
A table that maps key names to functions that should be called
when they are pressed can be defined as follows:

```lua
local keyMap = {
  left = function() dec(monkeyPosition, "x") end,
  right = function() inc(monkeyPosition, "x") end,
  up = function() dec(monkeyPosition, "y") end,
  down = function() inc(monkeyPosition, "y") end
}
```

Then add code like the following in the `love.update` function:

```lua
  -- Process keys being held down.
  for key, fn in pairs(keyMap) do
    if love.keyboard.isDown(key) then fn() end
  end
```

## Mouse Clicks

There are several ways to process mouse clicks.

One way is to write the function `love.mousepressed`.
For example, the following code causes the app
to be restarted when the escape key is pressed.

```lua
function love.mousepressed(x, y, button)
  -- button holds the number of the mouse button that was pressed
  -- where 1 is the left mouse button.
  if button ~= 1 then return end -- only processing left clicks

  -- For all shapes that should process mouse clicks,
  -- determine whether (x,y) is on the shape
  -- and execute the appropriate code if it is.
end
```

Another may is to use the `love.mouse.isDown` function.
This takes arguments that are the button numbers to check
and returns a boolean indicating if any of them are down.

## Physics

The `love.physics` module provides many functions that wrap the functionality
of {% aTargetBlank "https://box2d.org/", "Box2D" %} which is
a C++-based 2D physics engine.
Box2D was created by Erin Catto who works at Blizzard Games.
Box2D is used by many games including Angry Birds.

## Deploying

LÖVE apps can be deployed to Windows, macOS, Linux, Android, and iOS.
Details can be found at {% aTargetBlank
"https://love2d.org/wiki/Game_Distribution", "Game Distribution" %}.

### Web

One way to deploy a LÖVE app to the web is to use {% aTargetBlank
"https://github.com/Davidobot/love.js", "love.js" %}.

Generate a web application from a LÖVE project in the current directory
with the following steps.
This creates the subdirectory `web` containing many generated files.

- Enter `npx love.js . web`
- Press return to proceed.
- Enter a name for the game.

Create a web server that sets the appropriate HTTP headers
to enable use of `SharedArrayBuffer` with the following steps:

- Install Node.js.
- Enter `mkdir server`
- Enter `cd server`
- Enter `touch server.js`
- Edit `server.js` and add the following code:

  ```js
  const express = require('express');

  const app = express();

  app.use(express.static(
    '../web',
    {
      setHeaders: res => {
        res.set('Cross-Origin-Opener-Policy', 'same-origin');
        res.set('Cross-Origin-Embedder-Policy', 'require-corp');
      }
    }
  ));

  const PORT = 1919;
  app.listen(PORT, () => console.log('browse localhost:' + PORT));
  ```

Start the web server and run the game with the following steps:

- Enter `node server.js`
- Browse localhost:1919

During development it will be necessary to rebuild the web app
and restart the web server many times.
The following shell script automates those tasks.
Consider naming it `websrv` and make it executable.

```sh
#!/usr/bin/env zsh

rm -rf web
npx love.js src web
pushd server
node server.js
popd
```

To run this, enter `./websrv`.

### iOS

The YouTube video {% aTargetBlank
"https://www.youtube.com/watch?v=MsYanwcU42E&list=WL&index=108&t=6s",
"Build LOVE2D for iOS iPhone, iPad, iPod Touch" %}
walks through the steps to deploy a LÖVE app to iOS.
This must be done in macOS.

The following steps create a default iOS project and run it:

1. If not already installed, install Xcode.
1. Download iOS source by clicking the "iOS source /libraries" link
   in the Download section of the
   {% aTargetBlank "https://love2d.org/", "LÖVE" %} home page.
1. Double-click the downloaded file to unzip it.
1. Move this directory to its desired location and rename it.
1. In the Finder, navigate to `platform/xcode`.
1. Double-click the file `love.xcodeproj` to open it in Xcode.
1. Select a simulator device.
1. Build and run the app.
1. In the simulator, tap "No-game screen"
   to see the default "NO GAME" screen.

The following steps customize the default project to run your game.

1. Enter `./bundle` to bundle the LOVE project into a `.love` file.
1. Back in Xcode, select the top-most project navigator item
   to edit the LÖVE project.
1. Select the "love.ios" target.
1. Select the "General" tab.
1. In the "Identity" section, enter a "Display Name".
   TODO: Why is the displayed app name "love" instead of this value?
1. In the "Deployment Info" section under "iPhone Orientation",
   consider only checking "Portrait".
1. Select the "Signing & Capabilities" tab.
1. In the "Team" dropdown, select your development team.
1. Change the bundle identifier to uniquely identify your project.
1. Select the "Build Phases" tab.
1. Expand the "Copy Bundle Resources" section.
1. Click the "+" button and add your `.love` zip file.
1. Check the "Copy items if needed" checkbox.
1. Select the "Create folder references" radio button.
1. Click the "Finish" button.

To run the app on a real device:

1. Attach the device to the mac with a USB cable.
1. Select the device from the device menu.
1. Build and run the project.

To size the window appropriately, the following settings in the `conf.lua` file:

To change the app icon, use Xcode to replace the images
in the file `Images.xcassets`.

```lua
  t.window.width = 590   -- half of 1179 (iPhone 14 Pro width)
  t.window.height = 1276 -- half of 2556 (iPhone 14 Pro height)
```

After making changes to the Lua code, enter `./bundle` again
to create a new `.love` file and then re-run the iOS project.

## Unorganized Content

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

The `love.draw` and `love.update` functions
are typically called 60 times per second (frame rate).
The `vsync` option can be set to synchronize the frame rate
with the refresh rate of the monitor.
It can be set in the `conf.lua` file with `t.window.vsync = 1`.
Alternatively it can be passed as an argument
to the `love.graphics.setMode` function.

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

