---
eleventyNavigation:
  key: LÖVE
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://love2d.org/", "LÖVE" %} is
"an _awesome_ framework you can use to make 2D games in Lua.
It's free, open-source, and works on Windows, Mac OS X, Linux, Android and iOS."

LÖVE can be downloaded from the previous link.

LÖVE use LuaJIT which mostly only supports Lua 5.1.
It includes some Lua 5.2 and 5.3 features.

## Resources

- {% aTargetBlank "https://github.com/love2d-community/awesome-love2d",
  "Awesome Löve" %} - "A categorized community-driven collection of
  high-quality, awesome LÖVE libraries, projects, and resources."

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

## Getting Started

To creating a new LÖVE app:

- Create a directory for the app.
- Create the file `main.lua` in this directory.
- Define the functions `love.load`, `love.update`, and `love.draw` in this file
- Create the file `conf.lua` in this directory.
- Add content like the following to configure the app:

  ```lua
  -- This is not required, but prevents warnings in code editors
  -- about "love" being undefined.
  local love = require "love"

  function love.conf(t)
    t.modules.joystick = false -- when not supporting joystick use
    t.title = "Monkey Nim"
    t.version = "11.4"    -- version of Love2D
    -- When t.window.width and t.window.height are not set,
    -- love.graphics.getDimensions() returns the screen width and height.
    -- t.window.width = 393 -- iPhone 14 Pro width
    -- t.window.height = 852 -- iPhone 14 Pro height
    t.window.resizable = true -- defaults to false
  end
  ```

For a list of additional configuration options, see {% aTargetBlank
"https://love2d.org/wiki/Config_Files", "Config Files" %}.

LÖVE apps always define the functions
`love.load()`, `love.draw()`, and `love.update(dt)` in their `main.lua` file.
The `love.load()` function performs initial game setup.
The `love.draw()` function specifies what should be
drawn on the screen at any point in time.
The `love.update(dt)` function implements the game logic.

Variables whose values never change can be defined at the top level
before the `love.load` function.
Variables whose values change while the app runs
can be declare as `local` at the top level,
but should be set in the `love.load` function.

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

## Running

The initial source file must be named `main.lua`.
When the LÖVE app is run without this file,
a window matching the screenshot below is displayed.

<img alt="LÖVE No Game" style="width: 80%"
    src="/blog/assets/love2d-no-game.png?v={{pkg.version}}"
    title="LÖVE No Game">

Note that the kite tail on the balloon says "NO GAME".

There are many ways to run a LÖVE app:

- Start from a terminal window.

  1. Add the following in `~/.zshrc`:

  ```bash
  alias love="/Applications/love.app/Contents/MacOS/love"
  ```

  1. Open a new terminal session.
  1. `cd` to your project directory.
  1. Enter `love .`

- Bundle the app files (as described in the next section)
  and double-click the bundle file.

- Drag the app directory onto the Love application icon.
  In macOS, drag from the Finder.

- Use VS Code.

  1. Start VS Code and open the project directory.
  1. Install the extension "Love2D Support" from Pixelbyte Studios.
  1. Click the gear icon and select "Extension Settings".
  1. Change "Pixelbyte > love2d: Path" to
     "/Applications/love.app/Contents/MacOS/love".
  1. Open the `main.lua` file in an editor tab.
  1. Press cmd-l to run the game.

## Bundling

The source code and asset files (fonts, sounds, and images) can be
bundled into a single file that can be double-clicked to run the app.
To do this, create a zip file containing all the required files
and make its extension `.love`.

Some projects contain files in the top project directory
(such as shell scripts and documentation)
that should not be included in the bundle file.
To accommodate this, consider moving all the `.lua` and asset files
into a subdirectory named `src`.

Create a shell script named `bundle` in the top directory
of the project containing the following where
`{project-name}` is replaced by the name of the project:

```bash
#!/usr/bin/env zsh
rm -f {project-name}.love
pushd src
zip -r ../{project-name}.love *
popd
```

Make the `bundle` script executable by entering `chmod a+x bundle`.

Enter `./bundle` to bundle the app into a `.love` file.
Double-click this file to run the app locally.

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

The current color can be set using
`love.graphics.setColor(red, green, blue, alpha)`
where all the arguments are floating point numbers from 0 to 1.
The color parameters are required.
The `alpha` parameter can be omitted and defaults to 1.
This remains in effect until it is changed again
and affects many things that are drawn including text and shapes.

The `setColor` function can also be passed an array-like table of color values.

The following function can be used to convert color values
in the range 0 to 255 into the range 0 to 1:

```lua
local function rgb(red, green, blue)
  return red / 255, green / 255, blue / 255
end
```

Consider defining all the colors that the app will need
on startup and holding them in a table. For example:

```lua
return {
  black = { 0, 0, 0 },
  blue = { 0, 0, 1 },
  brown = { rgb(150, 75, 0) },
  gray = { rgb(150, 150, 150) },
  green = { 0, 1, 0 },
  purple = { rgb(148, 0, 211) },
  red = { 1, 0, 0 },
  white = { 1, 1, 1 },
  yellow = { 1, 1, 0 }
}
```

To use one of these colors later,
use `love.graphics.setColor(colors.yellow)`.

The following function can be used to generate a random color:

```lua
function randomColor()
  local r = math.random()
  local g = math.random()
  local b = math.random()

  -- This makes sure the color isn't too dark.
  local total = r + g + b
  return total >= 1 and { r, g, b } or randomColor()
end
```

## Fonts

LÖVE uses the font "Bitstream Vera Sans" by default.
Any font types supported by {% aTargetBlank "https://freetype.org/",
"loFreeType 2" %} can be also be used.
This includes TrueType, WOFF, and many others.

To create a font object, use
`local myFont = love.graphics.newFont(fontFilePath, fontSize)`.

To change the current font, use `love.graphics.setFont(myFont)`.
This remains in effect until it is changed again.
It affects what is rendered by `love.graphics.print(text, x, y)`.
and `love.graphics.printf(text, x, y, limit, align)`.
The latter differs in that it provides word wrapping and alignment.

Consider creating all the `Font` objects that the app will need
on startup and holding them in a table. For example:

```lua
local fonts = {
  default = g.newFont("Pangolin-Regular.ttf", 18),
  button = g.newFont("Pangolin-Regular.ttf", 30)
}
```

To use one of these fonts later,
use `love.graphics.setFont(fonts.button, x, y)`.

## Images

LÖVE supports the image formats jpg, png, bmp, and many others.
It does not support gif files

To create an `Image` object:

```lua
local monkey = love.graphics.newImage('images/monkey.png')
```

To draw an image at a given x,y location,
use `love.graphics.draw(monkeyImage, x, y)`.

The `draw` function also takes argments for the rotation,
x scale, y scale, origin x offset, origin y offset.

Consider creating all the `Image` objects that the app will need
on startup and holding them in a table. For example:

```lua
local image = {
  monkey = love.graphics.newImage('images/monkey.png'),
  banana = love.graphics.newImage('images/banana.png'),
}
```

To draw one of these images later,
use `love.graphics.draw(images.monkey, x, y)`.

## Sounds

LÖVE supports the sound formats mp3, ogg, wav, and many others.

To create a sound, specify its file path and source type as follows:

```lua
local mySound = love.audio.newSource("sounds/my-sound.mp3", "static")
```

The last argument is the source type.
"static" is preferred for short sounds.
"stream" is the default and is preferred for longer sounds
because it avoids holding the entire sound in memory.

To play a sound, use `mySound:play()`.

To cause a sound to loop continuously,
use `mySound:setLooping(true)` before playing it.

To stop a sound, use `mySound:stop()`.

To temporarily pause a sound, use `mySound:pause()`.
To resume it later, use `mySound:resume()`.

Consider creating all the sound `Source` objects that the app will need
on startup and holding them in a table. For example:

```lua
local sounds = {
  success = love.audio.newSource("sounds/success.mp3", "static"),
  failure = love.audio.newSource("sounds/failure.mp3", "static"),
  song = love.audio.newSource("sounds/song.mp3")
}
```

To play one of these sounds later, use `sounds.success:play()`.

To allow multiple occurrences of the same sound to play simultaneously,
clone it and play the clone. For example:

```lua
local clone = sounds.failure:clone()
clone:play()
```

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

## Mouse Cursor

The following code hides the default mouse cursor
and display an image in its place.

```lua
love.mouse.setVisible(false)

local x, y = love.mouse.getPosition()
local h = monkeyImage:getHeight()
local w = monkeyImage:getWidth()
g.draw(monkeyImage, x - w/2, y - h/2)
```

## Background Scrolling

A single image can be scrolled across the background in any direction.
It is best if the image being scrolled is pre-scaled
so one of its dimensions matches the corresponding window dimension.
For example, for horizontal scrolling
the image height should match the window height.
It order to achieve seamless scrolling,
the opposite edges should match.

For example:

```lua
local love = require "love"

local backgroundPosition
local backgroundSpeed = 100
local g = love.graphics
local image = love.graphics.newImage("background.jpg")

-- When t.window.width and t.window.height are not set,
-- love.graphics.getDimensions() returns the screen width and height.
local windowWidth, windowHeight = g.getDimensions()

function love.load()
    backgroundPosition = 0
end

function love.draw()
    -- To scroll vertically ...
    -- love.graphics.draw(image, 0, backgroundPosition)
    -- love.graphics.draw(image, 0, backgroundPosition - windowHeight)

    -- To scroll horizontally ...
    g.draw(image, backgroundPosition, 0)
    g.draw(image, backgroundPosition - windowWidth, 0)
end

function love.update(dt)
    -- To scroll vertically ...
    -- backgroundPosition = (backgroundPosition + backgroundSpeed * dt) % image:getHeight()

    -- To scroll horizontally ...
    backgroundPosition = (backgroundPosition - backgroundSpeed * dt) % windowWidth
end

function love.keypressed(key)
    if key == "escape" then
        love.event.quit()
    end
end
```

## Calling Functions Later

The following code can be used to schedule functions
to be called later.

```lua
local socket = require "socket"

local laters = {}

-- This schedules a function to run a given number of seconds later.
function later(fn, seconds)
  -- os.time() only returns the time in seconds as an integer.
  -- socket.gettime() is similar but returns
  -- a floating point value for subsecond precision.
  laters[fn] = socket.gettime() + seconds
end

-- Call this near the beginning of love.update.
function processLaters()
  for fn, time in pairs(laters) do
    if time <= os.time() then
      laters[fn] = nil
      fn()
    end
  end
end
```

## Tweening

Tweening is the process of gradually changing a value over time.
It is often used to implement animations.
The values changed can include the coordinates of an item on the screen,
the size of an item, a font size, a sound volume, and more.
Typically an easing function is selected to control the rate of change
over the duration.

There are several libraries that implementing tweening.
One example is {% aTargetBlank "https://github.com/kikito/tween.lua",
"tween.lua" %}.

To install this, copy the `tween.lua` from the link above into your project.

The following example tweens the location of a piece of text.
The text decreases in font size as it drops down the screen
and bounces when it reaches the bottom.

```lua
-- Describe the subject which is the thing to be tweened.
-- x and y define the starting location.
local label = { text = "TWEENING!", fontSize = 60, x = 100, y = 0, }

-- Define the tween.  The arguments are the duration in seconds,
-- the subject, the target, and the easing function.
-- The target is the set of properties be modified during the tween.
-- Any number of properties can be modified.
local labelTween = tween.new(
  4,
  label,
  { fontSize = 20, y = windowHeight - 28 },
  'outBounce'
)

-- Draw the thing being tweened using its current properties
-- in the "love.draw" function.
love.graphics.setFont(g.newFont("Pangolin-Regular.ttf", label.fontSize))
love.graphics.print(label.text, label.x, label.y)

-- Update the properties of the thing being tweened
-- in the "love.update" function.
labelTween:update(dt)
```

## Physics

The `love.physics` module provides many functions that wrap the functionality
of {% aTargetBlank "https://box2d.org/", "Box2D" %} which is
a C++-based 2D physics engine.
Box2D was created by Erin Catto who works at Blizzard Games.
Box2D is used by many games including Angry Birds.

For an example project that uses `love.physics`, see {% aTargetBlank
"https://github.com/mvolkmann/lua-examples/tree/main/love/monkey-nim",
"Monkey Nim" %}.

## Deploying

LÖVE apps can be deployed to Windows, macOS, Linux, Android, and iOS.
Details can be found at {% aTargetBlank
"https://love2d.org/wiki/Game_Distribution", "Game Distribution" %}.

### Web

One way to deploy a LÖVE app to the web is to use {% aTargetBlank
"https://github.com/Davidobot/love.js", "love.js" %}.
This uses {% aTargetBlank "https://emscripten.org/", "Emscripten" %}
to compile the C code from Lua and LÖVE to WebAssembly
so it can be run in web browsers.

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

  app.use(
    express.static('../web', {
      setHeaders: res => {
        res.set('Cross-Origin-Opener-Policy', 'same-origin');
        res.set('Cross-Origin-Embedder-Policy', 'require-corp');
      }
    })
  );

  const PORT = 1919;
  app.listen(PORT, () => console.log('browse localhost:' + PORT));
  ```

Start the web server and run the game with the following steps:

- Enter `node server.js`
- Browse localhost:1919

If running the app results in the error
"Uncaught RangeError: offset is out of bounds",
try adding the `--memory` flag to the `npx love.js` command
in order to allow it to use more memory.
The default is 16,777,216 bytes.

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
1. Download iOS source by clicking the "iOS source" link
   in the Download section of the
   {% aTargetBlank "https://love2d.org/", "LÖVE" %} home page.
   <img alt="LÖVE iOS Source" style="width: 100%"
    src="/blog/assets/love2d-ios-source.png?v={{pkg.version}}"
    title="LÖVE iOS Source">
1. Double-click the downloaded file to unzip it.
1. Move this directory to its desired location and rename it.
1. In the Finder, navigate to `platform/xcode`.
1. Double-click the file `love.xcodeproj` to open it in Xcode.
1. Select a simulator device.
1. Build and run the app.
1. In the simulator, tap "No-game screen"
   to see the default "NO GAME" screen.

The following steps customize the default project to run your game.

1. Enter `./bundle` to bundle the LÖVE project into a `.love` file.
   (See the "Bundling" section above.)
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
1. Click the "+" button, click the "Add Other..." button,
   and add your `.love` zip file.
1. Check the "Copy items if needed" checkbox.
1. Select the "Create folder references" radio button.
1. Click the "Finish" button.

To run the app on a real device:

1. Attach the device to the mac with a USB cable.
1. Select the device from the device menu.
1. Build and run the project.

To size the window appropriately, add the following
settings in the `conf.lua` file:

```lua
  t.window.width = 393  -- third of 1179 (iPhone 14 Pro width)
  t.window.height = 852 -- third of 2556 (iPhone 14 Pro height)
```

To change the app icon, use Xcode to replace the images
in the file `Images.xcassets`.

```lua
  t.window.width = 590   -- half of 1179 (iPhone 14 Pro width)
  t.window.height = 1276 -- half of 2556 (iPhone 14 Pro height)
```

After making changes to the Lua code, enter `./bundle` again
to create a new `.love` file and then re-run the iOS project.

If no sounds play, it is likely because the phone mute switch is on.

## Optimizations

When comparing the distance between two points to some value,
compare the square of the distance.
This removes the need to use the `math.sqrt` function
which can hurt game performance.
