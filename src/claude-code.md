---
eleventyNavigation:
  key: Claude Code
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

## Overview

{% aTargetBlank
"https://docs.anthropic.com/en/docs/agents-and-tools/claude-code/overview",
"Claude Code" %} is "an agentic coding tool made by Anthropic.
Currently in beta as a research preview."

The cost is based on usage, not on a flat monthly fee.
Reports so far claim it is quite expensive,
perhaps four times the cost of using Cursor.

## Installing

npm install -g @anthropic-ai/claude-code

## Starting

- Create a directory for a project.
- cd to the directory.
- Enter `claude`.
- You'll be prompted to create an account.
- A credit card is required to buy credits.

## Security

## Functionality

## Paid vs. Free Versions

TODO: What features does the paid version add?

## Examples

### Web Component

I ran the following from my dev/web-components/wrec directory:

'Create a speedometer dial web component using the wrec library
defined in this directory. The web component should accept
the attribute "min" which defaults to zero,
the attribute "max" which defaults to 100, and
the attribute "value" which defaults to the value of the "min" attribute.'

I approved all of its suggestions
This took around 3 minutes to complete and used n tokens.

"The red hand in the dial always points down.
I want the minimum value to be around 7 o'clock on a clock face,
the maximum value to be around 5 o'clock on a clock face,
and the values between to progress clockwise between those."

"The css string includes references to this.value, this.min, and this.max.
The wrec library does not support that. Those references can only appear
in the html string and in methods of the class SpeedometerDial."

"The tick marks on the dial should begin at the min value and end at
the max value. Create tick marks between those at meaningful intervals.
For example, if the range is 0 to 100,
place a tick mark at every increment of 10.
If the range is 0 to 1000, place a tick mark at every increment of 100."

"The tick marks are still not at the correct locations.
The first tick mark representing the min value should be at 7 o'clock.
The last tick mark representing the max value should be at 5 o'clock.
The remaining tick marks should be between those,
progressing in clockwise order."

"The min tick mark is at 4 o'clock and the max tick mark is at 12 o'clock.
Update the code to place the min tick mark at 7 o'clock
and the max tick mark at 5 o'clock."

"The tick marks are off by 90 degrees.
Add 90 degrees to their current positions."

Finally the tick marks look correct!

'Move the static "css" and static "html" properties
to after the static "properties" property.'

"The tick marks are too close to the center of the dial.
Move them to be on the edge of the dial."

"Allow users to change the speedometer value by dragging the needle."

"The needle is now rotated 90 degrees to far. It was correct earlier."

"The needle is now rotated correctly, but now all the tick marks
need to rotated 90 degrees more. They were correct earlier."

"When I drag the needle, it doesn't remain under the mouse cursor.
It jumps to 90 degrees less than the cursor position and
remains 90 degrees behind while I drag the mouse."

"Now the needle position is off by 180 degrees when dragged.
Your correction went in the wrong direction."

"Now it is back to being 90 degrees behind the drag position.
Please add 90 degrees to that calculation."

I got this message:
"Credit balance too low ·
Add funds: https://console.anthropic.com/settings/billing"

I ran "/login" and logged in. That seems to have fixed the balance issue.

I ran the previous request again and it worked!

"In the display of the current value near the bottom of the dial,
do not show any decimal places."

"Now it always shows "NaN" for the curren value.
Please show the actual value rounded to the nearest integer."

"Add a small, red circle to the end of the needle and make that be
the only part of the needle that can be dragged to change the value."

"Add support for optional attributes to the SpeedometerDial component
that customize the background color, tick color, and needle color."

"Change the attributes you added (backgroundColor, tickColor, and needleColor)
to use kebab-case (background-color, tick-color, and needle-color)."

"Add small labels to the tick marks that indicate their value,
rounded to the nearest integer."

"I want small labels on all the tick marks,
not just the min, max, and major ones."

### wrec Library

"There have been many updates to the wrec library since you last examined it.
Summarize the new features."

It described six features, but only these were actually new features:

- Computed Properties
- Enhanced Data Binding (ex. value:input attribute)
- Property-to-attribute name caching

It missed the new feature that enables
JavaScript expressions in CSS variable values.

### Cycling App

I quit `claude`, cd'ed to a different directory,
created the directory "cycling-app", restarted `claude`,
and entered the following:

'Build a web app that tracks my bike workouts including
location, miles, times, and elevation gain.
Use the npm packages "wrec" and "htmx".'

It generated the app, but it has these issues:

- It is using htmx 1.9.12 instead of the latest version which is 2.0.6.
- It is trying to use wrec as a server-side library,
  but that is only for use in browsers.

"wrec is a client-side JavaScript library for building web components.
The file server.js try to use it as a substitute for something like Express,
but it cannot be used that way. Change server.js to use the Hono library.

The summary of the changes included this:

Client-side improvements (index.html):

- Added wrec script for web components
- Created a custom <workout-stats> web component
  using standard Web Components API

However, it is not actually using the wrec library and
the web component "workout-stats" is defined inside index.html
instead of it a separate JavaScript file.

"Move the definition of the web component "workout-stats"
from index.html to a separate JavaScript file
and use the wrec library to implement it."

"You are misunderstanding how to implement web components
using the wrec library. Please review the documentation at
https://mvolkmann.github.io/blog/wrec/ and the code examples at
https://github.com/mvolkmann/wrec/tree/main/examples.
Then revise the file components/workout-stats.js
to properly use the wrec library."

The implementation is much better now.

"After a workout is entered, clear the form."

"In the WorkoutStats web component, it is not necessary to specify the
value property for each of the properties because wrec automatically
assigns a default value based on the property type."

"Add a summary of the total miles, total time, and total elevation gain
of all workouts above the list of recent workouts."

The app looks good!

"Store the workouts in a SQLite database instead of a JSON file."

"Where is the workouts.db file stored?"

It told me the file is in the root directory of the project.
I had to restart the server to get it to create that file.

"Add all the data from the workouts.json file
that was used by the app previously to the SQLite database."

"Change the import of wrec in workout-stats.js to just import from "wrec",
not a path into the node_modules directory."

"Create a public GitHub repository for the app.
Add the appropriate files and push."

It created the local repository, but did not create the remote repository
or push to it.

"Change the GitHub user id from volkmannm to mvolkmann."

After this, I had to manually create the remote repository and push to it.

"Add ability to enter a date for each workout.
Set the date for all existing workouts to today in the database."

"Move the "Workout Statistics" section to above the "Recent Workouts" section.
Remove the total statistics from the "Recent Workout" section because
they just duplicate what is in the "Workout Statistics" section."

"Sort the workouts from newest to oldest."

"Add pagination to the list of workouts so only five are displayed at a time."

"Add the ability to filter and sort the list of workouts
on any of their properties."

"The UI for filtering and sorting looks good, but there is no
button to click that applies the specified filtering and sorting."

"The filtering is not working. For example, I entered 20 for "Min Miles"
and clicked the "Apply Filters" button, but all the workouts
are still displayed, including ones with less than 20 miles."

I had to restart the server to get it to work.
I seems like Claude Code should know when it makes
a change that requires a server restart and do that for me.

### Nim Game

"Create a web-based Nim game with columns containing 3, 5, and 7 items.
Allow a human to play against the computer.
Allow the human to choose which player makes the first move.
The computer player should use the strategy described at
https://wild.maths.org/play-win-nim to make its moves."

This generated a good version of the game in a single, index.html file.

"When it was the computers turn and the remaining items were
1, 2, and 1, the computer made the wrong move. It removed
2 items from the second column which allowed the human to win.
It should have removed only one item from the second column."

"Add the code for this app to the GitHub repository at
https://github.com/mvolkmann/nim-claude."

"Alphabetize the CSS rules based on their first letter character."

"Format the code using Prettier."

"Lint the code using oxlint and fix all errors."

No errors or warnings were generated by oxlint, so no changes were needed.

"Change the way that the human player indicates the items to be removed
so instead of entering a number for a specific column and
clicking a "Remove Items" button, the player clicks one of the green circles
to remove that item and all the items to its right."

"Simplify the UI by removing the text that gives the
column numbers and the number of items remaining. Instead,
just show the green circles remaining in each column."

"Remove the gray panels around each column of green circles."

"In the updateGameStatus function, instead of creating a new span element
by setting the innerHTML of the parent element,
update the class attribute and textContent of the existing span element."

"Currently the code uses the methods getElementById and querySelector
repeatedly to find elements that need to be updated.
Change this to find all those elements at the beginning and
save them in variables so the search for them is only performed once."

"It seems that the gameState property humanFirst is set, but never used.
If that property isn't needed, remove it."

"In the CSS properties, prefer rem units over px units.
Use values that are multiples of 0.5."

"Alphabetize the properties within each CSS rule."

"It seems that the div elements with class="column" are not needed
because the div elements with class="items" hold all the items.
Remove the unnecessary div elements."

I manually removed some excessive blank lines.

'Instead of using the gameState property "currentPlayer"
with a string value of "human" or "computer",
use the gameState property "computerMove" with a Boolean value.'

"Alphabetize the function definitions."

"Currently the updateDisplay function blows away all the div elements inside
itemsContainer and creates new ones from scratch every time it is called.
This is inefficient. Instead just remove the items selected in the last move
and update the remaining div elements as needed."

"When the remaining items are 0, 1, and 2, at it is the computers turn to move,
it removes 1 item from the column containing 2 which allows the human to win.
Fix this."

"In the calculateOptimalMove function, the if statement that
tests pilesWithOne contains two return statements.
Replace this with a single return statement that uses the ternary operator
to get the value of the count property being returned."

"If the computer wins, display a large sad face emoji.
If the human wins, fire a confetti cannon."

"Your confetti implementation wasn't great. Use an
existing library for that instead of implementing it yourself."

"Deploy this as a web app to Cloudflare."

This created the files worker.js and wrangler.toml for me.
I followed the instructions from Claude Code which included the following:

- Commit and push the new files.
- Browse https://dash.cloudflare.com/.
- Log in.
- It said to "Navigate to Pages in the sidebar", but that doesn't exist.
- It said to click "Create a project", but I only see "Create application",
  so I clicked that.
- It said to select "Connect to Git", but I only see "Import a repository".
  I clicked the "Get Started" button for that.
- I clicked the "GitHub" button which wasn't in the instructions.
- I signed in to my GitHub account.
- On the "Install Cloudflare Workers and Pages" page, I clicked my username.
- I selected the "Only select repositories" radio button.
- I selected my "nim-claude" repository.
- I clicked the "Install & Authorize" button.
- I clicked "Back to Compute (Workers) overview".
- I do not see a new application!
- I started over using "Pages" instead of "Workers".
- This worked! See https://nim-claude.pages.dev/.
- Pushing changes to the GitHub repository automatically redeploys the app.

"Make the UI responsive so it looks good on phones."

"Add a Help button that displays game instructions in a popup when clicked."

"In the mobile view, left justify the items
just like they are in the desktop view."

"The CSS rules are no longer alphabetized like they were before. Fix that.
Also, alphabetize the CSS rules inside the media query."

## Resources
