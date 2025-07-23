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

"There have been many updates to the wrec library since you last examined it.
Summarize the new features."

It described six features, but only these were actually new features:

- Computed Properties
- Enhanced Data Binding (ex. value:input attribute)
- Property-to-attribute name caching

It missed the new feature that enables
JavaScript expressions in CSS variable values.

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

## Resources
