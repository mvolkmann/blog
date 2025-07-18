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

## Resources
