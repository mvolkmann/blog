---
eleventyNavigation:
  key: Better Touch Tool
layout: topic-layout.njk
---

{% aTargetBlank "https://folivora.ai/", "Better Touch Tool" %}
is a macOS tool for customizing keyboard shortcuts, mouse operation,
trackpad operation, touch bar operation, and more.
This is a commercial tool, but it is quite reasonably priced at \$8.50 USD.

This post documents customizations I have found useful.

## Mouse Position

If you use multiple monitors and you switch between apps using cmd-tab,
this tip is for you.
When the newly activated app is on a different monitor
than the previously activated app, the mouse cursor must be
manually moved between monitors in order to use it in the new app.
Better Touch Tool can automate this.

The mouse cursor can be moved to the center of the current window,
even if it is on a different monitor.
It seems there isn't a way to make this happen whenever any app is activated.
However, you can define an action to do this when a specific app is activated.
For example, to move the mouse cursor to the center of the active window
when the iTerm app is activated:

1. In the upper-left drop-down, select "Named & Other Triggers".
1. Click the "+" button in the "Groups & Top Level Triggers" column.
1. In the dialog that appears,
   select "General" and then "Specific App Did Activate".
1. Press the "Select App" button.
1. In the dialog that appears, select an app such as iTerm.
1. Click the "+" button in the "Actions Assigned to Selected Trigger" column.
1. In the dialog that appears,
   select "Other Mouse Actions" and then "Move Mouse to Position".
1. Click the "Record New Position" button.
1. In the "First select the anchor point" drop-down,
   select "Relative to: center of active window".
1. No values are needed for "X:" and "Y:", but 0 can be entered for both.
1. Press the "Save" button.

Now when you activate the app, perhaps using cmd-tab,
the cursor will move to the center of the active app window.

Another option is to assign a keyboard shortcut to this action
so pressing it does for the currently active app.
I assigned "fn esc" to this.

## Window Management

The current window can be moved and resized in many ways.
I created the following keyboard shortcuts
mapped to the indicated actions:

- fn l: "Resize Window to Left Third"
- fn c: "Resize Window to Middle Third"
- fn r: "Resize Window to Right Third"

- fn ←: "Resize Window to Left Quarter"
- fn C: "Maximize Window Left" and "Center Window"  
  resizes to half width, full height, and centers horizontally
- fn →: "Resize Window to Right Quarter"

- fn L: "Maximize Window Left Half"
- fn R: "Maximize Window Right Half"
- fn t: "Maximize Window to Top Half"
- fn b: "Maximize Window to Bottom Half"

- fn m: "Maximize Window"
- fn n: "Center Window on Next Monitor"
