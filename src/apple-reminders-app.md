---
eleventyNavigation:
  key: Apple Reminders App
layout: topic-layout.njk
---

### Overview

Apple Reminders is an excellent app for task management.
It runs on Mac computers (macOS), iPads (iPadOS),
iPhones (iOS), and Apple Watches (watchOS).
The data is synced across all these devices.

Reminders can be added using Siri,
which is typically faster than typing them.
Just speak phrases in forms like the following:

- Remind me to {task}
- Add {task} to my {list-name} list
- Remind me to {task} at {time} today
- Remind me to {task} at {time} on {day-or-date}
- Remind me to {task} on {date} at {time}
- Remind me to {task} when I get home
- Remind me to {task} when I get to work

By default, reminders are added to your default list.
This is specified in Settings for the Reminders app
and defaults to "Inbox".

The instructions below are primarily focused on
using Reminders in iPadOS.
The same functionality is available on macOS and iOS,
but may be organized slightly differently.

In iPadOS and macOS, the Reminders app displays a left nav area and a main area.
The left nav area displays large buttons for navigating to "pinned lists",
followed by the names of non-pinned lists.
Tapping a list button or name opens the list in the main area.

### Lists

Reminders can manage any number of task lists.

To create a new list:

- Tap "Add List" at the bottom of the left nav.
- Enter a name for the list.
- Select a list type (Standard, Groceries, or Smart List).
- Select a color.
- Select an icon.
  The smiling face emoji opens a picker for selecting any emoji.
- Tap "Done" in the upper-right.

To modify a list:

- Tap the list in the left nav.
- Tap the circled ellipsis in the upper-right of the main area.
- Tap "Show List Info".
- In the dialog that opens, edit the name, list type, color, and icon.
- Optionally tap "Convert to Smart List"
  to change a non-smart list to smart one.

To delete a list:

- Swipe the list left in the left nav and
  tap the "Delete" button that is exposed.
  This will delete all the reminders in the list.
- Tap "Delete" to confirm.

### Reminders

To add a reminder to a list:

- Tap the list in the left nav.
- Tap "New Reminder" at the bottom of the main area.
- Enter a description.
- Optionally enter a note below the description.
- Optionally tap the circled "i" on the right side of the reminder
  to open a dialog where more detail can be specified.
  This includes:

  - URL associated with the task
  - Date at which the task is due
  - Time at which the task is due
  - Tags
  - location at which you wish to be reminded
  - Flag (on or off) for easily viewing all flagged reminders
  - Priority (None, Low, Medium, or High)
  - List - change to move the reminder to a different list
  - Subtasks

Reminders with a date can be set to repeat at a specified interval.

To modify a reminder:

- Tap it.
- Modify the description.
- Modify the note.
- Tap the calendar icon at the top of the main area
  to change the associated date and/or time.
- Tap the map arrow icon at the top of the main area
  to change the location setting.
- Tap the flag icon at the top of the main area
  to toggle whether the reminder is flagged.
- Tap the hash icon at the top of the main area
  to add or remove a tag.
- Optionally tap the circled "i" to modify other aspects

To move a reminder to another list, drag it or:

- Tap it to select it.
- Tap the circled "i" on its right.
- In the dialog that opens, scroll down to "List".
- Tap "List".
- Tap the name of the list to which it should be moved.

To delete a reminder, swipe it left and
tap the "Delete" button that is exposed.
Deleted reminders are held in a list named "Recently Deleted"
that is always at the bottom of the left nav.
The reminders are not actually deleted until that list is cleared.
To clear it, tap the list, tap the circled ellipsis in the upper-right,
and tap "Delete All".

To search for a reminder containing specific text,
tap the "Search" input at the top of the left nav
and enter the text there.

### Calendar Integration

Reminders with dates appear in the iOS/iPad Calendar app (not in macOS)
with a checkbox in front of their description.
If they also have a time, the appear in the Calendar app at that time.

To make a reminder as completed from the Calendar app,
tap it open a dialog,
tap the checkbox to the left of the reminder description,
and tap "Done".

### Subtasks

Reminders can have subtasks.

To add a subtask to a reminder:

- Tap the circled ellipsis on the right side of a reminder to open a dialog.
- Tap "Subtask".
- Tap "Add Reminder".
- Enter descriptions for any number of subtasks.
- Tap "< Details" in the upper-left of the dialog.
- Tap "Done" in the upper-right of the dialog.

Alternatively, drag a reminder onto its intended parent reminder.

Subtasks appear indented below their parent task.
Parent tasks appear in bold.

One more approach is to drag a reminder to below its intended parent reminder,
swipe it right, and tap "Indent".
To undo the indent, swipe it right again and tap "Outdent".

Subtasks can be collapsed. When they are collapsed,
the parent task will display the number of hidden subtasks.

Marking all subtasks as completed does not mark the parent task as completed.
That must be done manually.

### Sorting

The reminders in a list can be sorted in ascending or descending order
based on their "Due Date", "Creation Date", "Priority", or "Title".

To change the sort order, select a list,
tap the circled ellipsis in the upper-right,
select "Sort By", and choose an option.
Select "Manual" to enable the reminders to be
dragged up and down to manually reorder them.

### Sections

The reminders in a list can be organized into sections.

To add a section to the currently selected list:

- Tap the circled ellipsis in the upper-right.
- Tap "New Section".
- Enter a name for the section.

Existing reminders will appear in a section titled "Others".

To associate a reminder with a given section, drag it to the section.
It's easiest to drag reminders by pressing to
the area to the right of its description.
This avoids being interpreted as an attempt to modify the description or note.

To reorder sections:

- Tap the circled ellipsis in the upper-right.
- Tap "Manage section".
- Drag section names up or down.

Alternative, in the list view, drag section names up or down.

To view sections of the currently selected list in columns like a Kanban board:

- Tap the circled ellipsis in the upper-right.
- Tap "View as Columns".
- Drag section names left or right to reorder the columns.

The Reminders app does not infer any kind of relative priority of reminders
based the position of the columns in which they appear.
However, the left to right order may be meaningful to you,
especially when modeling a Kanban board with section names like
"Backlog", "In Progress", "Ready for Review", and "Complete".

When viewing sections as colunns, the ellipsis menu
contains "Manage Columns" instead of "Manage Sections",
but the functionality is the same.

To stop viewing sections in columns:

- Tap the circled ellipsis in the upper-right.
- Tap "View as List".

### Groups

Related lists can be grouped into what appears as a folder in the left nav.

To create a new group, drag one list onto a related list,
enter a group title, and tap "Create".

Alternatively:

- Tap the ellipsis button that the top of the left nav.
- Tap "Edit Lists".
- Tap "Add Group" at the bottom of the left nav.
- Enter a group title.
- Tap "Include".
- Select the lists to be included in the group.
- Tap the "< New Group" button in the upper-left of the dialog.
- Tap "Create" in the upper-right of the dialog.

To delete a group, swipe it left and tap the "Delete" button that is exposed.

### Smart Lists

Smart lists display reminders that are also in non-smart lists
based specified critera.
The criteria can include tags,

To create a smart list:

- Tap "Add List" at the bottom of the left nav.
- Enter a name for the list.
- Change the list type to "Smart List".
- Tap "Edit Filters".
- Specify the criteria as any combination of tags, due date, due time,
  location, flag state, priority, and specific source lists.
  For example, tap "Priority" and select "High"
  to create a smart list that displays all the high priority reminders
  found in any non-smart list.

### Pinned Lists

Lists can be pinned which makes them appear
as buttons at the top of the left nav.
Unfortunately, groups cannot be pinned.

There is a default set of pinned lists.

To unpin a pinned list,
press on it until a popup appears and tap "Hide".

To pin an unpinned list,
press on it until a popup appears and tap "Pin".

To restore a list that was pinned by default
and is now unpinned,
tap the the circled ellipsis at the top of the left nav,
tap "Edit List", and tap the hollow circle
to the left of the name of an unpined list.

To change the order of pinned lists,
tap the the circled ellipsis at the top of the left nav,
tap "Edit List", and drag the pinned lists up and down.

### Today Focus

I find it useful to create a group of lists I can select
to see a prioritized list of tasks that are my goals for today.
To create this:

- Create a smart list named "Must Do" where the enabled filters are
  Date: Today and Priority: High.
- Create a smart list named "Should Do" where the enabled filters are
  Date: Today and Priority: Medium.
- Create a smart list named "Could Do" where the enabled filters are
  Date: Today and Priority: Low.
- Create a smart list named "Unprioritized" where the enabled filters are
  Date: Today and Priority: No Priority.
- Create a group named "Today Focus".
- Drag each of the lists created above into the group.
- Drag the "Today Focus" group to the top of the "My Lists" section
  in the left nav.

Every day, consider the reminders to be addressed that day.
For each one, set its Date to Today and its Priority to the appropriate value.
In the left nav, tap "Today Focus"
to see all of the tasks in the lists within that group,
ordered from highest to lowest priority.

### Sharing Lists

A specific list can be shared with other Apple users
so multiple people can view and update the reminders in it.
This is ideal for lists such a grocery lists.

To share a list the currently selected list:

- Tap the share button in the upper-right.
- Optionally tap "People you invite ..." to specify whether
  the selected person can add others to the sharing group.
- Tap "Messages" and enter a phone number,
  tap a button for a specific person,
  or tap "Mail".
- Send the message.

### Widgets

Three differently sized widgets can be added to the home screen.
These enable viewing reminders and marking them as completed
without going to the app.

To add a widget:

- Press down on the home screen.
- Tap the "Edit" button in the upper-left.
- Tap "Add Widget".
- Scroll down in the list of apps until "Reminders" appears and tap it.
- Swipe right and left to select one of the three available sizes.
- Tap the "Add Widget" button.

I don't personally find the use of these widgets compelling.
I'd rather just use the full app.

### Unorganized Content

- Describe creating an adding tags to items. This can be done either from the button or from the get info dialogue.
- select “show list info” to get a dialogue where you can change the color and icon for a list
- describe creating smart lists that are based on a set of tags; there is a button at the bottom of the show info. Dialogue to convert a normal list to a smart list.
- “View as Columns” adds the ability to drag items to different columns, which are actually sections
- Reminders can link to Mail, Notes, Safari, and Messages items (It’s easy to create a reminder from an item in those apps instead of adding the link inside the Reminders app? In other apps, do this through the share button. Tap the Reminders app. That opens a dialogue where remind your text can be entered. In macOS, you can drag items directly into the Reminders app. You can also do this in iOS, if you tap and hold an item, then swipe up from the bottom of the screen to select the Reminders app then drop the item onto a target list.)
- you can receive reminder notifications based on your location. To do this, select a reminder, tap the info button, turn on location, and select a location, such as home or work. It will be marked as showing the reminder either when you arrive or when you leave the location. To toggle that, tap the location again.
- There are several widgets you can add to your home screen for the Reminders app.
- Tapping the hollow circle on the left side of a reminder marks it as completed, but doesn’t delete it. You can still choose to view the completed items.
- Lists of type “Groceries” automatically categorize their items in categories like Beverages, Dairy, and Produce.
- Tap the three dots in the upper-right and then “Show Completed” to see items marked as completed (or purchased). Tap specific completed items to return them to the uncompleted state. This is great for updating a grocery list.
- When changing the icon for a list, tap the emoji icon to select any emoji (more detailed and multi-color)
- Can add arbitrary tags to a reminder or to all selected reminders.
- click a tag in the left nav to see all the reminders that have that tag.
- Describe how to rename and delete tags.
- Smart lists show all reminders with specified tags or priority. Their icon has a small gear in the lower right corner. They can be set to match on any or all conditions.
- A normal list can be converted to a smart list.
- Does every reminder need to reside in some non-smart list?
- To drag a reminder and not have the app think you want to modify its description, drag from the right of the description.
- Can assign tasks to specific people that are sharing a list.
- Can create a list template from an existing list.
- Can create a new list from a template. This is ideal for packing lists. Create the template once and create a new list from it for every trip. After the trip, delete the list, but keep the template for future trips.
- To create a template from an existing list, select a list, tap … in the upper right, and select “Save as Template”.
- To create a new list from a template, tap “Add List”, tap the “Templates” tab, tap a template, enter a name for the new list, and tap “Create”.
- To edit an existing template, tap ... at the top of the left column, select “Templates”, tap “i” to the right of a template name, tap “Edit Template”, makes changes, and tap “Done”. This will not modify existing lists that were created from the template.
- When you select a group in the left NAV, all the list inside it will appear in the main area grouped in sections that are the list names.
- The Today list can group its items by morning, afternoon, and evening. They seem to be automatically assigned at times of 9 AM, 3 PM, and 6 PM.
- In the info dialogue for a task you can add early reminders to be reminded before the due date.
- Lists can be sorted on many criteria and then ascending or descending order.
- New reminders are added to the currently selected list.
- Each reminder can optionally include notes, URL, date, time, early reminders (alerts), repeat interval, tags, location, flag, priority, images
- When a reminder is selected, the buttons at the top allow adding a date, time, location. flag, and tags.
- The default list for new reminders added with Siri is specified in the Settings for the Reminders app.
- The time at which you will be reminded about reminders that have a date, but no time, is specified in the Settings for the Reminders app.
- Including the number of reminders due today as a badge on the Reminders app icon is enabled in the Settings for the Reminders app. This seems to appear on the icon regardless of this setting.
- Reminders with dates or times appear in the calendar app with a checkbox in front of their description. To mark one as completed, tap it in the calendar, which opens a dialogue, tap the checkbox inside the dialogue, and tap done.
- Reminders in the calendar can be dragged to different times or days, and the reminder will be updated inside the Reminders app.
- Reminders that have a day, but not a time will appear at the top of a day in the calendar. Dragging it down to a specific time will set the time in the reminder.
- dragging reminders in the calendar is probably not supported for recurring reminders
- selecting a group of lists in the left NAV displays the reminders in each list within the group as separate sections.
