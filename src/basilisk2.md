---
eleventyNavigation:
  key: Basilisk II
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://basilisk.cebix.net", "Basilisk II" %}
is an open source 68K Macintosh emulator.

For instructions on installing this, see
{% aTargetBlank "https://www.youtube.com/watch?v=QSWWZ4hkvVk",
"Emulate a Classic Macintosh Today! Basilisk II Tutorial!" %}.
The main parts of this for installing and using the emulator
in a modern Mac are described below.

## Downloads

The following files must be downloaded:

- Macintosh ROM file #7 at
  https://macintoshgarden.org/apps/macintosh-mac-os-76/.
  The unzipped file name will be "Mac OS 7.6.1.toast".
- Macintosh ROM at
  http://hampa.ch/pub/software/ROM/Macintosh%2068K/.
  I selected the link "368CADFE - Macintosh IIci.7z".
  The file name will be "368CADFE - Macintosh IIci.7z".
  Double-click this file to get unzipped file "368CADFE - Macintosh IIci.rom".
- System 7.5.5 Drive Image from
  https://www.savagetaylor.com/wp-content/uploads/68k_Macintosh/Bootdisks/755_2GB_drive.zip
  The unzipped file name will be 755_2GB_drive.dsk.
- Basilisk II Emulator from
  https://www.emaculation.com/forum/viewtopic.php?t=7361&sid=c947aa32749beb939edf3fe187bb3352.
  I clicked the Download link after "SDL2 port, 28 February 2024, universal (x86_64 and arm64)".
  The unzipped file name will be BasiliskII.app.
- Basilisk II GUI from
  https://www.emaculation.com/forum/viewtopic.php?f=6&t=10454.
  I clicked the link after "Download version 0.22".
  The unzipped file name will be "Basilisk II GUI.app".

I moved the following files to a new directory
in the Applications directory named "Macintosh Emulator":

- 368CADFE - Macintosh IIci.rom
- 755_2GB_drive.dsk
- Basilisk II GUI.app
- BasiliskII.app
- Mac OS 7.6.1 toast

## Setup

In the Finder, locate the "Mac OS 7.6.1.toast" file, right-click it,
select "Get Info", check the Locked checkbox, and close the dialog.

Double-click the file "Basilisk II GUI.app".
A warning dialog will appear that asks
"Are you sure you want to open it?"
Click the "Open" button.

On the System tab:

- change "CPU Type" to 68040.
- check "Don't use CPU when idle"
- after "ROM File", click the "Browse..." button
- select the file "368CADFE - Macintosh IIci.rom" that was downloaded above

On the Graphics/Sound tab:

- change "Window Refresh Rate" to Dynamic
- optionally change the Width and Height values
  (640 x 480 may work best for some games)

On the Volumes tab:

- click the "Add..." button
- select the file 755_2GB_drive.dsk that was downloaded above
- click the "Add..." button again
- select the file "Mac OS 7.6.1.toast" that was downloaded above
- click the Save button
- click the Quit button

Start the emulator by double-clicking the file BasiliskII.app.

## Colors

To enable colors in the emulator:

- Open the Apple menu.
- Select "Control Panels".
- Double-click Monitors.
- Select the Colors radio button.
- Select 256.
- Close the Monitors window.
- Close the "Control Panels" window.

## File Sharing

To allow the emulator to share files with the host Mac:

- Quit "BasiliskII.app" if it is running.
- Create a directory in the host Mac that will be shared.
  I created a "share" directory in Applications/Macintosh Emulator.
- Double-click "Basilisk II GUI.app".
- Click the Volumes tab.
- After "Unix root", click the "Browse..." button.
- Select the directory to be shared.
- Click the Save button.
- Click the Quit button.
- Double-click "BasiliskII.app".
- Double-click the "Unix" drive to see the shared files.

For example, browse https://macintoshgarden.org, click the GAMES link,
download some sames, and move the files to the shared directory.

Extract `.sit` files inside the emulator using "Stuffit Expander",
not in the host Mac.

## HyperCard

HyperCard is located in Mac OS 7.6.1 ... CD Extras ... HyperCard Player.
Double-click "HyperCard Player".
A dialog will appear that says the Geneva and Palatino fonts
must be installed for text to display correctly in the stacks.
Press cmd-q to quit HyperCard.
