---
eleventyNavigation:
  key: Macintosh Emulators
layout: topic-layout.njk
---

## Overview

There are four primary ways to emulate older Macintosh computers.

- {% aTargetBlank "https://basilisk.cebix.net", "Basilisk II" %} for MacOS 0.x to 8.1
- {% aTargetBlank "https://infinitemac.org", "Infinite Mac" %} for MacOS 1.0 to 10.4
- {% aTargetBlank "https://www.gryphel.com/c/minivmac/", "Mini vMac" %} for MacOS 1.1 to 7.5.5
- {% aTargetBlank "https://sheepshaver.cebix.net", "SheepShaver" %} for MacOS 7.5.2 to 9.0.4

## Basilisk II

{% aTargetBlank "https://basilisk.cebix.net", "Basilisk II" %}
is an open source 68K Macintosh emulator.

For instructions on installing this, see
{% aTargetBlank "https://www.youtube.com/watch?v=QSWWZ4hkvVk",
"Emulate a Classic Macintosh Today! Basilisk II Tutorial!" %}.
The main parts of this for installing and using the emulator
in a modern Mac are described below.

### Downloads

The following files must be downloaded:

- one of these ISO files

  - Macintosh toast file #7 at
    https://macintoshgarden.org/apps/macintosh-mac-os-76/.
    The unzipped file name was "Mac OS 7.6.1.toast".

  - Macintosh OS 9.2.2 Universal toast file #1 at
    https://macintoshgarden.org/apps/mac-os-922-universal
    The downloaded file name was "macos-922-uni.zip".
    This unzips to a directory named "macos-922-uni"
    that contains the file "macos-922-uni.iso".

- one of these ROM files

  - Macintosh ROM at
    http://hampa.ch/pub/software/ROM/Macintosh%2068K/.
    Click the link "368CADFE - Macintosh IIci.7z".
    The file name was "368CADFE - Macintosh IIci.7z".
    Double-click this file to get unzipped file
    "368CADFE - Macintosh IIci.rom".

  - Macintosh ROM at
    http://hampa.ch/pub/software/ROM/Macintosh%2068K/.
    Click the link "F1ACAD13 - Macintosh Quadra 650.7z".
    The file name was "F1ACAD13 - Macintosh Quadra 650.7z".
    Double-click this file to get the unzipped file
    "F1ACAD13 - Macintosh Quadra 650.rom".

- one of these bootstrap image files

  - System 7.5.5 Drive Image from
    https://www.savagetaylor.com/wp-content/uploads/68k_Macintosh/Bootdisks/755_2GB_drive.zip
    The unzipped file name was 755_2GB_drive.dsk.

  - System 9.2.2 Drive Image from
    https://www.macintoshrepository.org/36322-random-mac-os-9-2-2-hard-drive-image-home-made-
    The unzipped file name was Mac_OS_9.2.2_HD_Restore.dmg.

- Basilisk II Emulator from
  https://www.emaculation.com/forum/viewtopic.php?t=7361&sid=c947aa32749beb939edf3fe187bb3352.
  I clicked the Download link after "SDL2 port, 28 February 2024, universal (x86_64 and arm64)".
  The unzipped file name was BasiliskII.app.

- Basilisk II GUI from
  https://www.emaculation.com/forum/viewtopic.php?f=6&t=10454.
  I clicked the link after "Download version 0.22".
  The unzipped file name was "Basilisk II GUI.app".

I moved the following files to a new directory in the
Documents/dev directory named "BasiliskII":

- 368CADFE - Macintosh IIci.rom
- 755_2GB_drive.dsk
- Basilisk II GUI.app
- BasiliskII.app
- Mac OS 7.6.1 toast

### Setup

In the Finder, locate the "Mac OS 7.6.1.toast" file, right-click it,
select "Get Info", check the Locked checkbox, and close the dialog.

Double-click the file "Basilisk II GUI.app".
A warning dialog will appear that asks
"Are you sure you want to open it?"
Click the "Open" button.

On the System tab:

- change "RAM Size (MB)" to 1024?.
- change "Mac Model ID" to .
- change "CPU Type" to 68040.
- check "Don't use CPU when idle"
- after "ROM File", click the "Browse..." button
- select the file "368CADFE - Macintosh IIci.rom" or
  "F1ACAD13 - Macintosh Quadra 650.rom" that was downloaded above

On the Graphics/Sound tab:

- change "Window Refresh Rate" to Dynamic
- optionally change the Width and Height values
  (640 x 480 may work best for some games)

On the Volumes tab:

- click the "Add..." button
- select the file "755_2GB_drive.dsk" or "Mac_OS_9.2.2_HD_Restore.dmg".
  that was downloaded above
- click the "Add..." button again
- select the file "Mac OS 7.6.1.toast" or "macos-922-uni.iso"
  that was downloaded above
- click the Save button
- click the Quit button

Start the emulator by double-clicking the file BasiliskII.app.

### File Sharing

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

## Infinite Mac

TODO: Add information about this.

The Mac OS 6.0.5, 7.6, 8.5, and 9 emulators all include HyperCard 2.4.1.

## Mini vMac

TODO: Add information about this.

## SheepShaver

To install in macOS:

- Browse https://www.emaculation.com
- Click the "E-Maculation wiki" link.
- Click the SheepShaver link.
- Under "Download the Latest Version",
  click the "SheepShaver for OSX/macOS" link.
- Under "Version 2.5, 25 January 2025, universal (x86_64 and arm64)",
  click the Download link to get the file "SheepShaver.app".
- Under "For the above version 2.5 builds",
  click the link after "SheepShaver folder".
- Browse https://www.redundantrobot.com
- Click the "SHEEPSHAVER TUTORIAL" link.
- Click the "New World PPC ROM" link.
- Browse https://winworldpc.com
- Click the Library link.
- Under "Apple/Mac", click the "Mac OS 9" link.
- Under "Available releases", click the 9.0 link.
  Supposedly SheepShaver does not support 9.1 or 9.2.
- Under Downloads, click the "Apple Mac OS 9.0.4 (ISO)" link.
- Click one of the Server links.
  I chose "Server 2" because it is closest to me.
- Create a directory named SheepShaver
  and move all the downloaded files into it.
  These include SheepShaver.app, the SheepShaver directory, newworld86.rom,
  and "Apple Mac OS 9.0.4.7z" (which may still be downloading).
- Right-click the newworld86.rom file and select "Get Info".
- Under "Name & Extension", change the name to "Mac OS ROM".
- Close the info dialog.
- Double-click the "Apple Mac OS 9.0.4.7z" file to unzip it.
- Delete the "Apple Mac OS 9.0.4.7z" file.
- Double-click the "Apple Mac OS 9.0.4" directory.
- Move the "Apple MacOS 9.0.4.iso" file up one directory.
- Delete the "Apple Mac OS 9.0.4" directory.
- Right-click the "Apple MacOS 9.0.4.iso" file and select "Get Info".
- Click the Locked checkbox.
- Close the info dialog.
- Double-click the SheepShaver.app file.

##

## Colors

To enable colors in the emulator:

- Open the Apple menu.
- Select "Control Panels".
- Double-click Monitors.
- Select the Colors radio button.
- Select 256.
- Close the Monitors window.
- Close the "Control Panels" window.

## HyperCard

HyperCard is located in Mac OS 7.6.1 ... CD Extras ... HyperCard Player.
Double-click "HyperCard Player".
A dialog will appear that says the Geneva and Palatino fonts
must be installed for text to display correctly in the stacks.
Press cmd-q to quit HyperCard.
