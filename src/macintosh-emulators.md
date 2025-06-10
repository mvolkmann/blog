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
- {% aTargetBlank "https://www.qemu.org", "QEMU" %}
- {% aTargetBlank "https://sheepshaver.cebix.net", "SheepShaver" %} for MacOS 7.5.2 to 9.0.4
- {% aTargetBlank "https://mac.getutm.app", "UTM" %}

## UTM

1. Browse the {% aTargetBlank "https://mac.getutm.app", "UTM home page" %}.
1. Click the "Download" button to download the file `UTM.dmg`.
1. Double-click the file `UTM.dmg`.
1. Drag the icon for `UTM.app` to the `Applications` folder.
1. Close the window.
1. In a Finder window under "Locations", eject `UTM`.
1. In the Finder window, click `Applications` in the left nav.
1. Double-click `UTM.app`.
1. Click the "Continue" button.
1. Download `Mac OS X 9.2.2.utm.zip` from {% aTargetBlank
   "https://www.macintoshrepository.org/85192-utm-powerpc-mac-os-archive-9-1-9-2-2-10-0-10-1-10-2-10-3-10-4-10-5-",
   "Macintosh Repository" %}.
   I gave up here. I can't find the file anywhere.

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

- one of these ISO files (exact copies of CDs)
  for installing system software

  - Mac OS 7.6.1 at
    https://macintoshgarden.org/apps/macintosh-mac-os-76/ (file #7).
    The unzipped file name was "Mac OS 7.6.1.toast".

  - Mac OS 8.1 at
    https://www.macintoshrepository.org/15953-mac-os-8-1-691-1912-a-cd-

  - Mac OS 9.2.2 Universal toast file #1 at
    https://macintoshgarden.org/apps/mac-os-922-universal
    The downloaded file name was "macos-922-uni.zip".
    This unzips to a directory named "macos-922-uni"
    that contains the file "macos-922-uni.iso".

  - Mac OS 8.1 at https://winworldpc.com/product/mac-os-8/81
    Click the link "Apple Mac OS 8.1 (ISO)".
    Click the server link closet to you.
    This takes several minutes to download the file `Apple Mac OS 8.1 (ISO).7z`.
    Double-click this file to get the directory `Apple Mac OS 8.1 (ISO)`
    which contains the file `MacOS8_1.iso`.

- one of these ROM files for emulating a specific Macintosh computer

  - http://hampa.ch/pub/software/ROM/Macintosh%2068K/.
    Click the link "368CADFE - Macintosh IIci.7z".
    Double-click this file to get unzipped file
    "368CADFE - Macintosh IIci.rom".

  - http://hampa.ch/pub/software/ROM/Macintosh%2068K/.
    Click the link "F1ACAD13 - Macintosh Quadra 650.7z".
    Double-click this file to get the unzipped file
    "F1ACAD13 - Macintosh Quadra 650.rom".

  - http://hampa.ch/pub/software/ROM/Macintosh%2068K/420DBFF3%20-%20Macintosh%20Quadra%20700.7z
    Click the link "420DBFF3 - Macintosh Quadra 700.7z".
    Double-click this file to get the unzipped file
    "420DBFF3 - Macintosh Quadra 700.rom".

  - https://www.redundantrobot.com/sheepshaver
    Click the link "Performa ROM".
    This downloads a directory named `1mbMacrom`
    that contains the file `PERFORMA.ROM`.

- one of these bootstrap image files

  - System 7.5.5 drive image from
    https://www.savagetaylor.com/wp-content/uploads/68k_Macintosh/Bootdisks/755_2GB_drive.zip
    The unzipped file name was 755_2GB_drive.dsk.

  - System 8.1 drive image from
    https://www.macintoshrepository.org/15953-mac-os-8-1-691-1912-a-cd-
    The unzipped file name was Mac OS 8.1.cdr.

  - System 9.2.2 drive image from
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

I moved the following files to the new directory
`Documents/dev/mac-emulators/BasiliskII`.

- Basilisk II GUI.app
- BasiliskII.app
- Mac OS 8.1.cdr
- MacOS8_1.iso
- PERFORMA.ROM

### Setup

In the Finder, rename `Mac OS 8.1.cdr` to `Macintosh HD.dsk`.

In the Finder, do the following for `MacOS8_1.iso` and `PERFORMA.ROM`:

- find the file in the Finder
- right-click it
- select "Get Info"
- check the Locked checkbox
- close the dialog

Double-click the file "Basilisk II GUI.app".
A warning dialog will appear that asks
"Are you sure you want to open it?"
Click the "Open" button.

On the System tab:

- change "RAM Size (MB)" to 1024
- change "Mac Model ID" to "Quadra 900 (MacOS 8.x)"
- change "CPU Type" to 68040
- check "Don't use CPU when idle"
- after "ROM File", click the "Browse..." button
- select the file "PERFORMA.ROM"

On the Graphics/Sound tab:

- Change "Window Refresh Rate" to Dynamic.
- Optionally change the Width and Height values.
  640 x 480 may work best for some games.

On the Volumes tab:

- Click the "Add..." button.
- Select a drive file such as `Mac OS 8.1.cdr`
- Click the "Add..." button again.
- Select an ISO file such as `MacOS8_1.iso`
- Create a drive where new software can be installed and files can be saved.
  - Click the "Create..." button.
  - Enter the name "Macintosh HD.dsk".
  - Click the Create button.
- Click the Save button.
- Click the Quit button.

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

The Mini vMac emulator supports Mac System 1 to 7.5.

The steps to install and configure this emulator are:

TODO: These steps are not working! Maybe the downloaded files are incorrect.

1. Download a ROM file for Mac OS 7.0.1 from the {% aTargetBlank
   "https://archive.org/details/AppleMacintoshSystem701", "Internet Archive" %}
   by clicking the "ISO IMAGE" link.
1. Rename the downloaded file `System_7_0_1.img` to `vMac.ROM`.
1. Download a bootable disk image for Mac OS 7.5.5 from {% aTargetBlank
   "https://winworldpc.com/product/mac-os-7/75", "WinWorld" %}.
   by clicking the "Apple Mac OS 7.5.5 [PowerPC Specific] (CD)" link
   at the bottom of the list.
1. Double-click the downloaded file `Apple Mac OS 7.5.5.7z`
   to get the directory `Apple Mac OS 7.5.5`
   that contains the file `MacOS 7.5.5.iso`.
1. Download `minivmac-36.04-mc64.bin.tar` from {% aTargetBlank
   "https://www.gryphel.com/c/minivmac/", "Mini vMac" %} web site.
1. Double-click the downloaded `.tar` file to get the file `Mini vMac.app`.
1. Double-click the file `Mini vMac.app`.
1. Drag the ROM file `System_7_0_1.img` onto the Mini vMac window.
1. Drag the bootable disk image file `` onto the Mini vMac window.

## SheepShaver

For instructions on installing this, see the YouTube videos
{% aTargetBlank "https://www.youtube.com/watch?v=4kiACxmEe6Q&t=607s",
"Mac OS 9 on SheepShaver" %} and
{% aTargetBlank "https://www.youtube.com/watch?v=XQAf7GII9NY",
"How To Install Mac OS 9 In OS X Using SheepShaver" %}.

The steps for installing this in macOS are summarized below.

1. Browse https://www.emaculation.com
1. Click the "E-Maculation wiki" link.
1. Under "Welcome", click the SheepShaver link.
1. Under "Download the Latest Version",
   click the "SheepShaver for OSX/macOS" link.
1. Under "Version 2.5, 25 January 2025, universal (x86_64 and arm64)",
   click the Download link.
1. Double-click the downloaded file to get the file "SheepShaver.app".
1. Under "For the above version 2.5 builds",
   click the link after "SheepShaver folder".
1. Double-click the downloaded file to get the folder "SheepShaver scripts".
1. Browse https://www.redundantrobot.com
1. Click the "SHEEPSHAVER TUTORIAL" link.
1. Click the "New World PPC ROM" link.
1. Browse https://winworldpc.com
1. Click the Library link.
1. Under "Apple/Mac", click the "Mac OS 9" link.
1. Under "Available releases", click the 9.0 link.
   Supposedly SheepShaver does not support 9.1 or 9.2.
1. Under Downloads, click the "Apple Mac OS 9.0.4 (ISO)" link.
1. Click one of the Server links.
   I chose "Server 2" because it is closest to me.
1. Create a directory named SheepShaver
   and move all the downloaded files into it.
   These include SheepShaver.app, the "SheepShaver scripts" directory,
   newworld86.rom, and "Apple Mac OS 9.0.4.7z" (which may still be downloading).
1. Right-click the newworld86.rom file and select "Get Info".
1. Under "Name & Extension", change the name to "Mac OS ROM".
1. Close the info dialog.
1. Double-click the "Apple Mac OS 9.0.4.7z" file to unzip it.
1. Delete the "Apple Mac OS 9.0.4.7z" file.
1. Double-click the "Apple Mac OS 9.0.4" directory.
1. Move the "Apple MacOS 9.0.4.iso" file up one directory.
1. Delete the "Apple Mac OS 9.0.4" directory.
1. Right-click the "Apple MacOS 9.0.4.iso" file and select "Get Info".
1. Click the Locked checkbox.
1. Close the info dialog.
1. Double-click the SheepShaver.app file.
1. Click the SheepShaver menu and select "Settings...".
1. After "ROM File:", click the "Browse..." button.
1. Select the "Mac OS ROM" file.
1. Create a directory that will be used to share files with the emulator,
   perhaps named "Mac OS Share".
1. After "Unix Root:", click the "Browse..." button.
1. Select the directory that was just created.
1. Change "RAM Size (MB):" to 512.
1. Click the "Create..." button.
1. Change "Save As:" to Mac_Disk.
1. Change "Volume Size (MB)" to 10000.
1. Click the Save button.
1. Click the "Add..." button.
1. Select the "Apple MacOS 9.0.4.iso" file.
1. Click the "Open" button.
1. Uncheck the CDROM checkbox for the "Apple MacOS 9.0.4.iso" file.
1. Click the "Audio / Video" tab.
1. Change "Refresh Rate:" to Dynamic.
1. Change "Width:" to Maximum.
1. Change "Height:" to Maximum.
1. Click the "Miscellaneous" tab.
1. Uncheck "Ignore Illegal Instructions".
1. Change "Mouse Wheel Function:" to "Page Up/Down".
1. Change "Lines to Scroll:" to 1.
1. Click the "Save and Quit" button.
1. Select "Force Quit..." from the Apple menu (or press cmd-option-esc)
   and force quit the SheepShaver app.
1. Double-click the SheepShaver.app file again.
1. Wait for a dialog to appear that says "This disk is unreadable ...".
1. Change "Name:" from untitled to "MacOS_HD".
1. Change "Format:" to "Mac OS Extended 9.7 GB".
1. Click the Initialize button.
1. Click the Continue button.
1. In the Finder dialog that appears, double-click "Mac OS 9 Install".
1. Click the Continue button.
1. Click the Select button.
1. Click the Continue button.
1. Click the Continue button.
1. Click the Agree button.
1. Click the Start button. This will run for around five minutes.
1. Click the Continue button.
1. In the Finder window, double-click "Mac OS 9.0.4 Update Install".
1. Click the Select button.
1. Click the Continue button.
1. Click the Agree button.
1. Click the Start button.
1. Click the Quit button.
1. Click and hold on the Special menu.
1. Select Restart.
1. In the "Mac OS Setup Assistant" window,
   click the right triangle in the lower-right to advance to page 2.
1. Click the right triangle again to advance to page 3.
1. Under "What is your name?", enter your name.
1. Click the right triangle again to advance to page 4.
1. If necessary, correct whether you are currently
   observing daylight savings time.
1. If necessary, correct the time and date.
1. Click the right triangle again to advance to page 5.
1. Select the nearest city ("Chicago, U.S.A." for me).
1. Close the window.
1. Click the Quit button.
1. In the Finder window, double-click "Adobe Software" folder.
1. Double-click the "Install Adobe Acrobat Reader 4.0" file.
1. Double-click the "Install Adobe Acrobat Reader 4.0" folder.
1. Double-click the "Adobe Acrobat Installer" file.
1. Click the Continue button.
1. Click the Install button.
1. Click the Quit button.
1. Close all the Finder windows.
1. Click the SheepShaver menu and select "Settings...".
1. Uncheck CDROM checkbox for the "Apple MacOS 9.0.4.iso" file.
1. Click the Remove button to remove the file "Apple MacOS 9.0.4.iso"
   since we are finished using it to install the operating system.
1. Click the "Save and Quit" button.
1. Click the "Shut Down" button.
1. Double-click the SheepShaver.app file.
1. Click the SheepShaver menu and select "Settings...".
1. Click the Miscellaneous tab.
1. Click the "Save and Quit" button.
1. Click the "Shut Down" button.
1. Double-click the SheepShaver.app file.
1. There will now be a "Unix" drive that is empty.
   Files placed in the "Mac OS Share" directory of the host
   will appear in the "Unix" drive.
1. To adjust the window size, select Apple menu ... Control Panels ... Monitors
   and select another resolution.
1. To enable sound, select Apple menu ... Control Panels ... Sound,
   select "Output", and select "Built-in".
1. In SheepShaver, click the Apple menu
   and select Control Panels ... Appearance.
1. On the Themes tab, optionally change the theme or keep "Mac OS Default".
1. On the Sound tab, change "Sound tracks:" to "Platinum Sounds".
1. On the Desktop tab, optionally select a different pattern and
   click the "Set Desktop" button, or keep "Mac OS Default".
1. Close the Appearance window.
1. Drag all the aliases on the desktop to the trash can.
1. From the Special menu, select "Empty Trash..." and click the OK button.
1. In SheepShaver, click the Apple menu and select Favorites ... www.apple.com.
   This will launch Microsoft Internet Explorer 4.5 and open an error dialog
   that says "Security failure. The server reply is invalid."
   "Microsoft Internet Explorer" can also be found in MacOS_HD ... Internet.
1. In the address bar, enter "http://macintoshgarden.org/"
   or "http://macintoshrepository.org" to download apps and games.

The last version of HyperCard is 2.4.1.
The last Mac OS version that supports this is 9.2.2.
To install HyperCard:

1. Browse https://macintoshgarden.org/apps/hypercard-241.
1. Download the HyperCard-241.iso file using link #2.
1. Move it to the "Mac OS Share" directory.
1. In SheepShaver, double-click the Unix drive.
1. Double-click the HyperCard-241.iso file to create a HyperCard drive.
1. Double-click the HyperCard drive.
1. Double click HyperCard Software Installer.
1. Click the Continue button three times.
1. Click the Agree button.
1. Click the Select button.
1. Click the Start button. This will run for a few minutes.
1. Click the Restart button.
1. The folder "HyperCard 2.4" will be at the top of "MacOS_HD".
1. Drag it to the Applications folder.
1. To launch HyperCard, open the "HyperCard 2.4" folder
   and double-click HyperCard.

## Full Screen Mode

To toggle full screen mode, press ctrl-return.
This is highly recommended because it enables resizing windows
(such as script editors) that have become taller than the SheepShaver window.
However, when in full screen mode the mouse cursor cannot be
moved outside of SheepShaver, which prevents accessing other applications.

## Colors

To enable colors in the emulator:

- Open the Apple menu.
- Select "Control Panels".
- Double-click Monitors.
- Select the Colors radio button.
- Select 256.
- Close the Monitors window.
- Close the "Control Panels" window.

## Zooming In

One way to increase the size of an app (such as HyperCard)
running in an emulator from a modern Mac host is to:

- open "System Settings"
- select Accessibility
- select Zoom
- toggle the "Use keyboard shortcuts to zoom" option on
- open the emulator
- launch the app
- press cmd-option-= (plus key) to zoom in
- press cmd-option-minus to zoom out

## HyperCard

HyperCard is located in Mac OS 7.6.1 ... CD Extras ... HyperCard Player.
Double-click "HyperCard Player".
A dialog will appear that says the Geneva and Palatino fonts
must be installed for text to display correctly in the stacks.
Press cmd-q to quit HyperCard.

## Resources

- {% aTargetBlank "https://www.emaculation.com/doku.php", "Emaculation.com" %}
- {% aTargetBlank "https://archive.org", "Internet Archive" %}
- {% aTargetBlank "http://obsolete.macfixer.com/vintage-software/", "MacFixer" %}
- {% aTargetBlank "http://macintoshgarden.org", "Macintosh Garden" %}
- {% aTargetBlank "https://www.macintoshrepository.org", "Macintosh Repository" %}
- {% aTargetBlank "https://rescuemyclassicmac.com", "RescueMyClassicMac" %}
- {% aTargetBlank "https://vintagemacmuseum.com/resources/mac-software/", "The Vintage Mac Museum" %}
