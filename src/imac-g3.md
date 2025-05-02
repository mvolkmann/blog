---
eleventyNavigation:
  key: iMac G3
layout: topic-layout.njk
---

## Overview

I purchased a used iMac G3 on April 30, 2025 for $200.
It chose this model because it is one of the newest Macintosh computers
that can run the HyperCard application.
I'm learning about HyperCard so I can attempt to recreate it as web application.

The specs for this computer can be found at {% aTargetBlank
"https://everymac.com/systems/apple/imac/specs/imac_dv_se_500.html",
"everymac.com" %}.
The highlights are:

- Apple iMac G3/500 DV SE (Summer 2000)
- Graphite color
- 15" CRT display
- native resolution 800x600
- 30 GB hard drive
- PC100 SDRAM 128 MB
- processor type PowerPC 750
- processor speed 500 MHz
- 4X DVD ROM slot loading
- Ethernet port
- 2 USB-A ports
- 2 Firewire 400 ports
- AirPort card for wireless internet using WEP
- original price $1499

This is one of the cleanest iMac G3 computers I was able to find,
as evidenced by the photos below.

<img alt="iMac G3 full" style="width: 90%"
  src="/blog/assets/iMac-G3-full.jpg?v={{pkg.version}}">

<img alt="iMac G3 top" style="width: 90%"
  src="/blog/assets/iMac-G3-top.jpg?v={{pkg.version}}">

<img alt="iMac G3 bottom" style="width: 90%"
  src="/blog/assets/iMac-G3-bottom.jpg?v={{pkg.version}}">

<img alt="iMac G3 left" style="width: 90%"
  src="/blog/assets/iMac-G3-left.jpg?v={{pkg.version}}">

<img alt="iMac G3 right" style="width: 90%"
  src="/blog/assets/iMac-G3-right.jpg?v={{pkg.version}}">

<img alt="iMac G3 ports" style="width: 90%"
  src="/blog/assets/iMac-G3-ports.jpg?v={{pkg.version}}">

<img alt="iMac G3 back" style="width: 90%"
  src="/blog/assets/iMac-G3-back.jpg?v={{pkg.version}}">

<img alt="iMac G3 keyboard and mouse" style="width: 90%"
  src="/blog/assets/iMac-G3-keyboard-mouse.jpg?v={{pkg.version}}">

## Setup

Below are the steps I took to setup this computer.

- Reinstall the operating system to run Mac OS 9.0.1 instead of OS X
  using the provided CD "iMac Software Restore".
- Insert a 32GB USB stick into the iMac G3.
- Erase the USB stick and format it as "Macintosh ? Extended".
- Use the USB stick to copy software from a modern Mac to the iMac G3.
- Install "Aladdin Stuffit Expander" from a self-extracting archive.
- Install the "Mac OS 9.1 Updater".
- Install the "Mac OS 9.2.1 Updater".
- Install HyperCard 4.2.1.
- Install "The Haunted House 1.0.2" HyperCard stack.

## Installing Software

The websites
{% aTargetBlank "https://macintoshgarden.org", "Macintosh Garden" %} and
{% aTargetBlank "https://www.macintoshrepository.org", "Macintosh Repository" %}
provide lots of software that can be
downloaded and installed on older Macintoshes.

Web browsers that run in Mac OS 9 do not support HTTPS.
That prevents them from accessing modern websites.
One way to obtain software for a computer running Mac OS 9 is to:

- Insert a USB stick into the computer running Mac OS 9 (referred to as "old" below).
- If you see the message "This disk is unreadable by this Computer",
  select the format "Mac OS Extended 1.8 GB" and click the Initialize button.
- Eject the USB stick.
- Insert the USB stick into a modern Macintosh computer (referred to as "new" below).
- Download the software onto the new Mac.
- Copy the software to the USB stick.
- Insert the USB stick into the old Mac.
- Copy the software from the USB stick to the hard drive of the old Mac.

The software comes in many file formats:

- `.bin`: binary executable file
- `.dmg`: standard disk image format for macOS (OS X and later); mount with "Disk Copy" utility
- `.dsk`: disk image
- `.gz`: compressed with gzip
- `.hqx`: ASCII text in BinHex format; use StuffIt Expander or BinHex 4.0 to decode
- `.img`: generic disk image format; mount with "Disk Copy" utility
- `.sea`: self-extracting archive; double-click to decompress
- `.sit`: compressed with Stuffit; requires Stuffit Expander or Stuffit Deluxe to decompress
- `.smi`: Self-Mounting Image; double-click to create a virtual disk on the desktop
- `.toast`: proprietary disk image format associated with Toast Titanium (a CD/DVD burning software for macOS); mount with "Roxio Toast"
- `.zip`: compression format; decompress with MacZip or StuffIt Expander
- `.tar.gz` or `.tgz`: gzipped Tape Archive; a Unix utility for bundling multiple files into a single archive that is then compressed with gzip

The utility "Toast 5 Titanium" can be obtained from {% aTargetBlank
"https://macintoshgarden.org/apps/toast-5-titanium", "Macintosh Garden" %}.
The first "www" link downloads the file `Toast_501_523_Install.sit`.
Copy the file to the old Mac and double-click it to expand.
Install version ? and then each of the updates in order.
Double-click the file ? to run it and enter the license key "SP-HG15N-HP693-S72OR"
which is provided after the list of links on the download page.

To mount a disk image using Toast,
TODO: Finish this.
The disk image often contains an installer for a specific application,
but it make contain raw files that just need to be copied to the hard drive.

Some applications like the game Myst require having a CD installed to use it.
Mounting the disk image satisfies this requirement.

## Mac OS 9 Tips

To simulate a right mouse button click in order to get context-sensitive menus,
hold down the control key while clicking.
For example, doing this on the trash can icon,
opens a menu containing "Empty Trash...".

To delete a file or folder in the Finder,
click it to select it and press cmd-delete.
The trash can icon will change to a bulging trash can
to indicate that something is inside it.

To empty the trash can, do one of the following:

- Select Special ... Empty Trash...
- control-click the trash can and select "Empty Trash..."
- Press cmd-shift-delete.

To eject a disc or USB stick, drag it to the trash can.

The name of the currently running application
is displayed on the right end of the menu bar.
To see a list of the applications that are currently running in a menu,
click that.
To make a different application active, click its name in the menu.

The "Control Strip" provides quick access to many settings.
It is not displayed by default.
To display it, select Apple menu ... Control Panels ... Control Strip
and click the "Show Control Strip" radio button.
To collapse the Control Strip so only its right end is visible,
click on its left or right end.
To expand the Control Strip,
click on its right end which is the only visible part.

To adjust the number of minutes the computer
must be inactive before it goes to sleep,
select Apple menu ... Control Panels ... Energy Saver
and drag the slider to the desired number of minutes.

To wake the computer from sleep, press any key.
Moving the mouse will not wake it.

To boot from a CD, insert the CD, reboot the computer,
and hold down the "C" key until the Apple logo appears.

To adjust the monitor settings:

- Click the Apple logo on the left end of the menu bar.
- Select "Control Panels".
- Select "Monitors"
  - Select "Monitor".
    - Adjust the slider for Contrast.
    - Adjust the slider for Brightness.
  - Select "Geometry".
    - Select "Height/Width".
      - Adjust the width and height.
    - Select "Position".
      - Adjust the horizontal and vertical position.
- Close the control panel window.

To adjust the layout of icons in the Finder:

- Open a Finder window.
- Select View ... as Icons.
- Select View ... View Options...
- Select "None" to enable dragging icons to any pixel location.
- Select "Always snap to grid" to constrain the placement of icons.
- Select "Keep arranged: by Name (or another option)
  to automatically order icons by their names.
- Optionally change "Icon Size" to small or large icons.

To sort the contents of a directory by file/folder names,
select View ... Arrange ...by Name.

To resize a Finder window to the minimum size required
to display all of its contents, click the first button
in the upper-right corner of its title bar.

To capture a screenshot to a file named "Picture {n}"
at the top of the hard drive:

- Press cmd-shift-3 to capture the full screen.
- Press cmd-shift-4 to drag out a selected area of the screen.

To watch a DVD:

- Insert a DVD in the slot.
- Wait for the "Apple DVD Player" app to launch.
- Click the Play button (right-pointing triangle).
- Optionally select Video ... Fill Screen or press cmd-3.
- For me this played for about 30 seconds and then locked up,
  requiring a reboot. Force quit did not work.

To eject a disc when booting, hold down the mouse button.
