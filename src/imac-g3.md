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
- Pro Keyboard
- Pro Mouse
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
- VGA port for display mirroring (couldn't get this to work)
- original price $1499

This is one of the cleanest iMac G3 computers I was able to find,
as evidenced by the photos below.

<img alt="iMac G3 full" style="width: 100%"
  src="/blog/assets/imac-g3-full.jpeg?v={{pkg.version}}">

<img alt="iMac G3 top" style="width: 100%"
  src="/blog/assets/imac-g3-top.jpeg?v={{pkg.version}}">

<img alt="iMac G3 bottom" style="width: 100%"
  src="/blog/assets/imac-g3-bottom.jpeg?v={{pkg.version}}">

<img alt="iMac G3 barcodes" style="width: 50%"
   src="/blog/assets/imac-g3-barcodes.jpeg?v={{pkg.version}}">

<img alt="iMac G3 left" style="width: 100%"
  src="/blog/assets/imac-g3-left.jpeg?v={{pkg.version}}">

<img alt="iMac G3 right" style="width: 100%"
  src="/blog/assets/imac-g3-right.jpeg?v={{pkg.version}}">

<img alt="iMac G3 ports" style="width: 100%"
  src="/blog/assets/imac-g3-ports.jpeg?v={{pkg.version}}">

<img alt="iMac G3 back" style="width: 100%"
  src="/blog/assets/imac-g3-back.jpeg?v={{pkg.version}}">

<img alt="iMac G3 keyboard and mouse" style="width: 100%"
  src="/blog/assets/imac-g3-keyboard-mouse.jpeg?v={{pkg.version}}">

## Setup

Below are the steps I took to setup this computer.

1. Reinstall the operating system to run Mac OS 9.0.1 instead of macOS X
   using the provided CD "iMac Software Restore".
   While macOS X can run on this computer, it is slow compared to Mac OS 9.
1. Insert a 32GB USB stick into the iMac G3.
1. Erase the USB stick and format it as "Macintosh ? Extended".
1. Use the USB stick to copy software from a modern Mac to the iMac G3.
1. Install "Aladdin Stuffit Expander" from a self-extracting archive.
1. Install the "Mac OS 9.1 Updater".
1. Install the "Mac OS 9.2.1 Updater".
1. Install HyperCard 4.2.1.
1. Install "The Haunted House 1.0.2" HyperCard stack.

## PRAM Battery

The PRAM battery in a computer is responsible for maintaining:

- date and time
- startup disk preference
- display settings like resolution and color depth
- sound volume
- other system parameters

Old computers likely have a PRAM battery with very little charge remaining.
These batteries can also begin to leak acid which can damage the computer.
It's highly recommended to replace the PRAM battery with a new one
as soon as possible.

In an iMac G3 computer, the PRAM battery is a 1/2 AA 3.6 volt lithium battery.
I ordered a {% aTargetBlank
"https://www.amazon.com/dp/B008POD4E2?ref=ppx_yo2ov_dt_b_fed_asin_title",
"Xeno Energy XL-050F" %} battery from Amazon.

The steps to install a new PRAM battery are shown below.

1. Verify that the new PRAM battery has close to 3.5 volts.
1. Shut down the computer.
1. Unplug everything including the
   power cord, keyboard, mouse, and Ethernet cable.
1. Spread a blanket or towel on the table/desk where you will work
   to provide a soft surface for the computer.
1. Lay the computer upside down on the soft surface.
1. Unscrew the four screws that hold the bottom of the plastic case in place.
   The photos below show the location of the four screws.

   <img alt="iMac G3 bottom plastic screws set #1" style="width: 49%"
     src="/blog/assets/imac-g3-bottom-plastic-screws-1.jpeg?v={{pkg.version}}">
   <img alt="iMac G3 bottom plastic screws set #2" style="width: 49%"
     src="/blog/assets/imac-g3-bottom-plastic-screws-2.jpeg?v={{pkg.version}}">

1. Gently remove the bottom plastic case.
   It will be somewhat difficult to free the edge that is near the CD slot.
   You will likely hear some cracking sounds, but supposedly that is okay.
   I heard cracking, but nothing seems to have broken.

1. Unscrew the six screws that hold the
   electromagnetic interference (EMI) shield
   (a molded, thin, silver metal with lots of pin holes) in place.
   Be careful not to allow the removed screws to fall inside the computer!
   The photo below shows the location of the six screws.

   <img alt="iMac G3 bottom plastic removed" style="width: 70%"
     src="/blog/assets/imac-g3-bottom-plastic-removed.jpeg?v={{pkg.version}}">

1. Remove the EMI shield.

   <img alt="iMac G3 EMI shield removed" style="width: 70%"
     src="/blog/assets/imac-g3-emi-shield-removed.jpeg?v={{pkg.version}}">

1. Remove the old PRAM battery.

   The image above shows the location of the PRAM battery.

1. Insert the new PRAM battery.

   <img alt="iMac G3 new PRAM battery" style="width: 70%"
     src="/blog/assets/imac-g3-new-pram-battery.jpeg?v={{pkg.version}}">

1. Replace the EMI shield.
   Be careful not to allow the screws to fall inside the computer!
1. Optionally clean the bottom of the plastic case. See "Cleaning" below.
1. Replace the bottom of the plastic case.
1. Return the computer to its upright position.
1. Plug in the power cord, Ethernet cable, keyboard, and mouse.
1. Boot the computer.
1. Open the "Date & Time" control panel.
1. Set the date and time.

## Cleaning

### Plastic Case

To clean the top and bottom parts of the plastic case of an iMac G3 computer:

1. Remove the bottom of the plastic case as described above.
1. Remove the top of the plastic case.
1. Wash the plastic case parts using Dawn liquid dish washing soap or similar
   and a microfiber cloth.
   Do not use paper towels because
   the wood fibers in them can scratch the plastic.
   Do not use Windex, isopropyl alcohol, or a Magic Eraser sponge
   because they can make the plastic cloudy.
1. Dry the plastic case parts with another microfiber cloth.
1. Use a blow dryer on low heat to thoroughly dry the plastic case parts.
1. Optionally use Meguiar's PlasticX to
   remove scuffs and scratches in the plastic case parts.
1. Reattach the top of the plastic case.
1. Reattach the bottom of the plastic case.

### Pro Keyboard

To disassemble, clean, and reassemble the Pro Keyboard, see this {% aTargetBlank
"https://www.youtube.com/watch?v=dnO3kCGxdgY", "YouTube video" %}.

To clean the key caps and the area beneath all the keys,
gently pop them off with something like a butter knife.

There are three phillips screws on the top side of the keyboard
that need to be removed. They are under the caps lock key,
the semicolon key, and the 6 key on the numeric keypad.

There are four screws on the bottom of the keyboard that need to be removed
with a 1.3mm allen wrench. You likely don't have one that small!

From 7:36 to 8:28 in the video he struggles to remove a hidden screw.
It is not necessary to use heat and pull back the sticker.
It is located 15mm to the left of the Apple logo on the sticker
and can be seen from the top edge of the keyboard.
Poke the point of a razor knife directly over the top of the screw
and gently turn it to create a small hole in the sticker
through which it can be unscrewed.

At this point the back of the keyboard case can be removed, but not easily.
From the top side of the keyboard,
pry the top edge that runs between the two USB ports free.
Then work your way down the sides with a thin piece of metal
until you reach the front edge of the keyboard.
Continue using the thin piece of metal to pop the top plastic piece
free from the tabs that snap it into place,
being careful not to crack the case.

<img alt="iMac G3 keyboard screw hole" style="width: 70%"
  src="/blog/assets/imac-g3-keyboard-screw-hole.jpg?v={{pkg.version}}">

## Installing Software

The websites
{% aTargetBlank "https://macintoshgarden.org", "Macintosh Garden" %} and
{% aTargetBlank "https://www.macintoshrepository.org", "Macintosh Repository" %}
provide lots of software that can be
downloaded and installed on older Macintoshes.

Web browsers that run in Mac OS 9 do not support HTTPS.
That prevents them from accessing modern websites.
One way to obtain software for a computer running Mac OS 9 is to:

- Insert a USB stick into the computer running Mac OS 9
  (referred to as "old" below).
- If you see the message "This disk is unreadable by this Computer",
  select the format "Mac OS Extended 1.8 GB" and click the Initialize button.
- Eject the USB stick.
- Insert the USB stick into a modern Macintosh computer
  (referred to as "new" below).
- Download the software onto the new Mac.
- Copy the software to the USB stick.
- Insert the USB stick into the old Mac.
- Copy the software from the USB stick to the hard drive of the old Mac.

The software comes in many file formats:

- `.bin`: binary executable file
- `.dmg`: standard disk image format for macOS (OS X and later);
  mount with "Disk Copy" utility
- `.dsk`: disk image
- `.gz`: compressed with gzip
- `.hqx`: ASCII text in BinHex format;
  use StuffIt Expander or BinHex 4.0 to decode
- `.img`: generic disk image format; mount with "Disk Copy" utility
- `.sea`: self-extracting archive; double-click to decompress
- `.sit`: compressed with Stuffit;
  requires Stuffit Expander or Stuffit Deluxe to decompress
- `.smi`: Self-Mounting Image;
  double-click to create a virtual disk on the desktop
- `.toast`: proprietary disk image format associated with Toast Titanium
  (a CD/DVD burning software for macOS); mount with "Roxio Toast"
- `.zip`: compression format; decompress with MacZip or StuffIt Expander
- `.tar.gz` or `.tgz`: gzipped Tape Archive; a Unix utility for bundling
  multiple files into a single archive that is then compressed with gzip

The utility "Toast 5 Titanium" can be obtained from {% aTargetBlank
"https://macintoshgarden.org/apps/toast-5-titanium", "Macintosh Garden" %}.
The first "www" link downloads the file `Toast_501_523_Install.sit`.
Copy the file to the old Mac and double-click it to expand.
Install version ? and then each of the updates in order.
Double-click the file ? to run it and
enter the license key "SP-HG15N-HP693-S72OR"
which is provided after the list of links on the download page.

To mount a disk image using Toast:

1. Launch "Toast Titanium".
1. Select Utilities ... Mount Disc Image...
1. Select a disk image file.
1. Click the "Open" button.
1. This may run the app in the disk image or
   it may open a Finder window that displays the contents.
1. If it opens a Finder window,
   it may contain an installer app or the needed files.
1. If it contains an installer app, double-click it to run it.
1. If it contains the raw files, copy them to the hard drive.

Some applications like the game Myst require having a CD installed to use it.
Mounting the disk image satisfies this requirement.

## Printing

One solution to printing is to use Adobe Acrobat PDFWriter
and configure that as a printer.
It will create a PDF file rather than actually printing.
The PDF can be copied to another computer, perhaps via a USB stick,
that has access to a real printer.

Adobe Acrobat PDFWriter 4.0 can be downloaded from
{% aTargetBlank "https://macintoshgarden.org/apps/adobe-acrobat-pdfwriter-40",
"Macintosh Garden" %}.

The steps to configure this are:

1. Install Adobe Acrobat PDFWriter.
1. Move the file "Acrobat PDFWriter 4.0" to
   Macintosh HD ... System Folder ... Extensions.
1. Select "Chooser" from the Apple menu.
1. Select "Acrobat PDFWriter 4.0".
1. Close the Chooser window.

When an application prints something, a PDF named "untitled [n]"
will be created on the desktop.
Double click one to open it in Acrobat Reader.

## Internet

If WiFi is available and the iMac G3 has an AirPort card,
it can be used to connect to the internet wirelessly.
However, this requires the wireless router
to support WEP protocol which is insecure.
It is likely that the router is not configure to support WEP.

To connect to the internet using an Ethernet cable:

1. Connect an active Ethernet cable to the Ethernet port
   on the right side of the iMac G3.
1. Open the "TCP/IP" control panel.
1. In the "Connect via" dropdown, select "Ethernet built-in".
1. In the "Configure" dropdown, select "Using DHCP Server".
1. Close the control panel.
1. Open the "AppleTalk" control panel.
1. In the "Connect via" dropdown, select "Ethernet built-in".
1. Close the control panel.
1. Open a web browser such as "Internet Explorer" found in
   Applications (Mac OS 9) ... Internet Explorer 5

Internet Explorer 5 does not support HTTPS.
That means it cannot browser the majority of modern websites that require HTTPS.
Notable web sites that do not require HTTPS include:

- www.google.com
- freshsilverbrightplan.neverssl.com/online
- macintoshgarden.org
- macintoshrepository.org

Attempting to access a site that requires HTTPS
will display an error dialog with the message
"Security failure. A secure connection could not be established."

## File Sharing

### Using AFP (does not work)

Once connection to the internet is working,
the following steps enable sharing files between
a Macintosh running Mac OS 9 and a Macintosh running macOS X
that are on the same network:

On the Macintosh running Mac OS 9:

1. Open the "File Sharing" control panel.
1. Click the "Start/Stop" tab.
1. Under "File Sharing Off",
   check the "Enable File Sharing clients to connect over TCP/IP" checkbox
   and click the "Start" button.
1. Make a note of the URL that is displayed which begins with "afp://".
1. Click the "Users & Groups" tab.
1. Click the "New User" button.
1. If a user is not already present, click the "New User" button and create one.
1. Close the control panel.
1. Create a directory to hold shared files on the desktop.
1. Select the directory.
1. Press cmd-i to view its Info dialog.
1. In the "Show" dropdown, select "Sharing".
1. Check the "Share this item and its contents" checkbox.
1. In the "Owner" dropdown, select the user from "Users & Groups" earlier.
1. In the "Privilege" dropdown in the "Owner" row, select "Read & Write".
1. Close the control panel.

On the Macintosh running macOS X:

1. Open the Finder.
1. Select Go ... Connect to Server...
1. Enter "afp://" followed by the IP address of the Macintosh running Mac OS 9.
1. Click the "Connect" button.

This gives the error "There was a problem connecting to the server".
The reason is that macOS X does not support AFP.
It does support SMB, but Mac OS 9 does not support that.

### Using FTP

On the computer running Mac OS 9:

1. Open the "File Sharing" control panel.
1. Click the "Start/Stop" tab.
1. Under "File Sharing Off",
   check the "Enable File Sharing clients to connect over TCP/IP" checkbox
   and click the "Start" button.
1. Make a note of the IP address that is displayed after "afp://".
1. Download and install {% aTargetBlank
   "https://macintoshgarden.org/apps/netpresenz", "Netpresenz 4.1" %}
   on the computer running Mac OS 9.
1. Double-click "NetPresenz Setup".
1. Dismiss the "About" dialog.
1. Click "FTP Setup".
1. Under "Owner", change "File Access" to Full and check "Enabled".
1. Under "Users" and "Guest", change "File Access" to None and uncheck "Enabled".
1. Uncheck "Allow Simple Internet Version Control (SIVC) periodic queries"
   because there will never be an updates.
1. Click the "Save" button.
1. Press cmd-q to quit the setup app.
1. Double-click "NetPresenz" to start the FTP server.

Once this has been done the first time,
only the last step is required to start the FTP server.

On computers running macOS X:

1. Open the Finder.
1. Select Go ... Connect to Server...
1. Enter or select "ftp://{ip-address}"
   where ip-address is that of the Mac OS 9 computer.
1. Click the "+" button in the lower-left to remember this server.
1. Change "Name" to a username on the Mac OS 9 computer.
1. Enter the password for for that username.
1. Check the "Remember this password in my keychain" checkbox.
1. Click the "Connect" button to open a new Finder window.
1. Read the shared files.

Writing to the Mac OS 9 computer requires using
an FTP client on the macOS X computer like Transmit or CyberDuck.

To use CyberDuck:

- On the Mac OS 9 computer:

  1. Open the "Multiple Users" control panel.
  1. After "Multiple User Accounts", select the "On" radio button.
  1. Optionally change the user name and password.

- On the macOS computer:
  1. Launch the CyberDuck app.
  1. Click "Open Connection".
  1. After "Server", enter the IP address of the Mac OS 9 computer
     which can be found in the "File Sharing" control panel.
  1. Enter the username and password for the Mac OS 9 computer.
  1. Click the "Connect" button.
  1. Drag files from the CyberDuck window to the Finder.
  1. Drag files from the Finder to the CyberDuck window.

## Screenshots

To capture a screenshot to a PICT file named "Picture {n}"
at the top of the hard drive:

- Press cmd-shift-3 to capture the full screen.
- Press cmd-shift-4 to drag out a selected area of the screen.
- Press caps lock, then cmd-shift-4 to capture a selected window.
- Hold down the control when with any of the above
  to copy the screenshot to the clipboard.

When viewed in macOS using the Preview app,
PICT files will likely appear as an all white rectangle.
This happens because Mac OS 9 stores the image data
in the Resource Fork instead of the Data Fork.

One way to convert a PICT file to a PNG file in macOS is to use
the free app XnConvert which can be downloaded from the App Store.

1. Add the file extension ".pict" to PICT files.
1. Launch the XnConvert app.
1. Click the "Add files..." button.
1. Select any number of ".pict" files.
1. Click the "Convert" button.
1. Select the destination directory (defaults to directory of PICT file).
1. Click the "Open" button to generate
   ".png" files in the destination directory.
   The file names will be the same name as the ".pict" files,
   but with "\_result" appended.
1. Click the "Close" button.

As an alternative to steps 2, 3, and 4, right-click a ".pict" file
and select Open With ... XnConvert.

For a more powerful screenshot solution that supports many graphics formats
including GIF, JPEG, and PNG, see {% aTargetBlank
"https://macintoshgarden.org/apps/snapz-pro-2", "Snapz Pro 2" %}.
License keys are provided in a text file.
This adds the "Snapz Pro" control panel.
Click the "Settings..." button to configure.
The default key to take a screenshot is cmd-shift-3.
In the dialog that appears, specify:

- Capture: Select Screen, Window, Menu, or Selection.
- Send to: Select a location such as Desktop.
  It defaults to Macintosh HD:SystemFolder:Screen Snapz.
- "Cursor visible" checkbox:
  Uncheck this to avoid including the mouse cursor in the screenshot.
- File Type: Select one such as .gif, .jpg, or .png.
- "Choose file name" checkbox: When prompted for a file name,
  include a file extension because it will not be
  added automatically based on the selected file type.

When capturing a selection, adjust the rectangle to capture
and press the return key.

## Sounds

To record a new alert sound:

1. Open the "Sound" control panel.
1. Click the "Input" tab.
1. Select "Built-in Mic".
1. Check the "Check signal level" checkbox.
1. Say something to verify that the microphone is working.
1. If the "Level" meter doesn't move, adjust the "Gain" slider.
1. Click the "Alert" tab.
1. Click the "New Sound" button.
1. Click the "Record" button.
1. Make the desired sound.
1. Click the "Stop" button.
1. Click the "Play" button to verify that the sound was captured.
1. Click the "Save" button.

All the alert sounds are stored in "snd" resources
inside the `System` file which is in the `System Folder` directory.
These can be copied to HyperCard stacks using ResEdit.

## Applications

### AppleWorks

AppleWorks is an office suite from Apple that supersedes ClarisWorks.
The final version was 6.2.9.
It can be downloaded from {% aTargetBlank
"https://www.macintoshrepository.org/21008-appleworks-6-2-9-691-4718-a-cd-",
"Macintosh Repository" %}.

To install AppleWorks:

- Launch the "Toast Titanium" app.
- Selecting Utilities ... Mount Disc Image...
- Select the downloaded `.toast` file and click the "Open" button.
- Double-click the disc image "AppleWorks Mac 6.2.9".
- Double-click "US and Canadian Installer" or "International English Installer"
  and perform the installation.
- Restart the computer.

To create a PNG file for an icon:

- Launch ResEdit.
- Open the file that contains the icon resource.
  When copying resources from the `System` file,
  avoid damaging that file by making a copy of it in the Finder
  and opening the copy in ResEdit.
- Double-click the icon resource.
- Press cmd-a to select all the pixels.
- Press cmd-c to copy the icon.
- Launch AppleWorks.
- Click the "Painting" button.
- Press cmd-v to paste the icon.
- Drag the icon to the upper-left corner.
- Press cmd-s to save the icon.
- Optionally change the file destination which defaults to "Desktop".
- Enter a name with the file extension `.png`.
- Change the "File Format" to "PNG [QT]".
- Click the "Save" button.
- Press cmd-q to quit AppleWorks.
- Click the "Don't Save" button.
- Return to ResEdit.
- Press cmd-q to quit ResEdit.

### BBEdit

The highest version of BBEdit that runs in Mac OS 9 is 6.1.2.

This can create pure text files that can be copied to and read on
a newer computer like one running macOS.

TODO: What is the difference between BBEdit and BBEdit Lite?

### SimpleEdit

This is a simple text editor that is installed by default.
The text files it produces cannot be read by macOS apps like TextEdit.
To create plain text files, consider using BBEdit instead.

SimpleEdit documents can contain graphics.
TODO: What other kinds of media can be pasted in these documents?

## Finder Tips

To adjust the layout of icons in the Finder:

1. Open a Finder window.
1. Select View ... as Icons.
1. Select View ... View Options...
1. Select "None" to enable dragging icons to any pixel location.
1. Select "Always snap to grid" to constrain the placement of icons.
1. Select "Keep arranged: by Name (or another option)
   to automatically order icons by their names.
1. Optionally change "Icon Size" to small or large icons.

To sort the contents of a directory by file/folder names,
select View ... Arrange ...by Name.

To resize a Finder window to the minimum size required
to display all of its contents, click the first button
in the upper-right corner of its title bar.

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

To close all Finder windows, hold down the option key
and click the close box of any one of them.

## Mac OS 9 Tips

To add a desktop picture:

1. Select Apple menu ... Control Panels ... Appearance ... Desktop.
1. If a picture was previously selected, click the "Remove Picture" button.
1. Click the "Place Picture..." button.
1. Click the "Place Picture..." button.
1. Select a file with the format JPEG, GIF, PICT, or Photoshop.
1. Click the "Open" button.

To simulate a right mouse button click in order to get context-sensitive menus,
hold down the control key while clicking.
For example, doing this on the trash can icon,
opens a menu containing "Empty Trash...".

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

1. Click the Apple logo on the left end of the menu bar.
1. Select "Control Panels".
1. Select "Monitors"
   1. Select "Monitor".
      1. Adjust the slider for Contrast.
      1. Adjust the slider for Brightness.
   1. Select "Geometry".
      1. Select "Height/Width".
         1. Adjust the width and height.
      1. Select "Position".
         1. Adjust the horizontal and vertical position.
1. Close the control panel window.

Mac OS 9 did not ship with a music app like iTunes,
which was introduced in macOS X.

To play an audio CD:

1. Insert a CD in the slot and wait about 10 seconds for it to load.
1. An icon representing the CD titled "Audio CD {n}" will appear on the desktop.
1. The first track will start playing automatically.
1. To control the playback, open the "AppleCD Audio Player" app
   found in Macintosh HD:AppleCD Audio Player f.

The "AppleCD Audio Player" app:

- contains buttons to play, pause,
  skip to the next and previous tracks,
  shuffle the tracks, repeat after the last track plays,
  and eject the disc.
- contains a dropdown to skip to a specific track.
- contains a slider to control the volume relative to the system volume.
- displays the total duration and the duration of each track.
- contains inputs to the enter the CD title and the name of each track.
  These are remembered after the disc is ejected
  and displayed when the disc is reinserted.
- cannot import the tracks and save them on the hard drive

To watch a DVD:

1. Insert a DVD in the slot.
1. Wait for the "Apple DVD Player" app to launch.
1. Click the Play button (right-pointing triangle).
1. Optionally select Video ... Fill Screen or press cmd-3.
1. For me this played for about 30 seconds and then locked up,
   requiring a reboot. Force quit did not work.

To eject a disc when booting, hold down the mouse button.

All audio, including that from CDs and DVDs,
can be played on external speakers by plugging them into
one of the headphone jacks on the front of the iMac G3.
There are two headphone jacks to allow
two people to listen on headphones at the same time.
Later versions of the iMac only have one headphone jack.
