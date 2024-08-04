---
eleventyNavigation:
  key: Startup Scripts
  order: 43

  parent: Smalltalk
layout: topic-layout.njk
---

Startup scripts automate starting an image with:

- preferred settings
- tool windows open at specfied locations with specified sizes
- loaded packages

## Example

The following is an example script in a file named `cuis`.

```bash
#!/usr/bin/env zsh
CUIS_DIR=$SMALLTALK_DIR/Cuis-Smalltalk-Dev
VM=$CUIS_DIR/CuisVM.app/Contents/MacOS/Squeak
IMAGE=$CUIS_DIR/CuisImage/base-copy.image
$VM $IMAGE -s env-setup.st
```

The following is an example of what can appear in the file `env-setup.st`
which contains Smalltalk code:

```smalltalk
| world |
Utilities setAuthorName: 'R. Mark Volkmann' initials: 'rmv'.
world := UISupervisor ui.
[
    Display fullScreenMode: true.
    (Delay forSeconds: 1) wait.
    UISupervisor whenUIinSafeState: [
        | area browser children extent taskbarHeight workspace |

        "Delete all submorphs of the WorldMorph except the taskbar."
        children := world submorphs reject: [:aMorph | aMorph is: #TaskbarMorph].
        children do: [:child | child delete].

        "Set some preferences."
        "Preferences at: #defaultFontSize put: 14."
        Preferences at: #defaultFontSize put: 24.

        "Get the usable area not including the taskbar."
        taskbarHeight := UISupervisor ui taskbar morphHeight.
        area := RealEstateAgent maximumUsableArea
            extendBy: 0 @ taskbarHeight negated.
        extent := area extent // 2. "quarter of world"

        "Open a System Browser in the upper-left."
        browser := Browser open.
        browser
            "morphPosition: 0 @ 0;
            morphExtent: extent."
            morphPosition: extent x / 2 @ 0;
            morphExtent: extent x @ area extent y.

        "Open an Installed Packages window lower-left."
        cplw := CodePackageListWindow open: CodePackageList new.
        cplw morphPosition: 0 @ extent y; morphExtent: extent.

        "Open a Workspace in the upper-right."
        workspace := Workspace open.
        workspace
            morphPosition: extent x @ 0;
            morphExtent: extent.
        workspace model actualContents: '1 + 2.
''testing'' print.'.

        "Open a Transcript in the lower-right."
        TranscriptWindow openTranscript
            morphPosition: extent;
            morphExtent: extent.

        Feature require: 'JSON'.
        Feature require: 'UI-MetaProperties'.
        Feature require: 'WebClient'.
        "Load my package which defines the Object method logAs: and much more."
        Feature require: 'Volkmann'.

        "browser activateWindow." "brings to front"

        "Add buttons in upper-left to toggle full screen mode."
        "WHY DOES THIS FAIL IN SCRIPT, BUT NOT FROM WORKSPACE?"
        "(Delay forSeconds: 1) wait.
        VFullScreenButtons new."
    ]
] fork
```

To use this script:

- Set the environment variable `SMALLTALK_DIR` to
  the directory path where Cuis Smalltalk was installed.
- Make a copy of the base image file that has a name like
  `Cuis7.1-6541.image` and name the copy `base-copy.image`.
- Make a copy of the base changes file that has a name like
  `Cuis7.1-6541.changes` and name the copy `base-copy.changes`.
- Create the file `env-setup.st` (described later)
  in the same directory as this script.
- Open a terminal window.
- `cd` to the directory containing this script.
- Enter `./cuis` to launch the VM in your preferred starting state.

## When To Use

Startup scripts like the one described above are useful
when a new image version is released.
They enable restoring your preferred environment setup using a new image.
Once this is done, it's a good idea to open the World menu
and select "Save Image as..." to save the image under a new name.

Starting future sessions using the saved image will be much faster
than starting the base image using your startup script.
The reason is that the saved image will already contain
all the packages installed by the startup script.
Loading packages during startup is relative time consuming.
