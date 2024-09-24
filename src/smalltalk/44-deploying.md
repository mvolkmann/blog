---
eleventyNavigation:
  key: Deploying
  order: 44

  parent: Smalltalk
layout: topic-layout.njk
---

Smalltalk can be used to build command-line, desktop, and web applications.
After writing and testing the code in a standard Smalltalk image,
it is recommended to create a stripped down version of the image
that only contains what is necessary for running the application.

All developer tools can be removed.
For command-line and web applications, everything related to
the Morphic GUI framework can be removed.

Stripping an image is a manual process that is
described in the FAST 2024 conference video
<a href="https://www.youtube.com/watch?v=MfAclig5XyI"
target="_blank">Bootstrap: Creating Minimal Images from Scratch</a>.

Also see
<a href="https://www.youtube.com/watch?v=b3oGOMCjKU8&list=PLu8vLCSA-4hklsvT9W6ruintbdx_K0DYW&index=2&t=53s"
target="_blank">Make a standalone click-&-run Smalltalk application for macOS</a>.

### Running Headless

To run Smalltalk programs that are command-line utilities, apps, or servers,
use the Smalltalk VM that is bundled inside the macOS app
`CuisVM.app` that is included in the Cuis-Smalltlk-Dev GitHub repository.
This is actually a Squeak VM.

Squeak VMs can also be obtained from
<a href="https://github.com/OpenSmalltalk/opensmalltalk-vm"
target="_blank">OpenSmalltalk</a>.
Under "Releases" on the right side, click "Latest".
Click the proper file for your operating system and processor.
For example, `squeak.cog.spur_macos64ARMv8.dmg`.
Double-click the downloaded file to install it.

To manually download and build a Squeak VM for macOS:

1. Open a Terminal and cd to the directory where the VM will be created.
1. Enter `git clone https://github.com/OpenSmalltalk/opensmalltalk-vm.git`
1. Enter `cd opensmalltalk-vm`
1. Enter `./scripts/updateSCCSVersions`
1. Enter `cd building`
1. `cd` to the appropriate subdirectory for the target OS and processor.
   For example, `macos64ARMv8`.
1. See the instructions in the file "HowToBuild".
1. Enter `cd squeak.coq.spur`
1. Enter `./mvm -A`. This will run for around 10 minutes and
   generate an extreme amount of output.
1. Enter `chmod a+x Squeak.app`
1. Create a symbolic link to the VM that is inside this app
   by entering `ln -s "./Squeak.app/Contents/MacOS/Squeak" squeak-vm`.

To simplify running files containing Smalltalk code from the command line,
create a script named "cuis" in a directory that is in your PATH
containing the following:

```bash
#!/usr/bin/env bash
# Runs a file containing Smalltalk code in headless mode using Cuis Smalltalk.

if [ $# -ne 1 ]; then
  echo usage: cuis {file-name}
  exit 1
fi

CUIS_DIR=$SMALLTALK_DIR/Cuis-Smalltalk-Dev
VM=$CUIS_DIR/CuisVM.app/Contents/MacOS/Squeak
IMAGE=$CUIS_DIR/CuisImage/Cuis7.1-6452.image
$VM -headless $IMAGE -s $1
```

Create `.st` files containing Smalltalk code.
For example, the file `demo.st` could contain the following:

```smalltalk
| stdout |
stdout := StdIOWriteStream.
stdout nextPutAll: 'Hello, World!'; newLine.
Smalltalk quit
```

To run this, enter `cuis demo.st`.

TODO: This was working, but just hangs now. Why?

To get help on options, cd to your `Cuis-Smalltalk-Dev` directory
and enter the following:

```bash
./CuisVM.app/Contents/MacOS/Squeak -help
```

For more detail see the `SystemDictionary` class
`displayCommandLineUsageOn:` class method.

## Building a Mac App

See https://www.youtube.com/watch?v=T95k9m5zcX4&list=PLu8vLCSA-4hklsvT9W6ruintbdx_K0DYW&index=4

See https://www.youtube.com/watch?v=b3oGOMCjKU8&list=PLu8vLCSA-4hklsvT9W6ruintbdx_K0DYW&index=3.

The steps to create a double-clickable macOS app are:

- In the `/Applications` directory, create the directory structure
  `{app-name}.app/Contents/MacOS`.
- To see what is inside this directory in the Finder,
  right-click the {app-name}.app file and select "Show Package Contents".
- Copy a Squeak VM into this directory.
  One can be found in Applications/Squeak.app/Contents/MacOS/Squeak.
- Copy a Squeak image that implements the app into this directory
  with the name `{image-name}.image`.
- Create the following shell script in this directory
  with the file name `{app-name}`:

  ```bash
  #!/bin/sh
  DIR=`dirname $0`
  exec $DIR/Squeak.app/Contents/MacOS/Squeak $DIR/{image-name}.image
  exit 0 # TODO: Is this needed?
  ```

- Make the shell script executable by running `chmod a+x {app-name}`.
- Create the file `PkgInfo` into this directory
  containing "APPL????" with no newline character.
- Add an app icon.
  Copy an image onto the clipboard.
  One source for app icons is https://www.macosicongallery.com.
  In the Finder, right-click the app in the Applications directory
  and select "Get Info".
  Click the app icon in the upper-left of the dialog that appears
  and press cmd-v to paste the new icon.

## Preparing User Images

To create images that define applications to be used by non-developers,
it is a good idea to disable and remove developer features.
This the confusion that can arise when users
accidentally open developer features
and results in smaller images.

To hide the taskbar at the bottom of the World:

```smalltalk
UISupervisor runningWorld hideTaskbar.
```

To disable the World menu so clicking on the World background
no longer opens it:

TODO: How is this done?

Perhaps the way to prevent MessageNotUnderstood and other errors
from displaying a debugger window is to execute the following:

```smalltalk
SystemDictionary removeKey: #Debugger
```

The `SystemDictionary` class method `isDevelopmentEnvironmentPresent`
checks for that key in the `SystemDictionary`.

## Steps to Deploy TodoApp

- Open a base image (currently named `Cuis7.1-6713.image`).
- Open a Workspace.
- Enter `Feature require: 'UI-MetaProperties'.` and "Do it".
- Enter `Feature require: 'TodoApp'.` and "Do it".
- Enter `TodoApp new` and "Do it".
- Position and size the "Todo App" window as desired.
- Close the Workspace and Transcript windows.
- Remove the taskbar at the bottom.

  - cmd-click the taskbar.
  - Click the menu halo button and select debug...inspect morph.
  - Enter `self delete` in the bottom pane and "Do it".

- Save the image with a new name.

  - Open the World menu and select "Save Image as...".
  - Enter the image name "TodoApp.image".

- Create the file `todoapp.st` that contains the following:

  ```smalltalk
  | world |

  world := UISupervisor ui.
  [
      (Delay forSeconds: 1) wait.
      UISupervisor whenUIinSafeState: [
          "Disable the World menu."
          Preferences at: #worldMenu put: nil.
          "Disable the cmd-click to get morph halos."
      ]
  ] fork

  ```

- Create the following shell script in the file `todoapp`.
  This assumes that the environment variable `SMALLTALK_DIR` is set to
  the path where the `Cuis-Smalltalk-Dev` local repository resides.

  ```bash
  #!/usr/bin/env zsh
  CUIS_DIR=$SMALLTALK_DIR/Cuis-Smalltalk-Dev
  VM=$CUIS_DIR/CuisVM.app/Contents/MacOS/Squeak
  IMAGE=$CUIS_DIR/CuisImage/TodoApp.image
  $VM $IMAGE -s todoapp.st
  ```

## Hilaire Fernandes Approach

This does not remove the World menu.

- cd to `Cuis-Smalltalk-Dev` directory

- git clone https://github.com/hilaire/CuisApp.git

- Modify the value of the `cuisVersion` variable near the beginning of the file
  `CuisApp/build/makeBundle.sh` to something like "7.1-6713".

- Build an image for the app by entering
  `./CuisApp/build/makeBundle.sh --build`
  TODO: It's unclear whether this can run in macOS. I get the following:

  ```text
  CuisVM.app/Contents/Linux-x86_64/squeak: line 29: /usr/bin/ldd: No such file or directory
  CuisVM.app/Contents/Linux-x86_64/squeak: line 29: /bin/fgrep: No such file or directory
  Error. Could not determine platform's libc path for VM.
  Try forcing $PLATFORMLIBDIR in CuisVM.app/Contents/Linux-x86_64/squeak, based on LIBC_SO.
  ```

  TODO: Where is the new image saved?

- Create a bundle for macOS by entering
  `./CuisApp/build/makeBundle.sh --package mac`

- Create a bundle for Windows by entering
  `./CuisApp/build/makeBundle.sh --package windows`

- Create a bundle for GNU Linux by entering
  `./CuisApp/build/makeBundle.sh --package gnulinux`

To run a bundle ...
TODO: Where is it saved?
