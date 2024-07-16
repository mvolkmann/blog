---
eleventyNavigation:
  key: Deploying
  order: 43

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
