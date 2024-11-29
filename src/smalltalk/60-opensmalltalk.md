---
eleventyNavigation:
  key: OpenSmalltalk
  order: 60
  parent: Smalltalk
layout: topic-layout.njk
---

<a href="https://opensmalltalk.org" target="_blank">OpenSmalltalk</a>
is a "cross-platform virtual machine for Squeak, Pharo, Cuis, and Newspeak".
It has many contributors.
The person with the most commits is
<a href="http://www.mirandabanda.org/cogblog/microbio/" target="_blank">Eliot Miranda</a>.

Many variants of the VM can be built with options such as:

- with or without just-in-time (JIT) compilation

- green threads ("light-weight processes above a single-threaded VM) or
  multi-threaded VM ("shares the VM betwwen any number of native threads")

- with or without Spur

  Spur provides "a partial read barrier for efficient support of
  live object-oriented programming."
  It is currently used by Squeak, Pharo, and Cuis.

- with or without Sista

  Sista "adds support for adaptive optimization that does speculative inlining".
  As of October 2024 it was still in development.

## Obtaining a VM

To obtain an OpenSmalltalk VM:

- Clone the repository by entering
  `git clone https://github.com/OpenSmalltalk/opensmalltalk-vm.git`

  The only branch present on 11/28/2024 was "Cog".

- Perform source file substitutions by entering `./scripts/updateSCCSVersions`

- Choose between the Stack, Cog, and Sista VMs.

  Currently it seems that Cog is preferred.

- Choose between the v3 and Spur VMs.

  Currently it seems that Spur is preferred.

- For ARM-based Macs

  - cd to the `building/macos64ARMv8/squeak.cog.spur` directory.
  - Copy the file `Squeak.app` to the `Applications` directory.

## Building a VM

To install everything needed to build an OpenSmalltalk VM for macOS:

- If Xcode.app is not installed, install it.
- If the Xcode Command Line Tools are not installed,
  install them by entering `xcode-select --install`.
- If Homebrew is not installed, install it.
- If the `cmake` command is not installed,i
  install it by entering `brew install cmake`.

To build the VM:

- cd to the `building/macos64ARMv8/squeak.cog.spur` directory.
- Enter 'xcodebuild -runFirstLaunch` (not sure about this step)
- Enter './makespur` (not sure about this step)

  This will run for a long time (~10 minutes) and produces a large amount of output.
  There will be many warnings about issues such as unused variables.
  I got 39 warnings and 1 error.
  The error was "call to undeclared function 'literalBeforeInlineCacheTagAt'".
  Where is the generated VM placed?

For more detail, see
<a href="https://github.com/OpenSmalltalk/opensmalltalk-vm/blob/Cog/building/macos64ARMv8/HowToBuild"
target="_blank">HowToBuild</a>.

## Downloading an Image

### Squeak

To download the base image for Squeak, browse
<a href="https://squeak.org/downloads/" target="_blank">Squeak Downloads</a>.

For the latest non-stable image:

- Click the link under "Current Trunk Image".
- Scroll to the bottom of the list to see the most recent image.
- Click the link for the desired image.
- Select the appropriate file for your platform
  which will be a `.dmg`, `.tar.gz`, or `.zip` file.
- Double-click the downloaded file to install/uncompress it.

For the latest stable image:

- Click a "Release" link under "Images and Changes".
- Click the "Sources" link under "History"
  to download the file `SqueakV60.sources` which must be placed
  in the same directory as each Squeak `.image` file.

## Running an Image

To run an image, double-click `Squeak.app`.
That will open a dialog that prompts for a `.image` file.
Select one and click the "Open" button.

Alternatively, double-click a Squeak `.image` file.
