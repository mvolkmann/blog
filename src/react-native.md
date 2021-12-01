---
eleventyNavigation:
  key: React Native
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://reactnative.dev", "React Native" %}
enables using JavaScript and React to implement Android and iOS applications.

There are two command-line tools that can be used
to create and run React Native applications,
Expo and the React Native CLI.

## Expo Setup

- Install the Expo CLI by entering `npm install -g expo-cli`.

- Create a new Expo project by entering `expo init app-name.

- `cd app-name`

- Change the `Text` rendered by `App.js` to "Hello, World!"

- Start the Expo server by entering `npm start`.

  This displays a QR code in the default web browser.

- Install the "Expo Go" app on a mobile device.

- Launch the camera app.

- Scan the QR code displayed in the web browser.

## React Native CLI Setup

- Install Node.js if not already installed.

  One way is to install Homebrew and then enter `brew install node`.

- Install watchman by entering `brew install watchman`.

- Install Xcode from the Mac App Store.

- Install the "Xcode Command Line Tools".

- Install CocoaPods by entering `sudo gem install cocoapods`.

- Create a new project by entering `npx react-native init app-name`.

- `cd app-name`

- Start a Metro server by entering `npx react-native start`.

- Run the app in the Simulator by entering
  `npx react-native run-ios` in another terminal.

- To run the app on a device:

  - Attached the device to the computer using a USB cable.
  - Open the app in Xcode.
  - Register for an Apple developer account.
