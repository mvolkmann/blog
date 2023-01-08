---
eleventyNavigation:
  key: PhotosPicker
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS 16 introduced the {% aTargetBlank
"https://developer.apple.com/documentation/photokit/photospicker",
"PhotosPicker" %} view.

It provides an easier way to allow users to
select assets from their photo library than using {% aTargetBlank
"https://developer.apple.com/documentation/uikit/uiimagepickercontroller",
"UIPickerController" %} from UIKit.

See the excellent YouTube videos from Stewart Lynch, {% aTargetBlank
"https://www.youtube.com/watch?v=gfUBKhZLcK0", "PhotosPicker - SwiftUI" %}.

## Example App

See {% aTargetBlank "https://github.com/mvolkmann/PhotosPickerDemo",
"PhotosPickerDemo" %} which demonstrates everything
shared in the Steward Lynch video linked above.

The app has two tabs.

<img alt="PhotosPicker Initial" style="border: 1px solid gray; width: 50%"
  src="/blog/assets/swiftui-photospicker-initial.png?v={{pkg.version}}">

### Single Image tab

The first tab "Single Image" allows the user
to select a single image from their photo library.
Tap the icon in the upper-right corner to view the photo library.

<img alt="PhotosPicker Single" style="border: 1px solid gray; width: 50%"
  src="/blog/assets/swiftui-photospicker-single.png?v={{pkg.version}}">

See the code for {% aTargetBlank
"https://github.com/mvolkmann/PhotosPickerDemo/blob/main/PhotosPickerDemo/Views/SingleImagePicker.swift",
"SingleImagePicker" %}.

### Multiple Images tab

The second tab "Multiple Images" allows the user
to select multiple images from their photo library.
Tap the icon in the upper-right corner to view the photo library.

<img alt="PhotosPicker Multiple" style="border: 1px solid gray; width: 50%"
  src="/blog/assets/swiftui-photospicker-multiple.png?v={{pkg.version}}">

See the code for {% aTargetBlank
"https://github.com/mvolkmann/PhotosPickerDemo/blob/main/PhotosPickerDemo/Views/MultipleImagePicker.swift",
"MultipleImagePicker" %}.
