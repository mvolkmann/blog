---
eleventyNavigation:
  key: PhotosPicker
  parent: Swift
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

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

<img alt="PhotosPicker Initial" style="width: 50%"
  src="/blog/assets/swiftui-photospicker-initial.png?v={{pkg.version}}">

### Single Image tab

The first tab "Single Image" allows the user
to select a single image from their photo library.
Tap the icon in the upper-right corner to view the photo library.

<img alt="PhotosPicker Single" style="width: 50%"
  src="/blog/assets/swiftui-photospicker-single.png?v={{pkg.version}}">

See the code for {% aTargetBlank
"https://github.com/mvolkmann/PhotosPickerDemo/blob/main/PhotosPickerDemo/Views/SingleImagePicker.swift",
"SingleImagePicker" %}.

The `matching` argument to `PhotosPicker` accepts many values.
Examples from {% aTargetBlank
"https://www.hackingwithswift.com/quick-start/swiftui/how-to-let-users-select-pictures-using-photospicker",
"How to let users select pictures using PhotosPicker" %} by Paul Hudson include:

- `.screenshots` to only accept screenshots.
- `.any(of: [.panoramas, .screenshots])`
- `.not(.videos)` to accept anything that is not a video
- `.any(of: [.images, .not(.screenshots)]))`

### Multiple Images tab

The second tab "Multiple Images" allows the user
to select multiple images from their photo library.
Tap the icon in the upper-right corner to view the photo library.

<img alt="PhotosPicker Multiple" style="width: 50%"
  src="/blog/assets/swiftui-photospicker-multiple.png?v={{pkg.version}}">

See the code for {% aTargetBlank
"https://github.com/mvolkmann/PhotosPickerDemo/blob/main/PhotosPickerDemo/Views/MultipleImagePicker.swift",
"MultipleImagePicker" %}.

## Embedding

iOS 17 added the ability to embed a `PhotosPicker` in an app.
instead of rendering it in a sheet controlled by a separate process.
This is configured with three new view modifiers.

To display a `PhotosPicker` inline within an app,
apply the view modifier `photosPickerStyle(.inline)`.

To hide specific controls that the `PhotosPicker` supplies by default,
apply the `photosPickerDisabledCapabilities({capability})` view modifier.
where the supported capabilities are `collectionNavigation`, `search`,
`selectionActions`, `sensitivityAnalysisIntervention`, and `stagingArea`.
The `selectionActions` capability includes the "Clear" button (deselects all)
and the "Done" button (closes picker when not inline).

It seems there is no need to disable any of the capabilities
if they are configured to be hidden using the
`photosPickerAccessoryVisibility` view modifier.
The first argument specifies whether you will indicate
which edges containing controls should be `.visible` or `.hidden`.
The association of controls to edges differs based on the platform.
In iOS, the navigation bar is at the top and contains
the Clear button, the Done button,
and a segmented Picker to select "Photos" or "Albums".
In iOS, the toolbar is at the bottom and contains
the "Options" button and some text.
The following view modifier hides all edges:

```swift
.photosPickerAccessoryVisibility(.hidden, edges: .all)
```

See the demo app {% aTargetBlank
"https://github.com/mvolkmann/PhotosPicker2023Demo/blob/main/PhotosPicker2023Demo/ContentView.swift",
"PhotosPicker2023Demo" %}.

The app above renders a `PhotoPicker` with the following:

```swift
PhotosPicker(
    selection: $imageSelections,

    // Places checkmarks in blue circles on each selected image.
    selectionBehavior: .continuous, // value added in iOS 17

    // Places numbered blue circles on each selected image.
    // selectionBehavior: .continuousAndOrdered, // value added in iOS 17

    // matching: .images,

    preferredItemEncoding: .current

    // photoLibrary: .shared()
) {
    Image(systemName: "photo")
        .imageScale(.large)
}

// This embeds the PhotosPicker in this app instead of
// rendering it in a sheet controlled by a separate process.
.photosPickerStyle(.inline)

// This hides all controls normally rendered by the PhotosPicker.
.photosPickerAccessoryVisibility(.hidden, edges: .all)

// .ignoresSafeArea()
// The height of each row is 120.
.frame(height: 240) // two rows
```
