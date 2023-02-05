---
eleventyNavigation:
  key: ARKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/arkit", "ARKit" %}
is an Apple framework for rendering augmented reality experiences
in iOS and iPadOS.
ARKit utilizes device front and rear facing cameras and motion sensors.

{% aTargetBlank "https://developer.apple.com/documentation/realitykit/",
"RealityKit" %} is another Apple framework that is often used with ARKit.
It renders 3D images and simulates changes to them.
RealityKit can be used to add virtual objects to an ARKit view.

ARKit and RealityKit use the USDZ file format to describe 3D objects.
The USDZ file format was created by Pixar.
Many USDZ files can be downloaded from the {% aTargetBlank
"https://developer.apple.com/augmented-reality/quick-look/",
"Apple Developer website" %}.

## Resources

Florian Schweizer (@FloWritesCode) created the following
excellent YouTube videos on ARKit:

- [Intro to ARKit 01: Project Setup & ARView](https://www.youtube.com/watch?v=lamIxNozxv4)
- [Intro to ARKit 02: Placing Object](https://www.youtube.com/watch?v=vL-JKo_wtxM)
- [Intro to ARKit 03: Communication via Combine](https://www.youtube.com/watch?v=KbqbU-cCKf4)
- [Intro to ARKit 04: RealityComposer](https://www.youtube.com/watch?v=brko7EoYJAk)

## Reality Composer

"Reality Composer" is an iOS and macOS app that allows users to
"easily prototype and produce AR experiences directly
with no prior 3D experience".
See [RealityComposer](/blog/swift/RealityComposer).

## Object Capture

The "Object Capture" API that is part of the RealityKit framework
processes a series of object photos taken from many angles
and produces a 3D model in the USDZ format.

Apple provides the HelloPhotogrammetry Xcode project
that uses the Object Capture API.
It can be downloaded at {% aTargetBlank
"https://developer.apple.com/documentation/realitykit/creating_a_photogrammetry_command-line_app",
"Creating a Photogrammetry Command-Line App" %}.
Building this project creates a command-line app
that generates a model file from a collection of model photos.

To build this app:

1. Double-click the file `HelloPhotogrammetry.xcodeproj`
   to open the project in Xcode.
1. Select the top entry in the Project Navigator.
1. Select the top target.
1. Select the General tab.
1. Select a value in the "Team" dropdown.
1. Select File ... Project Settings...
1. In the dialog that appears, change the "Derived Data" dropdown
   to "Custom Location".
1. Enter the path to your home directory or another easily located directory.
1. Click the scheme dropdown to the left of the device dropdown
   at the top of the Xcode window.
1. Select "Edit Scheme..."
1. In the dialog that appears, click "Run" in the left nav.
1. Click the "Info" tab.
1. Change the "Build Configuration" dropdown from "Debug" to "Release".
1. Click the "Close" button.
1. Select Product ... Build For ... Running
1. Wait for the build to finish.
1. Copy or move the file named "HelloPhotogrammetry"
   found below the specified directory in Build/Products/Release
   to a directory in your PATH.

Models can be produced in four detail levels which are
Reduced, Medium, Full, and Raw (for custom workflows).
The Reduced and Medium levels are good for use in web and mobile apps.
The Full and Raw levels are intended for high-end usage.
Medium detail models can be viewed in the ARQuickLook macOS app.
To display multiple scans in the same scene, use the Reduced level.

To run this app:

1. From a Terminal window enter `HelloPhotogrammetry`.
   This will output usage instructions.
1. Enter `HelloPhotogrammetry {images-dir-path} {output-file-path}`
   to generate a `.usdz` file from the images in a given directory.
1. To specify a detail level, add the `-d {level}` option
   where `{level}` is `preview`, `reduced`, `medium`, `full`, or `raw`.

To create your own app for reading images and producing `.usdz` files,
write code similar to the following:

```swift
import RealityKit

// Create a session.
let url = URL(fileURLWithPath: "/tmp/photos/", isDirectory: true)
let session = try! PhotogrammetrySession(
    input: url,
    configuration: PhotogrammetrySession.Configuration()
)

// Prepare to process outputs.
Task {
    for try await output in session.outputs {
        switch output {
        case .requestProgress(let request, let fraction):
            print("Progress:", fraction)
        case .requestComplete(let request, let result):
            if case .modelFile(let url) = result {
                print("Output at", url)
            }
        case .requestError(let request, let error):
            print("Error:", error)
        case .processingComplete:
            print("Completed")
        default:
            print("Unhandled case")
        }
    }
}

// Initiate processing of input photos.
// This request two models to be produced.
try! session.process(requests: [
    .modelFile("/tmp/models/reduced.usdz", detail: .reduced),
    .modelFile("/tmp/models/medium.usdz", detail: .medium)
])
```

## Writing Code

An Xcode project that uses ARKit can be created in two ways.
It can be an SwiftUI "App" or
an "Augmented Reality App" which does not use SwiftUI.
We will only consider the first option here.

Currently ARKit views are compatible with UIKit, but not SwiftUI.
We need to wrap the `ARView` in a `UIViewRepresentable`
in order to access it from SwiftUI.

1. In Xcode, create a SwiftUI project.
1. Select the top entry in the Project Navigator.
1. Select the main target.
1. Select the "Info" tab.
1. Hover over any row and click the "+" button.
1. For the value, describe for users why camera access is needed.
   For example, "Need for augmented reality view".
1. See code in the example project {% aTargetBlank
   "https://github.com/mvolkmann/ARKitDemo/", "ARKitDemo" %}.

### ARView Configuration

To change the configuration of an `ARView` session,
pass a configuration object to the `run` method of the {% aTargetBlank
"https://developer.apple.com/documentation/arkit/arsession", "ARSession" %}  
object stored in the `session` property of the `ARView` object.
Supported configuration types include:

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/arbodytrackingconfiguration", "ARBodyTrackingConfiguration" %}
  "tracks human body poses, planar surfaces, and images using the rear-facing camera".

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/arfacetrackingconfiguration", "ARFaceTrackingConfiguration" %}
  "tracks facial movement and expressions using the front camera".

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/argeotrackingconfiguration", "ARGeoTrackingConfiguration" %}
  "tracks locations with GPS, map data, and a device's compass".
  For a list of supported cities, scroll to the
  "Supported Areas and Cities" section of the `ARGeoTrackingConfiguration` page.

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/arimagetrackingconfiguration", "ARImageTrackingConfiguration" %}
  "tracks known images using the rear-facing camera".

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/arobjectscanningconfiguration", "ARObjectScanningConfiguration" %}
  "recognizes objects and collects high-fidelity data about specific objects using the rear-facing camera".

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/arorientationtrackingconfiguration", "AROrientationTrackingConfiguration" %}
  "tracks only the device’s orientation using the rear-facing camera".

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/arpositionaltrackingconfiguration", "ARPositionalTrackingConfiguration" %}
  "tracks only the device’s position in 3D space".

- {% aTargetBlank "https://developer.apple.com/documentation/arkit/arworldtrackingconfiguration", "ARWorldTrackingConfiguration" %}
  "tracks the position of a device in relation to objects in the environment".

### Important RealityKit Types

- {% aTargetBlank "https://developer.apple.com/documentation/realitykit/arview", "ARView" %}
  class manages and displays a single `Scene`
  along with a camera stream on a device.
- {% aTargetBlank "https://developer.apple.com/documentation/realitykit/scene", "Scene" %}
  class holds a `Scene.AnchorCollection`.
- {% aTargetBlank "https://developer.apple.com/documentation/realitykit/scene/anchorcollection", "Scene.AnchorCollection" %}
  struct holds a collection of `AnchorEntity` objects that are accessed by subscript.
- {% aTargetBlank "https://developer.apple.com/documentation/realitykit/anchorentity/", "AnchorEntity" %}
  class inherits from `Entity` and anchors any number of entities to a `Scene`.
- {% aTargetBlank "https://developer.apple.com/documentation/realitykit/entity", "Entity" %}
  class instances represent virtual or physical objects that can have appearance and behaviors.
- {% aTargetBlank "https://developer.apple.com/documentation/realitykit/modelentity", "ModelEntity" %}
  class is a subclass of `Entity`.
  Instances represent physical objects to be rendered.

There are many ways to load entities.
One option is to use a USDZ file.

To load an entity from a file and place it at an anchor:

```swift
let entity = try? Entity.load(named: "usdz-file-name")
anchor.addChild(entity)
```

The Apple {% aTargetBlank
"https://developer.apple.com/augmented-reality/tools/",
"Augmented Reality Tools" %} web page contains links for downloading
the tools "Reality Composer" (iPhone and iPad only) and "Reality Converter".

[Reality Composer](#reality-composer) is described below.

The **Reality Converter** macOS app "converts common 3D file formats such as
`.obj`, `.gltf`, `.fbx`, and `USD` to `USDZ`.
It also lets you view the converted model,
customize its material properties with your own textures,
and edit the file's metadata."

Files produced by these apps can be loaded into an ARKIt app
in the same way as USDZ files.

Entities can be created using the {% aTargetBlank
"https://developer.apple.com/documentation/realitykit/meshresource",
"MeshResource" %} class provided by ARKit.
This has the methods `generateBox`, `generatePlane`, and `generateSphere`.
For example, to create and display a blue box:

```swift
let block = MeshResource.generateBox(size: 1) // 1 meter on all sides
let material = SimpleMaterial(color: UIColor(Color.blue), isMetallic: false)
let entity = ModelEntity(mesh: block, materials: [material])
let anchor = AnchorEntity(plane: .horizontal)
anchor.addChild(entity)
scene.addAnchor(anchor)
```

## Getting Started

## CaptureSample App

Apple provides the CaptureSample project that implements an iOS app
for capturing photos of a real-world object
in preparation for generating a USDZ model file.
The project can be downloaded from {% aTargetBlank
"https://developer.apple.com/documentation/realitykit/taking_pictures_for_3d_object_capture",
"Taking Pictures for 3D Object Capture" %}.

I was able to run this app and capture images, but the images
were not saved anywhere so I could not use them to create a USDZ model file.
Consider using the app PhotoCatch in the iOS App Store.

To build and run the CaptureSample app:

1. Double-click the file `CaptureSample.xcodeproj`
   to open the project in Xcode.
1. Select the top entry in the Project Navigator.
1. Select the top target.
1. Select the General tab.
1. Select a value in the "Team" dropdown.
1. Attach an iPhone to the computer with a USB cable.
1. Select the iPhone from device dropdown at the top of the Xcode window.
1. Press cmd-r to build and run the app on the iPhone.

## Pre-built models

Apple provides some pre-built USDZ at {% aTargetBlank
"https://developer.apple.com/augmented-reality/quick-look/", "AR Quick Look" %}.

## Viewing Models

Double-clicking a `.usdz` file opens and displays the model
in Xcode using "AR Quick Look".
Drag around on the image to rotate it around any axis to see all sides.
Some models appear in shades of black when viewed in Xcode,
but will be in color when imported into the "Reality Composer" app.

## Guidelines for Creating Models

When creating models from real-world objects:

- Choose an object that has adequate texture detail.
  If an object has low texture or transparent portions,
  the resulting model will lack detail.
- Choose an object that does not have highly reflective regions.
- If the object will need to be moved to take photos at various angles,
  choose a rigid object that will not change its shape when moved.
- Place the object on an uncluttered background such as a solid white table
  so it clearly stands out.
- Move slowly around the object while taking photos
  to capture a uniform set of views.
  Alternatively place the camera on a tripod, place the object on a turntable,
  and turn it to capture photos and various angles.
  Consider using timed shutter mode that is synced to a motorized turntable.
- To capture the bottom of the object,
  flip it over and repeat the process of taking photos.
- Take between 20 and 200 photos.

## Example Project

See {% aTargetBlank "https://github.com/mvolkmann/ARKitDemo/", "ARKitDemo" %}.
