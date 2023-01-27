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

One of the most important classes provided by ARKit is {% aTargetBlank
"https://developer.apple.com/documentation/realitykit/arview", "ARView" %}.
This displays a camera stream on a device.

{% aTargetBlank "https://developer.apple.com/documentation/realitykit/",
"RealityKit" %} is another Apple framework that is often used with ARKit.
It renders 3D images and simulates changes to them.
It can be used to add virtual objects to an ARKit view.

## Resources

Florian Schweizer (@FloWritesCode) created the following
excellent YouTube videos on ARKit:

- [Intro to ARKit 01: Project Setup & ARView](https://www.youtube.com/watch?v=lamIxNozxv4)
- [Intro to ARKit 02: Placing Object](https://www.youtube.com/watch?v=vL-JKo_wtxM)
- [Intro to ARKit 03: Communication via Combine](https://www.youtube.com/watch?v=KbqbU-cCKf4)
- [Intro to ARKit 04: RealityComposer](https://www.youtube.com/watch?v=brko7EoYJAk)

## Configuration

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

## Anchors

An anchor ties entities to a location in a scene such as
a coordinate relative to an iPhone location when the app launches.
There are several kinds of anchors including
a 3D coordinate (specified by a 3x3 matrix of the type `SIMD3<Scalar>`),
a horizontal plane (ex. the floor or a desk top),
a vertical plane (ex. a wall), a face, a body, and image in the scene.

After creating an anchor it must be added to a scene with
`scene.addAnchor(myAnchor)`.

## Entities

An entity is a 3D object that can be placed in a scene.
There are many ways to load entities into a project.
One option is to use a USDZ file.
The USDZ file format was created by Pixar.
Many USDZ files can be downloaded from the {% aTargetBlank
"https://developer.apple.com/augmented-reality/quick-look/",
"Apple Developer website" %}.

To load an entity from a file and place it at an anchor:

```swift
let entity = try? Entity. load (named: "usdz-file-name")
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
1. TODO: Finish this!

## Reality Composer

The "Reality Composer" iOS app (best used on an iPad) allows users to
"easily prototype and produce AR experiences directly
with no prior 3D experience".
This includes creating virtual objects from existing 3D models,
adding animations to them, and
adding audio that can be triggered in many ways.
The app can "record sensor and camera data in the actual location
you are building it for, and then replay it on your iOS device".

To create a new "Reality Composer" project:

- Click the "+" in the upper-right.
- Choose an anchor such as "Horizontal".
  This creates an initial scene containing a single white cube.
- To pan, rotate, and zoom the scene, use two fingers on the screen.
- To remove the cube, select add press the delete key
  or long-press the cube and tap "Delete".
- To delete the thing (what is it?) that was below the cube,
  use the same steps.
- Tap "< Projects" in the upper-left.
- Tap the current project name which will be "New Project.rcproject".
- Type a new name, keeping the file extension `.rcproject`.
- This will be saved in "iCloud Drive"
  inside a directory named "Reality Composer"
  so it is also accessible from your Mac.

To add a shape to the scene:

- Tap the "+" in the upper-right.
- In the dialog that is displayed, tap "Import" to add your own model
  or select a predefined model.
  For example, tap the "Activities" category in the left nav,
  scroll down to the "Sports" section, and tap the football.
  This will download a 3D model and add it to the center of the scene.

To change the position, rotation, and scale of an object:

- Tap the object to select it.
- In the Inspector on the right, expand the "Transform" section.
- Modify the values under Position, Rotation, and Scale.
- The position can also be modified by dragging the red, blue, and green cones
  just outside the object that represent the x, y, z axes.
  TODO: Does the y-axis value specify the distance above the surface?
  TODO: Does the z-axis value specify the distance away?

To change the "look" of an object:

- Tap the object to select it.
- In the Inspector on the right, expand the "Look" section.
- Under "Style", tap the "Realistic", "Stylized", or "Iconic" button.

To cause an object to participate in physics:

- Tap the object to select it.
- In the Inspector on the right, expand the "Physics" section.
- Change the "Participates" toggle to be on.
- Select a "Motion Type" of either "Fixed" or "Dynamic".
- Select a "Material" which can be "Lead", "Plastic", "Wood", or "Rubber".
  This affects how the object responds to
  collisions with surfaces or other objects.
- Select a "Collision Shape" which can be
  "Automatic", "Box", "Capsule", or "Sphere".
