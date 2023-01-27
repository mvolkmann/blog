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

The "Reality Composer" iOS app (best used on an iPad)
and a macOS app (included with Xcode?) that allows users to
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
- To remove the cube, tap the shape and press the delete key.
  Alternatively, tap the shape, tap it again to get a popup of options,
  and tap "Delete".
- To delete the thing (what is it?) that was below the cube,
  use the same steps.

To transform the scene:

- To pan the scene, drag two fingers on the screen.
- To rotate the scene, drag one finger on the screen.
- To zoom the scene, pinch or spread two fingers on the screen.

To rename a project:

- Tap "< Projects" in the upper-left.
- Tap the current project name which will be "New Project.rcproject".
- Type a new name, keeping the file extension `.rcproject`.
- This will be saved in "iCloud Drive"
  inside a directory named "Reality Composer"
  so it is also accessible from your Mac.
  Editing the project from any device or Mac that shares the iCloud account
  will edit the same project file.

### Adding a Shape

To add a shape to the scene:

- Tap the "+" in the upper-right.
- In the dialog that is displayed, tap "Import" to add your own model
  or select a predefined model.
  For example, tap the "Activities" category in the left nav,
  scroll down to the "Sports" section, and tap the football.
  This will download a 3D model and add it to the center of the scene.
- In the Inspector panel on the right, in the "Object Name" input,
  enter a name that will be used to refer to the object in code.

### Configuring a Shape

To toggle display of the Inspector panel on the right side,
tap the button in the upper-right that contains a cube in a gear in a circle.

To change the position, rotation, and scale of an object:

- Tap the object to select it.
- In the Inspector on the right, expand the "Transform" section.
- Modify the values under Position, Rotation, and Scale.
- The position can also be modified by dragging the red, blue, and green cones
  just outside the object that represent the x, y, z axes.
  TODO: Does the y-axis value specify the distance above the surface?
  TODO: Does the z-axis value specify the distance away?
- To rotate a shape around a given axis
  where the origin is considered to be the center of the shape,
  select a shape, tap one of the colored cones outside it,
  and drag the ring that appears.
- To cause a shape to snap to planes and other objects when dragged near it,
  tap the magnet button in the upper-left to toggle snap mode on.

To change the "look" of an object:

- Tap the object to select it.
- In the Inspector on the right, expand the "Look" section.
- Under "Style", tap the "Realistic", "Stylized", or "Iconic" button.

Basic shapes can also select a "Material" and a "Material Color".
Material options include "Glossy Paint", "Matte Paint", "Plastic", "Car Paint",
"Aluminum", "Brass", "Bronze", "Gold", "Steel", "Rubber", and "Terracotta".

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

Shapes can be grouped so they can be transformed as a single unit.
To select all objects in the scene,
tap outside all of them and tap "Select All".
To select a subset of the objects,
hold down on one object and tap all the others.
After selecting the shapes to be grouped,
tap any one of them to get a popup of options, and tap "Group".
To ungroup the shapes in a group, tap the group to selected it,
tap again to get a popup of options, and tap "Ungroup".

### Duplicate

To duplicate a shape, tap the shape,
tap it again to get a popup of options, and tap "Duplicate".

### Undo

To undo the last change, tap the undo button in the upper-left.

### Behaviors

Any number of behaviors can be added to each object.
Each behavior has a trigger and a sequence of actions.
The trigger and each action can be configured.

Each trigger has a type and a set of "Affected Objects".
The supported trigger types include:

- "Tap" when an object in a specified set is tapped
- "Scene Start" when the scene is rendered
- "Proximity to Camera" when an object in a specified set
  is a specified distance from the camera
- "Collide" when an object from a specified set
  collides with an object in another specified set
- "Notification" when a notification with a specified identifier is received

Each action also has a type and a set of "Affected Objects".
Example action types include:

- "Emphasize" has "Duration", "Motion Type", and "Style".
- "Play Sound" has volume and "Audio Clip", and "Triggered while Running".
- "Add Force" has "Velocity".
- "Hide" has "Duration" and "Motion Type".
- "Wait" has "Duration".
- "Show" has "Duration" and "Motion Type".

To add a behavior to the selected object:

- Tap the circle containing a right-pointing arrow in the upper-right.
  This opens an area at the bottom titled "Behaviors".
- Tap the "+" in the left nav to create a new behavior.
- In the popup that appears, select a behavior type which can be:
  - "Tap & Flip" flips the object when tapped.
  - "Tap & Play Sound" plays a sound when tapped.
  - "Tap & Add Force" adds "impulse force" when tapped (What is this?).
  - "Start Hidden" hides the object when the scene begins.
  - "Wait & Show" shows the object after a configurable delay.
  - "Proximity & Jiggle" jiggles the object when near another.
  - "Custom" defines a custom behavior,
    beginning with no predefined trigger or actions.

Behaviors are given default names like "Behavior" and "Behavior 1".
To rename a behavior, long-press it in the left nav,
tap "Rename", and edit the name.
Since these names will become property names in code,
use a camelCase name that starts with a lowercase letter.

To change a trigger, long-press its name, select "Replace",
and select a new trigger from the popup that appears.

To add another action to the action sequence of a behavior:

- Tap the "+" after the heading "Action Sequence".
- In the popup that appears, select a behavior type.

To play a behavior in order to see its effect:

- Tap a behavior in the left nav to select it.
- Tap the play (triangle) button after "Action Sequence"
  to play the entire sequence.
- Tap the play (triangle) button in an action box
  to play only that action.

TODO: Is there a way to execute two actions at the same time
TODO: rather than in sequence?

Behaviors that have missing or invalid data in their trigger or actions
will have a warning icon after their name.
Select the behavior in the left nav. to repair it.
The trigger or actions that are invalid will have
the same warning icon in the upper-right of their boxes.

To copy a behavior:

- Long press it in the left nav.
- Tap "Duplicate".

To temporarily disable a behavior:

- Long press it in the left nav.
- Tap "Disable".

To delete a behavior:

- Long press it in the left nav.
- Tap "Delete".

### Occlusion

To cause shapes to occlude real-world objects when viewing them,
tap the "..." button in the upper-left and
toggle "Real-World Occlusion" to be on.

### Accessing Project File

To add a Reality Composer project that created on a iPad
and saved in "iCloud Drive" to an Xcode project on a Mac:

- Open the Finder.
- Select "iCloud Drive" in the left nav.
- Select the "Reality Composer" directory.
- Drag the `.rcproject` file into the Xcode Project Navigator.
- In the dialog that appears, check the "Copy items if needed" checkbox
  and click the "Finish" button.
- Click the `.rcproject` file in the Project Navigator.
- Click the "Open in Reality Composer" button in the upper-right.
  This will open it in "Reality Composer" on the Mac.
  This is not an app in the Applications directory.
  TODO: How did this get installed? Is it included with Xcode?
- Select each object one at a time and click the cloud icon
  in the upper-right to download the model for the object.
- Select File ... Save to save the downloaded models in the project.
  They will now appear in the Xcode view of the project.

## Example Project

See {% aTargetBlank "https://github.com/mvolkmann/ARKitDemo/", "ARKitDemo" %}.
