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
The macOS version is included with Xcode and cannot be downloaded separately.

The app can import virtual objects from existing 3D models,
add animations to them, and add audio that can be triggered in many ways.
The app can also "record sensor and camera data in the actual location
you are building it for, and then replay it on your iOS device".

To launch the "Reality Composer" app in macOS, launch Xcode and
select Xcode ... Open Developer Tool .. Reality Composer.

### Terminology

- **project**: has one or more scenes
- **scene**: has one anchor and one or more objects
- **anchor**: determines how objects are positioned
- **object**: a 3D model that has any number of behaviors
- **behavior**: associated with one or more "affected objects";
  has one trigger and one or more effects
- **trigger**: determines what causes a behavior to execute
- **action**: changes one or more objects in specific ways

### Creating a Project

To create a new project:

- Click the "+" in the upper-right.
- Choose an anchor such as "Horizontal".
  This creates an initial scene containing a single white cube.
- To remove the cube, tap it and press the delete key.
  Alternatively, tap the cube, tap it again to get a popup of options,
  and tap "Delete".
- To delete the thing (what is it?) that was below the cube,
  use the same steps.

To rename a project:

- Tap "< Projects" in the upper-left.
- Tap the current project name which will be "New Project.rcproject".
- Type a new name, keeping the file extension `.rcproject`.
- This will be saved in "iCloud Drive"
  inside a directory named "Reality Composer"
  so it is also accessible from your Mac.
  Editing the project from any device or Mac that shares the iCloud account
  will edit the same project file.

### Creating a Scene

A project begins with one default scene.

To create another scene:

- Tap the left nav button in the upper-left
  to toggle display of the list of current scenes.
- Tap "+" after the title "Scenes".
- Select an anchor type.
- The new scene will be given a default name of "Scene" followed by a number
  or just "Scene" if no other scene has that name.

To select a scene:

- Tap the left nav button in the upper-left
  to toggle display of the list of current scenes.
- Tap a scene.

To rename a scene:

- Tap the left nav button in the upper-left
  to toggle display of the list of current scenes.
- Tap the scene to be renamed.
- Tap the properties button (cube in gear in circle) in the upper-right
  to open the properties panel on the right.
- Tap the name input at the top.
- Modify the scene name and press the return key.

To delete a scene:

- Tap the left nav button in the upper-left
  to toggle display of the list of current scenes.
- Long-press the scene to be deleted.
- Tap "Delete".

To duplicate a scene:

- Tap the left nav button in the upper-left
  to toggle display of the list of current scenes.
- Long-press the scene to be duplicated.
- Tap "Duplicate".

To enable switching from one scene to another when the project is running,
add a "Change Scene" action to a behavior and select a destination scene.
For example, this can be an action in a "Tap" behavior
so tapping an object triggers a scene change.
Perhaps this can be used to simulate exiting a room and entering another.

To transform the view of a scene:

- Pan by dragging two fingers on the screen.
- Rotate by dragging one finger on the screen.
- Zoom by pinching or spreading two fingers on the screen.

### Anchors

An anchor ties objects in a scene to a scene location such as
a coordinate relative to an iPhone location when the app launches.

The supported anchor types include:

- Horizontal: a horizontal plane that can represent a surface
  such as the floor or table top
- Vertical: a vertical plane that can represent a surface such as a wall
- Image: a real-world image such as a painting on a wall;
  objects in the scene will be positioned relative to the selected image
- Face: the face of a person
  objects in the scene will be positioned relative to any human face
- Object: a real-world object (ex. a globe)
  objects in the scene will be positioned relative to the object
  TODO: TRY THIS!

When an anchor is created in code, it must be added to a scene with
`scene.addAnchor(myAnchor)`.

### Creating Objects

To add an object to the scene:

- Tap the "+" in the upper-right.
- In the dialog that is displayed, select from a large set of predefined objects
  or tap "Import" to add your own model.
  The predefined objects are divided into
  the following categories and sub-categories:

  - Imported: models you imported from `.usdz` files
  - Basic: Shapes, Text, Arrows, Callouts, Signs, and Charts
  - Activities: Billiards, Checkers, Chess, Events, Games, Sports, and Toys
  - Arts: Frames and Music
  - Food: Baked Goods, Cookware, Drinks, Fruit, Meals, Scanned, Tableware, and Vegetables
  - Furnishings: Furniture
  - Nature: Scanned
  - Places: Houses and Locations
  - Retail: Shopping,
  - Storage: Storage
  - Symbols: Communication, Finance, Security, and Time
  - Work: Supplies, Technology, and Tools

  For example, tap the "Activities" category in the left nav,
  scroll down to the "Sports" section, and tap the football.
  This will download a 3D model and add it to the center of the scene.

- In the Inspector panel on the right, in the "Object Name" input,
  enter a name that will be used to refer to the object in code.

### Configuring an Object

To toggle display of the Inspector panel on the right side,
tap the properties button in the upper-right (cube in a gear in a circle).

To change the position, rotation, and scale of an object:

- Tap the object to select it.
- In the Inspector on the right, expand the "Transform" section.
- Modify the values under Position, Rotation, and Scale.
- The position can also be modified by dragging the red, blue, and green cones
  just outside the object that represent the x, y, z axes.
  TODO: Does the y-axis value specify the distance above the surface?
  TODO: Does the z-axis value specify the distance away?
- To rotate an object around a given axis
  where the origin is considered to be the center of the object,
  select an object, tap one of the colored cones outside it,
  and drag the ring that appears.
- To cause an object to snap to planes and other objects when dragged near it,
  tap the magnet button in the upper-left to toggle snap mode on.

To change the "look" of an object:

- Tap the object to select it.
- In the Inspector on the right, expand the "Look" section.
- Under "Style", tap the "Realistic", "Stylized", or "Iconic" button.

Basic shapes can also select a "Material" and a "Material Color".
Material options include "Glossy Paint", "Matte Paint", "Plastic", "Car Paint",
"Aluminum", "Brass", "Bronze", "Gold", "Steel", "Rubber", and "Terracotta".

Objects can be grouped so they can be transformed as a single unit.
To select all objects in the scene,
tap outside all of them and tap "Select All".
To select a subset of the objects,
hold down on one object and tap all the others.
After selecting the objects to be grouped,
tap any one of them to get a popup of options, and tap "Group".
To ungroup the objects in a group, tap the group to selected it,
tap again to get a popup of options, and tap "Ungroup".

### Physics

Objects can optionally participate in physics.
When participating objects are off the ground and their scene is played,
gravity causes them to fall toward the ground.

To cause an object to participate in physics:

- Tap the object to select it.
- In the Inspector on the right, expand the "Physics" section.
- Change the "Participates" toggle to be on.
- Select a "Motion Type" of either
  "Fixed" (never moves) or "Dynamic" (can move).
- Select a "Material" which can be "Lead", "Plastic", "Wood", or "Rubber".
  This affects how the object responds to
  collisions with surfaces or other objects.
- Select a "Collision Shape" which can be
  "Automatic", "Box", "Capsule", or "Sphere".

### Duplicating an Object

To duplicate an object, tap the object,
tap it again to get a popup of options, and tap "Duplicate".
By default this does not copy behaviors.
To do that, open the settings panel by tapping the ellipsis button
and toggle "Duplicate with Behaviors" on.

### Undo

To undo the last change, tap the undo button in the upper-left.

### Behaviors

Any number of behaviors can be added to each object.
Each behavior has a trigger and a sequence of actions.
The trigger and each action can be configured.

To add a behavior to the selected object:

- Tap the circle containing a right-pointing arrow in the upper-right.
  This opens an area at the bottom titled "Behaviors".
- Tap the "+" in the left nav to create a new behavior.
- In the popup that appears, select a behavior type which can be:
  - "Tap & Flip" flips the object when tapped.
  - "Tap & Play Sound" plays a sound when tapped.
  - "Tap & Add Force" adds "impulse force" when tapped.
    For example, this can be used to propel a ball into a wall.
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

### Triggers

Each trigger has a type and a set of "Affected Objects".
The supported trigger types include:

- "Tap" when an object in a specified set is tapped
- "Scene Start" when the scene is rendered
- "Proximity to Camera" when an object in a specified set
  is a specified distance from the camera
- "Collide" when an object from a specified set
  collides with an object in another specified set
- "Notification" when a notification with a specified identifier is received

### Actions

Each action has a type and a set of "Affected Objects".
The supported actions include:

- "Emphasize" has Duration, Motion Type, and Style.
- "Show" has Duration and Motion Type.
- "Hide" has Duration and Motion Type.
- "Move, Rotate, Scale To" has Duration, Ease Type, Position, Rotation, and Scale.
- "Move, Rotate, Scale By" has Duration, Ease Type, Position, Rotation, and Scale.
- "Add Force" has "Velocity".
- "Orbit" has Center, Duration, Revolutions, and Orbit Direction.
- "Spin" has Duration, Iteration, Spin Direction, and Axis.
- "Change Scene" has Scene.
- "Play Sound" has volume, Audio Clip, and Triggered while Running.
- "Play Ambience" has volume and Audio Clip.
- "Play Music" has volume and Audio Clip.
- "Wait" has "Duration".
- "USDZ Animation" has Duration, Iterations, and Triggered while Running.
- "Look at Camera" has Duration, Facing Direction, and Rotation.
- "Notify" has a behavior identifier.
  This can bve used to trigger another behavior.

To change a trigger, long-press its name, select "Replace",
and select a new trigger from the popup that appears.

To add another action to the action sequence of a behavior:

- Tap the "+" after the heading "Action Sequence".
- In the popup that appears, select a behavior type.

To cause an action to execute simultaneously with the action before it,
drag it on top of the previous action and drop it.

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

### Images

Images can be added to a model.
One way to do this is to:

1. Copy a photo from the Photos app to the Files app
   using the share sheet button.
1. Add a Frame object from the Arts category to a Reality Composer project.
1. Edit the Frame object and select the image from the Files app.

Another way is to drag an image into a Reality Composer scene.
This is easy to do in macOS, but it's unclear how to do this in iOS.

### Sounds

Reality Composer provides a large selection of sounds
and new sounds can be imported from apps like "Voice Memo".

To play a sound when an object is tapped:

1. Open the Voice Memo app.
1. Record a new voice memo.
1. Tap the share sheet button.
1. Save the voice memo in the Files app.
1. In Reality Composer, add a "Tap & Play Sound" behavior to an object.
1. In the Trigger, select an object that will be tapped to play the sound.
1. In the "Play Sound" action, tap "Choose" for "Audio Clip".
1. In the dialog that appears, either select a provided sound or tap "Import".
1. If "Import" was tapped, locate a sound file in the Files app,
   open it, and select the newly imported sound.

### Playing a Project

To play a project tap the play button in the upper-right.
This will begin executing behaviors that are
configured to begin when the scene starts.
It will also display a stop button at the bottom center.
Tap the stop button to return to editing the project.

### AR Mode

In iOS (iPhone or iPad) display a model and execute its behaviors
by toggling the "AR" button in the upper-right on.
This will ask for permission to access the camera
and them display the model on top of what the camera sees.
Tap the play button to execute the behaviors.

### Occlusion

To cause objects to occlude real-world objects when viewing them,
tap the "..." button in the upper-left and
toggle "Real-World Occlusion" to be on.

### Accessing a Project

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
  This will open it in "Reality Composer" on the Mac
  which is included with Xcode.
- Select each object one at a time and click the cloud icon
  in the upper-right to download the model for the object.
- Select File ... Save to save the downloaded models in the project.
  They will now appear in the Xcode view of the project.

### Exporting a Project

A project can be exported as a USDZ model (not easily edited)
or a Reality file (easily edited).

To export the current model:

1. Tap the ellipsis button to open the settings panel.
1. Tap Export.
1. Choose between exporting the entire project or only the current scene.
1. Tap "Export" in the upper-right.
1. Choose a location where it will be send such as
   AirDrop, Messages, Email, or "Save to Files".

TODO: It seems that Reality Composer now only supports
TODO: exporting as a `.reality` file?

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
