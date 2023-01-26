---
eleventyNavigation:
  key: ARKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/arkit", "ARKit" %}
is an Apple framework for rendering augmented reality experiences.

One of the most important classes provided by ARKit is {% aTargetBlank
"https://developer.apple.com/documentation/realitykit/arview", "ARView" %}.
This displays a camera stream on a device.

{% aTargetBlank "https://developer.apple.com/documentation/realitykit/",
"RealityKit" %} is another Apple framework that is often used with ARKit.
It renders 3D images and simulates changes to them.
It can be used to add virtual objects to an ARKit view.

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
Many of these can be downloaded from the Apple Developer website.

To load an entity from a file and place it at an anchor:

```swift
let entity = try? Entity. load (named: "usdz-file-name")
anchor.addChild(entity)
```

The Apple {% aTargetBlank
"https://developer.apple.com/augmented-reality/tools/",
"Augmented Reality Tools" %} web page contains links for downloading
the tools "Reality Composer" (iPhone and iPad only) and "Reality Converter".

The **Reality Composer** iOS app (best used on an iPad) allows users to
"easily prototype and produce AR experiences directly
with no prior 3D experience".
This includes creating virtual objects, adding animations to them,
adding audio that can be triggered in many ways.
The app can "record sensor and camera data in the actual location
you are building it for, and then replay it on your iOS device".

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
1.
