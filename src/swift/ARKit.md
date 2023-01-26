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

## Getting Started

1. In Xcode, create a SwiftUI project.
