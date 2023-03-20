---
eleventyNavigation:
  key: SpriteKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

The {% aTargetBlank "https://developer.apple.com/documentation/spritekit/",
"SpriteKit" %} framework supports drawing text, shapes, images, and video.
It supports physics behaviors which makes it useful for creating games.

## Creating a Game

1. Open Xcode.
1. Select File ... New ... Project...
1. Select iOS ... Game.
1. Enter a "Product Name", preferably with no spaces or special characters.
1. Leave "Language" set to "Swift".
1. Leave "Game Technology" set to "SpriteKit".
1. Check the "Integrate GameplayKit" checkbox.
1. Click the "Next" button.
1. Select a directory where the project will be saved.
1. Click the "Create" button.

## Example

Here is a simple example derived from the
"Hacking With Swift" article {% aTargetBlank
"https://www.hackingwithswift.com/quick-start/swiftui/how-to-integrate-spritekit-using-spriteview",
"How to integrate SpriteKit using SpriteView" %}.

Tap the black scene area to drop a box with a random fill color.
The box will fall to the bottom and interact with previous boxes
using physics effects.

```swift
import SpriteKit
import SwiftUI

class GameScene: SKScene {
    let boxSize = 50

    override func didMove(to view: SKView) {
        physicsBody = SKPhysicsBody(edgeLoopFrom: frame)
    }

    private func randomColor() -> UIColor {
        UIColor(
            red: .random(in: 0 ... 1),
            green: .random(in: 0 ... 1),
            blue: .random(in: 0 ... 1),
            alpha: 1
        )
    }

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard let touch = touches.first else { return }
        let location = touch.location(in: self)
        let size = CGSize(width: boxSize, height: boxSize)
        let box = SKSpriteNode(color: randomColor(), size: size)
        box.position = location
        box.physicsBody = SKPhysicsBody(rectangleOf: size)
        addChild(box)
    }
}

struct ContentView: View {
    let size = CGSize(width: 300, height: 400)
    var scene: SKScene {
        let scene = GameScene()
        scene.size = size
        scene.scaleMode = .fill
        return scene
    }

    var body: some View {
        VStack {
            Text("Tap to drop a box.").font(.title)
            SpriteView(scene: scene)
                .frame(width: size.width, height: size.height)
                .ignoresSafeArea()
            Spacer()
        }
    }
}
```

## Game Projects

One way to begin creating a game is to create a new project in Xcode
using the "Game" template. This uses UIKit rather than SwiftUI.

## Resources

- {% aTargetBlank "https://www.youtube.com/watch?v=TJfh8wXbfEw",
  "Building a SpriteKit game with physics, particles, and shaders" %}
- {% aTargetBlank "https://www.youtube.com/watch?v=0-lM51yI-PA",
  "Building a space shooter with SpriteKit and Codable" %}

## Random Notes

In the debugging info in the lower right corner,
why are the number of nodes shown but the FPS is cut off?

Can set the friction on nodes.

Does the light blue outline added by settings show physics to true
follow the edge of images that have no background?

See the debugging options showNodes and showFPS that can be set on a SKView.

When does the SKScene method didMove get invoked?

Nodes are identified by name. Use an if let statement to try to find them.

To avoid a large number of conditional assignments
when assigning properties to a physicsBody, do this:

```swift
if let body = node.physicsBody { … }
```

See the SKScene method update which is called every time before it renders.

Describe the difference between the properties, anchorPoint in position.

When you have an issue with nodes moving and being affected by friction,
maybe you can fix it by turning off allowsRotation.

Document the touchDown, touchMoved, and touchUp methods.
