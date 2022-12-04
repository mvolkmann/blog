---
eleventyNavigation:
  key: SpriteKit
  parent: Swift
layout: topic-layout.njk
---

The {% aTargetBlank "https://developer.apple.com/documentation/spritekit/",
"SpriteKit" %} framework supports drawing text, shapes, images, and video.
It supports physics behaviors which makes it useful for creating games.

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
