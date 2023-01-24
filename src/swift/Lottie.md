---
eleventyNavigation:
  key: Lottie
  parent: Swift
layout: topic-layout.njk
---

## Overview

The {% aTargetBlank "https://airbnb.design/lottie/", "Lottie" %} is a
mobile, cross-platform, open-source library
for rendering vector motion graphics that are described by JSON files.
It was created by Airbnb.

Currently there is support for using Lottie in UIKit, but not SwiftUI.
However, we can wrap using the the UIKit version in a `UIViewRepresentable`
so it can be used in SwiftUI.

The iOS version of Lottie uses the {% aTargetBlank
"https://developer.apple.com/documentation/quartzcore", "Core Animation" %}
framework.

## Installing

To install Lottie in an Xcode project:

1. Select File ... Add Packages...
1. Paste "https://github.com/airbnb/lottie-spm.git"
   into the Search input in the upper-right.
1. Click the "Add Package" button.
1. Click the "Add Package" button in the next dialog that appears.

## Animation Files

Many Lottie animations are available at {% aTargetBlank
"https://lottiefiles.com", "LottieFiles" %}.
Downloading them requires creating a free account.

Many of the animations are free, but some require a paid account.
To see the free animations, select Discover ... Free Ready-to-use Animations.

A search input in the upper-right enables filtering the list of animations.
For example, enter "progress" to see a large number of animations
that can be used in place of a SwiftUI `ProgressView`.

To download an animation, click it.
This opens a dialog that can offer customization options.
Once all the options have been specified,
click the copy button under "Lottie Animation URL".
Paste this into a new browser tab and select File ... Save As...
in the web browser to save the animation in a JSON file.
This is easier than clicking the "Download" button at the top of the dialog
because that places the file in a "workspace" associated with your account
rather than directly downloading the file.

Add each animation JSON file to the project with the following steps:

- Move the JSON file into a directory within the project.
- Select File ... Add Files to "{project-name}"...
- In the dialog that appears, select the file to be added.
- Click the "Add" button.

## Stewart Lynch Solution

There is a fair amount of complexity in setting this up.
Stewart Lynch provides a fantastic solution in his YouTube video
{% aTargetBlank "https://www.youtube.com/watch?v=kUjHl7zfCeg",
"Lottie 4 in SwiftUI" %}.
The follow code is a modified version of what Stewart provides:

```swift
import Lottie
import SwiftUI

struct LottieView: UIViewRepresentable {
    let animationView: LottieAnimationView
    let contentMode: UIView.ContentMode
    let name: String // of the animation
    let loopMode: LottieLoopMode
    @Binding var play: Bool
    let speed: CGFloat

    init(
        name: String,
        loopMode: LottieLoopMode = .playOnce,
        speed: CGFloat = 1,
        contentMode: UIView.ContentMode = .scaleAspectFit,
        play: Binding<Bool> = .constant(true)
    ) {
        animationView = LottieAnimationView(name: name)
        self.contentMode = contentMode
        self.loopMode = loopMode
        self.name = name
        _play = play
        self.speed = speed
    }

    func makeUIView(context: Context) -> UIView {
        let view = UIView(frame: .zero)
        view.addSubview(animationView)

        animationView.contentMode = contentMode
        animationView.translatesAutoresizingMaskIntoConstraints = false
        animationView.heightAnchor.constraint(equalTo: view.heightAnchor)
            .isActive = true
        animationView.widthAnchor.constraint(equalTo: view.widthAnchor)
            .isActive = true
        animationView.loopMode = loopMode
        animationView.animationSpeed = speed

        return view
    }

    func updateUIView(_ uiView: UIView, context: Context) {
        if play {
            animationView.play { _ in play = false }
        }
    }
}
```

To display a Lottie animation, write code like the following:

```swift
struct ContentView: View {
    @State private var playConfetti = false

    var body: some View {
        ZStack {
            LottieView(
                name: "confetti", // required file name without .json
                speed: 2, // defaults to 1
                contentMode: .scaleAspectFill, // defaults to .scaleAspectFit
                // `play` defaults to true to play immediately just one time.
                play: $playConfetti
            )
            // If UI views that support user interaction will be
            // behind this animation, disable hit testing.
            // .allowsHitTesting(false)

            Button("Confetti") {
                playConfetti = true
            }
            .buttonStyle(.borderedProminent)
        }
        .ignoresSafeArea() // so confetti can cover the entire screen
    }
}
```

This works in the Preview area, in the Simulator, and on real devices.
