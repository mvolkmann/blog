---
eleventyNavigation:
  key: Animation
  parent: Swift
layout: topic-layout.njk
---

## Overview

Many view properties can be animated.
These include color, opacity, rotation, and scale.
Note that font sizes cannot be animated,
but views containing text can be scaled.

SwiftUI supports three ways of implementing animations.

- explicit: wraps code that changes a model or `@State` data
  with a call to `withAnimation`
- implicit: uses the `animation` view modifier
- transition: triggers when a view is inserted or removed

Explicit animations are the most commonly used
because they are triggered by model/state changes
which are typically made in response to user interactions.
These can cause multiple views to animate concurrently.
For example, the action of a `Button` can wrap calls to
ViewModel intent functions in a closure passed to `withAnimation`.
Most code that handles user events does this.

Key points to remember when implementing animations:

1. Only changes to view modifier arguments and shapes are animated.
2. Only views that are already on the screen
   are affected by explicit and implicit animations.
3. The `animation` view modifier applies to
   all view modifiers chained before it, but not to those chained after it.
4. Explicit animations do not override or prevent implicit animations.
   Both can be applied concurrently.
5. Animations are automatically and smoothly interrupted by new animations.

One way to achieve point #2 is to leave views on the screen permanently,
but conditionally hide them by setting their opacity to zero.
For example, `myView.opacity(show ? 1 : 0)`.

Animations can specify a duration (in seconds), delay, easing function,
and number of times to repeat.
Duration is the total time over which the animation takes place.
Delay is the amount of time the animation waits to begin after being triggered.
An easing function controls the speed at which an animation advances
over its duration.

Provided easing functions include
`linear`, `easeIn`, `easeOut`, `easeInOut` (default in iOS 16 and earlier),
`spring` (default in iOS 17 and later), `interactiveSpring`,
`interpolatingSpring`, `bouncy`, `smooth`, and `snappy`.
These are static functions on the `Animation` struct.
Most of them take a single, optional argument
which is the `duration` in seconds.
The `spring` function takes three optional arguments named
`response`, `dampingFunction`, and `blendDuration`.
For details, see the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation/spring(response:dampingfraction:blendduration:)",
"spring method" %}.

Custom easing functions can be defined with the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation/timingcurve(_:_:_:_:duration:)",
"timingCurve" %} function.

To begin an {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation", "Animation" %},
apply the `onAppear` view modifier to the view,
passing it a closure that calls `withAnimation`.
This is demonstrated in the [Marching Ants Border](#marching-ants-border)
section below.

To delay the start of an {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation", "Animation" %},
chain a call to its {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/animation/delay(_:)",
"delay" %} method .
For example:

```swift
let duration = 1.5 // seconds
withAnimation(.linear(duration: duration).delay(duration)) {
    ...
}
```

### Basic Examples

The following example provides form elements
for experimenting with different kinds of animations.

<img alt="SwiftUI Animation" style="width: 40%"
  src="/blog/assets/SwiftUI-Animation.png?v={{pkg.version}}"
  title="SwiftUI Animation">

```swift
enum EasingType: String, CaseIterable {
    case forever, linear, easeIn, easeOut, easeInOut, spring
}

struct ContentView: View {
    @State private var borderColor: Color = .red
    @State private var color = false
    @State private var easingType: EasingType = .linear
    @State private var on = false
    @State private var opacity = false
    @State private var rotate = false
    @State private var scale = false

    private var easingFunction: Animation {
        switch easingType {
        case .linear: return Animation.linear(duration: 1)
        case .forever: return Animation
            .linear(duration: 2)
            .repeatForever(autoreverses: false)
        case .easeIn: return Animation.easeIn(duration: 2)
        case .easeOut: return Animation.easeOut(duration: 2)
        case .easeInOut: return Animation.easeInOut(duration: 2)
        case .spring: return Animation
            .spring(dampingFraction: 0.5)
            .speed(0.3)
        }
    }

    var body: some View {
        VStack {
            VStack {
                Text("First Line")
                Text("Second Line")
                Text("🎉").font(.largeTitle)
            }
            .padding()
            .border(borderColor, width: 10)
            .opacity(!opacity || on ? 1 : 0)
            .scaleEffect(!scale || on ? 1 : 0)
            .rotationEffect(.degrees(!rotate || on ? 0 : 360))
            .animation(easingFunction, value: on ? 1 : 0) // implicit animation

            NavigationView { // Picker will be disabled without this.
                Form {
                    Toggle("Animate Color?", isOn: $color)
                    Toggle("Animate Opacity?", isOn: $opacity)
                    Toggle("Animate Rotation?", isOn: $rotate)
                    Toggle("Animate Scale?", isOn: $scale)
                    Picker("Easing Function", selection: $easingType) {
                        ForEach(EasingType.allCases, id: \.self) { easingType in
                            Text("\(easingType.rawValue)").tag(easingType)
                        }
                    }
                    Button("Toggle") {
                        if color {
                            borderColor =
                                borderColor == .red ? .blue : .red
                        }
                        on.toggle()
                    }
                }
            }
        }
    }
}
```

### Transitions

Transitions can be applied to any kind of view including container views.
They are specified when a view is defined,
but they are only applied when the view is inserted or removed.
This is implemented by using an `if` or `switch` statement inside a parent view.

By default an `opacity` transition (fade) is used.
This can be changed by applying the `transition` view modifier
which is passed the kind of {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/anytransition",
"AnyTransition" %} to perform.

Transitions are defined as static properties on the `AnyTransition` struct.
These include `move`, `opacity`, `scale`, and `slide`.
The `identity` constant is used to specify that no transition should occur.
Custom transitions can also be implemented.

By default the transition is reversed when the view is removed.
For example, `opacity` changes from 0 to 1 when the view is inserted,
and from 1 to 0 when it is removed.
This can be changed using the `asymmetric` transition function
which allows specifying one transition for insertion
and a different one for removal.

Most insertion transitions do not currently work in Preview,
but they do work in the Simulator.
However, the Simulator often displays odd rendering artifacts.

The following example demonstrates sliding a view
in from the top when it is inserted and
out to the right when it is removed.

```swift
struct ContentView: View {
    @State private var include = false

    let easeFn = Animation.easeInOut(duration: 1)

    var body: some View {
        VStack {
            VStack {
                if include {
                    Text("Conditionally Included")
                        .frame(maxWidth: .infinity) // use full screen width

                        // This slides in from left and out to right.
                        //.transition(.slide)

                        // The animation call here should apply the
                        // easing function when one is not specified
                        // in the call to withAnimation, but it doesn't work.
                        //.transition(.slide.animation(easeFn))

                        .transition(.asymmetric(
                            insertion: .move(edge: .top),
                            removal: .move(edge: .trailing)
                        ))

                        // This changes scale from 0 to 1 for insertion
                        // and 1 to 0 for removal.
                        //.transition(.scale)
                }
            }
            .frame(maxWidth: .infinity, minHeight: 50) // use full width
            .border(.red)

            NavigationView {
                Form {
                    // If a Toggle is used instead of a Button
                    // to toggle the value of "include",
                    // there is no opportunity to use "withAnimation".
                    //Toggle("Include Optional Text?", isOn: $include)

                    Button("Toggle Optional Text") {
                        withAnimation(easeFn) {
                            include.toggle()
                        }
                    }
                }
            }

        }
    }
}
```

Transitions can be combined. For example, we can create a variable
that holds a description of a combined transition as follows
and pass it to the `transition` view modifier.

```swift
    private let transition: AnyTransition =
        .move(edge: .bottom)
        .combined(with: .scale)
        .combined(with: .opacity)
```

### Binding Change Animations

An animation can be attached to a binding so the animation occurs
when views change as a result of the binding value.
For example, a `Bool` binding can be used to
determine whether a view should be shown or hidden.
The following code demonstrates this:

<img alt="SwiftUI Binding Animation" style="width: 50%"
  src="/blog/assets/SwiftUI-Binding-Animation.png?v={{pkg.version}}"
  title="SwiftUI Binding Animation">

```swift
struct ContentView: View {
    @State private var isShowing = false
    @State private var transitionName = "opacity"
    private let duration = 0.5
    private let transitionNames = ["move", "opacity", "scale", "slide"]

    private var transition: AnyTransition {
        switch transitionName {
        case "move": return .move(edge: .trailing)
        case "scale": return .scale
        case "slide": return .slide
        default: return .opacity
        }
    }

    var body: some View {
        VStack {
            Toggle(
                "Show Greeting?",
                isOn: $isShowing.animation(.easeInOut(duration: 1))
            )

            LabeledContent("Transition") {
                Picker("Transition", selection: $transitionName) {
                    ForEach(transitionNames, id: \.self) { name in
                        Text(name).tag(name)
                    }
                }
                .pickerStyle(.segmented)
            }

            if isShowing {
                Text("Hello, Animation!")
                    .font(.largeTitle)
                    .frame(maxWidth: .infinity)
                    .transition(transition)
            }

            // This stops the Toggle above from bouncing when the
            // animation is applied, which may or may not be desirable.
            Spacer()
        }
        .padding()
    }
}
```

The line above what uses the `.easeInOut` transition
could be replaced by one of these:

```swift
                // Default spring animation
                // (doesn't bounce at all with the default parameter values!)
                isOn: $isShowing.animation(.spring())

                // Customized spring animation using the spring method
                isOn: $isShowing.animation(
                    .spring(
                        response: duration, // defaults to 0.55
                        dampingFraction: 0.2, // defaults to 0.825
                        blendDuration: duration // defaults to 0
                    )
                )

                // Customized spring animation using the
                // interactiveSpring method which is the same as .spring(),
                // but whose parameters have different default values
                isOn: $isShowing.animation(
                    .interactiveSpring(
                        response: duration, // defaults to 0.15
                        dampingFraction: 0.2, // defaults to 0.86
                        blendDuration: duration // defaults to 0.25
                    )
                )

                // Customized spring animation using the
                // interpolatingSpring method which handles overlapping
                // animations better than .spring() and .interactiveSpring()
                isOn: $isShowing.animation(
                    .interpolatingSpring(
                        // mass: 1, // defaults to 1
                        stiffness: 100,
                        // required; lower values are more stiff
                        damping: 3 // required; lower values bounce more
                        // initialVelocity: 0 // defaults to 0; must be in [0, 1]
                    )
                )
```

### matchedGeometryEffect

The {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/matchedgeometryeffect(id:in:properties:anchor:issource:)",
"matchedGeometryEffect" %} view modifier is used to
smoothly move views between container views.
For example, this can be used to move `Text` views that describe food items
between lists of foods that available and those that have been selected.
Each food item must have a unique id.
When rendering the food items, `matchedGeometryEffect` is used
to associate the `Text` view with a particular id in a given namespace.

The following code demonstrates this:

```swift
struct Food: Identifiable {
    var name: String
    var selected: Bool = false
    var id: String { name }
}

struct ContentView: View {
    @Namespace private var foodNS

    @State private var foods: [Food] = [
        Food(name: "Hamburger"),
        Food(name: "Fries"),
        Food(name: "Shake"),
    ]

    func foodList(selected: Bool) -> some View {
        List(foods) { food in
            if food.selected == selected {
                Text(food.name)
                    .matchedGeometryEffect(id: food.id, in: foodNS)
                    .onTapGesture {
                        withAnimation { toggle(food: food) }
                    }
            }
        }
    }

    func toggle(food: Food) {
        let index = foods.firstIndex(where: { $0.id == food.id })!
        foods[index].selected.toggle()
    }

    var body: some View {
        HStack {
            VStack {
                Text("Available")
                foodList(selected: false)
            }
            VStack {
                Text("Selected")
                foodList(selected: true)
            }
        }
    }
}
```

TODO: Why do the food names eventually disappear after being moved
in both Preview and the Simulator?

### Marching Ants Border

We can create an animated dashed border around any view
that resembles marching ants.
This was inspired by the Hacking With Swift post {% aTargetBlank
"https://www.hackingwithswift.com/example-code/calayer/how-to-create-a-marching-ants-effect-using-linedashphase",
"How to create a marching ants effect using lineDashPhase" %}.

The following code is the contents of the file `MarchingAnts.swift`
which can be added to any project.
It uses {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/preferencekey",
"PreferenceKey" %} which "automatically combines its values for
a given preference into a single value visible to its ancestors."
In this case the preference is the size of the content view.
The size is set in a `@State` property of
the custom `MarchingAnts` view modifier.

```swift
// MarchingAnts.swift
import SwiftUI

// This is a custom view modifier that gets the size of its content.
// TODO: Could you use sizeThatFits in place of this?
struct SizeReporter: ViewModifier {
    @Binding var size: CGSize

    private struct SizePreferenceKey: PreferenceKey {
        static let defaultValue: CGSize = .zero
        // inout allows passing by reference so a copy is not made.
        static func reduce(value: inout CGSize, nextValue: () -> CGSize) {
            value = nextValue()
        }
    }

    func body(content: Content) -> some View {
        content
            // This is a tricky approach to get the content size
            // using an invisible background.
            .background(GeometryReader { geometry in
                Color.clear
                    .preference(
                        key: SizePreferenceKey.self,
                        value: geometry.size
                    )
            })
            .onPreferenceChange(SizePreferenceKey.self) { value in
                size = value
            }
    }
}

// This is a custom view modifier that draws an animated dashed border
// around its content.
struct MarchingAnts: ViewModifier {
    @State private var phase: CGFloat = 0
    @State private var size: CGSize = .zero

    let clockwise: Bool
    let dashLength: CGFloat
    let dashWidth: CGFloat

    func body(content: Content) -> some View {
        ZStack {
            content.getSize($size)
            Rectangle()
                .strokeBorder(style: StrokeStyle(
                    lineWidth: dashWidth,
                    dash: [dashLength],
                    dashPhase: phase
                ))
                .onAppear {
                    withAnimation(
                        .linear
                            .repeatForever(autoreverses: false)
                    ) {
                        // Adding goes counter-clockwise
                        // and subtracting goes clockwise.
                        phase = dashLength * 2 * (clockwise ? -1 : 1)
                    }
                }
                // .frame(width: 250, height: 200)
                .frame(width: size.width, height: size.height)
        }
    }
}

// This extension simplifies using the custom view modifiers defined above.
extension View {
    func getSize(_ size: Binding<CGSize>) -> some View {
        modifier(SizeReporter(size: size))
    }

    func marchingAnts(
        clockwise: Bool = true,
        dashLength: CGFloat = 10,
        dashWidth: CGFloat = 3
    ) -> some View {
        modifier(MarchingAnts(
            clockwise: clockwise,
            dashLength: dashLength,
            dashWidth: dashWidth
        ))
    }
}
```

The following code demonstrates using the `marchingAnts` view modifier
defined above.

<img alt="SwiftUI Marching Ants" style="width: 40%"
  src="/blog/assets/SwiftUI-Marching-Ants.png?v={{pkg.version}}"
  title="SwiftUI Marching Ants">

```swift
// Content.swift
import SwiftUI

struct ContentView: View {
    var body: some View {
        VStack {
            Text("See them march!")
                .padding()
                .marchingAnts(
                    // clockwise: false,
                    // dashLength: 20,
                    // dashWidth: 10
                )
        }
    }
}
```
