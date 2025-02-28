---
eleventyNavigation:
  key: SF Symbols
  parent: Swift
layout: topic-layout.njk
---

## Overview

SF Symbols is a macOS app from Apple that provides over 5,000 icons.
These can be rendered in custom app using the `Image` view
with the `systemName` argument.

## Rendering Modes

Some symbols support multiple symbol rendering modes
that enable using different colors for parts of the icon.
To see the available rendering modes for a given icon,
select the icon and click the paint brush tab in the Inspector on the right.
Each icon has a preferred rendering mode,
but a different rendering mode can be selected by applying the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/image/renderingmode(_:)",
"renderingMode" %} view modifier.

To specify a symbol rendering mode, apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/symbolrenderingmode(_:)",
"symbolRenderingMode" %}. The supported symbol rendering modes include:

- `.monochrome` - single color; seems the same as hierarchical
- `.hierarchical` - single color with multiple opacity levels
- `.palette` - two or three colors
- `.multicolor` - seems the same as hierarchical

When the symbol rendering mode is not specified,
the preferred rendering mode of the icon of is used.

## Variable Colors

Icons that support variable colors are grouped in the "Variable" category.
These display additional parts of the icon as a percentage value increases.
To see this in action:

- Select one of the icons in the "Variable category.
- Select the paint brush tab in the Inspector on the right.
- Select any rendering mode.
- Activate variable color mode by clicking the
  button to the left of the percentage slider.
- Drag the slider to change the percentage value.
- Note that all the displayed icons update to show the effect.

Here's an example of using such an icon:

```swift
// The @State property wrapper is described later.
// It allows a view instance to maintain state inside the property wrapper.
@State private var percent = 0.0
...
Image(systemName: "cellularbars", variableValue: percent)
Button("Decrease") {
    if percent > 0 { percent -= 0.1 }
}
Button("Increase") {
    if percent < 1 { percent += 0.1 }
}
```

See the {% aTargetBlank
"https://github.com/mvolkmann/SFSymbolsDemo/tree/main", "SFSymbolsDemo" %}
app in GitHub.

## Template Mode

To ignore the colors in a multicolor SF Symbol icon
and just use it as a template for applying a single specified color,
apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/image/renderingmode(_:)",
"renderingMode" %} view modifier with the argument value `.template`.
For example:

```swift
Image(systemName: "doc.fill.badge.plus")
    .renderingMode(.template)
    .resizable()
    .foregroundColor(.pink)
    .scaledToFit()
    .frame(width: 100, height: 100)
```

## Animation

iOS 17 added the ability to animate SF Symbols.
See the folloing WWDC 2023 sessions:

- {% aTargetBlank "https://developer.apple.com/wwdc23/10197",
  "What's new in SF Symbols 5" %}
- {% aTargetBlank "https://developer.apple.com/wwdc23/10258",
  "Animate symbols in your app" %}

SF Symbols 5 added an Animation Inspector, shown in the screenshot below.
This provides a way to see all the animation options available
for a given SF Symbol and preview them without writing any code.
To copy Swift code for a specified animation, click the clipboard icon
and select "Copy Configuration for Swift".

<img alt="SF Symbols Animation Inspector" style="width: 100%"
  src="/blog/assets/SFSymbols-animation-inspector.png?v={{pkg.version}}"
  title="SF Symbols Animation Inspector">

There are four animation behaviors:

- **Discrete** behaviors run for some duration and
  returns the symbol to its initial state on completion.
  The `bounce` effect has this behavior.
  Use the `value` argument to specify a variable where
  any change to its value should trigger the effect.

- **Indefinite** behaviors run indefinitely and
  do not return the symbol to its initial state
  on completion. The `scale` effect has this behavior.
  Use the `isActive` argument to specify a boolean variable
  that indicates whether the effect should be active.

- **Transition** behaviors transition the symbol in or out of view.
  The `appear` and `disappear` effects have this behavior.

- **Content Transition** behaviors animate from one symbol to another.
  The `replace` effect has this behavior

SF Symbols are composed of layers can be individually animated.
By default the layers animate one at a time,
but they can be configured to animate at the same time.

Many SF Symbols define multiple layers.
Animation presets can be applied to all or specific layers of any SF Symbol.
The presets include:

- **Appear** effect fades in a symbol when added to UI.

  This effect can have indefinite or transition behavior.

  This effect is similar to the Disappear effect.
  See examples below.

- **Disappear** effect fades out a symbol when removed from UI.

  This effect can have indefinite or transition behavior.

  For example:

  ```swift
  @State private var showHeart = false
  ...
  VStack {
      Button(showHeart ? "Hide Heart" : "Show Heart") {
          showHeart.toggle()
      }

      HStack {
          Image(systemName: "chevron.left")
          // This approach uses an indefinite behavior to maintain the
          // space occupied by the symbol even when it no longer appears.
          Image(systemName: "heart")
              .symbolEffect(.disappear, isActive: !showHeart)
          Image(systemName: "chevron.right")
      }
      .imageScale(.large)

      HStack {
          Image(systemName: "chevron.left")
          // This approach uses a transition behavior to closes the
          // space occupied by the symbol when it no longer appears.
          if showHeart {
              Image(systemName: "heart")
                  .transition(.symbolEffect(.disappear))
          }
          Image(systemName: "chevron.right")
      }
      .imageScale(.large)
  }
  ```

- **Bounce** effect quickly scales a symbol up and down.

  This effect has discrete behavior.

  By default the symbol bounces once.
  Use the `option` argument to specify the number of times it should bounce.

  For example:

  ```swift
  Image(systemName: "wifi")
      .symbolEffect(
          .bounce.up, // gets larger, then returns to original size
          // .bounce.down, // gets smaller, then returns to original size
          options: .repeat(3).speed(2),
          value: isActive
      )
  ```

- **Pulse** effect cycles a symbol through a range of opacities,
  gradually making it more transparent and then returning to full opacity.

  This effect can have discrete or indefinite behavior.
  With indefinite behavior, the effect continues until it is removed.

  For some symbols, only specific layers are pulsed.
  In "rectangle.inset.filled.and.person.filled"
  which displays a computer monitor and a person,
  only the screen is pulsed.

  For example:

  ```swift
  Image(systemName: "rectangle.inset.filled.and.person.filled")
      .resizable()
      .scaledToFit()
      .frame(width: size, height: size)
      .foregroundStyle(.tint)
      .symbolEffect(
          .pulse,
          options: .speed(2),
          isActive: isActive
      )
  ```

- **Replace** effect replaces one symbol with another.

  This effect has content transition behavior.

  This effect can happen either one layer at a time
  or replacing all layers at once (whole symbol).
  A good example is switching between the play and pause icons.

  For example:

  ```swift
  @State private var isActive = false
  ...
  Button {
      isActive.toggle()
  } label: {
      Image(systemName: isActive ? "play.circle" : "pause.circle")
          .resizable()
          .scaledToFit()
          .frame(width: 50, height: 50)
          .contentTransition(
              .symbolEffect(.replace.downUp.byLayer)
          )
  }
  ```

- **Scale** effect scales a symbol up or down until the effect is removed.

  This effect has indefinite behavior.

  For example:

  ```swift
  @State private var liked = false
  ...
  Button {
      liked.toggle()
  } label: {
      Image(systemName: "heart")
          .resizable()
          .scaledToFit()
          .frame(width: 50, height: 50)
          .symbolEffect(.scale.up.byLayer, isActive: liked)
  }
  ```

- **Variable Color** effect causes a cumulative or iterative color change.

  This effect can have discrete or indefinite behavior.

  Cumulative gradually applies color to each layer, retaining the changes.
  Iterative applies color to each layer one at a time,
  but not retaining the changes.
  Changes can also be reversed after each layer has changed.

To add an effect to an SF Symbol, apply the `symbolEffect` view modifier,
passing it an `enum` value that specifies the effect.
Effects can be configured by chaining values.
For example, `.symbolEffect(.variableColor.iterative.reversing)`.

To combine effects, apply the `symbolEffect` view modifier multiple times.
For example:

```swift
Image(systemName: "wifi.router")
    .symbolEffect(.variableColor.iterative.reversing)`.
    .symbolEffect(.scale.up)`.
```

To cause an effect to stop when a boolean value becomes `false`,
add the `isActive` argument.
For example:

```swift
@State private var isActive = false
let size = 100.0

Image(systemName: "ifi")
    // .imageScale(.large)
    .resizable()
    .scaledToFit()
    .frame(width: size, height: size)
    .foregroundStyle(.tint)
    .symbolEffect(
        .pulse,
        options: .speed(2),
        isActive: isActive
     )
```

The `symbolEffect` view modifier can be applied to a parent view such as
an `HStack` or `VStack` in order to apply it to all SF Symbols inside.
To remove the effect from a specific `Image` view inside the parent view,
apply the `symbolEffectsRemoved` view modifier.
