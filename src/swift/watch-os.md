---
eleventyNavigation:
  key: watchOS
  parent: Swift
  order: 3
layout: topic-layout.njk
---

## Overview

This post explains the steps for creating an Apple watchOS app
that runs on an Apple Watch.

## Project Creation

- Open XCode.
- Select File ... New ... Project...
- Select watchOS.
- Select "Watch App".
- Click the Next button.
- Enter a product name.
- Select your team.
- Enter your organization identifier.
- Uncheck "Include Notification Scene".
- Keep the other default values.
- Click the Next button.
- Select the directory where the project will be stored.
- Click the Create button.
- Click the triangle near the upper-left to run the app in the Watch Simulator.
- Verify that it just displays "Hello, World!".

## Example Code

The following code implements a countdown timer app.

<img alt="WatchOS Timer App #1" style="width: 30%"
  src="/blog/assets/watchOS-timer-app1.png?v={{pkg.version}}"
  title="WatchOS Timer App #1">
<img alt="WatchOS Timer App #2" style="width: 30%"
  src="/blog/assets/watchOS-timer-app2.png?v={{pkg.version}}"
  title="WatchOS Timer App #2">
<img alt="WatchOS Timer App #3" style="width: 30%"
  src="/blog/assets/watchOS-timer-app3.png?v={{pkg.version}}"
  title="WatchOS Timer App #3">

```swift
import SwiftUI

struct ContentView: View {
    @State var isTimerPresented = false
    @State var timerValue = 5

    var body: some View {
        VStack {
            Text("Timer").font(.system(size: 40))
            Picker(selection: $timerValue, label: Text("")) {
                Text("5").tag(5)
                Text("10").tag(10)
                Text("15").tag(15)
                Text("30").tag(30)
            }
            NavigationLink(
                destination: TimerView(
                    isPresented: $isTimerPresented,
                    timerValue: timerValue
                ),
                isActive: $isTimerPresented,
                label: { Text("Start") }
            )
                .foregroundColor(.green)
        }
    }
}

struct TimerView: View {
    @Binding var isPresented: Bool
    @State var timerValue: Int

    @State var timer: Timer?

    func startTimer() {
        timer = Timer.scheduledTimer(withTimeInterval: 1, repeats: true) {
            timer in
            if timerValue > 0 {
                timerValue -= 1
            } else {
                timer.invalidate()
            }
        }
    }

    var body: some View {
        VStack {
            if timerValue > 0 {
                Text("Time remaining")
                    .onAppear() { startTimer() }
                Text("\(timerValue)").font(.system(size: 40))
                Text("seconds")
                Button("Cancel") { isPresented = false }
                    .foregroundColor(.red)
            } else {
                Button("Done") { isPresented = false }
            }
        }
    }
}
```
