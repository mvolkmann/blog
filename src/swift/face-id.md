---
eleventyNavigation:
  key: Face ID
  parent: Swift
  order: 3
layout: topic-layout.njk
---

## Overview

The `LocalAuthentication` package supports authentication in iOS apps
using either Face ID or Touch ID.

## Setup

1. Select the top entry in the Navigator.
1. Under "TARGETS", select the app target.
1. Click the "Info" tab.
1. Click the "+" button to the right of the last key in the table.
1. Enter the key "Privacy - Face ID Usage Description".
1. Enter a value that describes why Face ID is being used.

## Implementation

1. In the main view of the app add `import LocalAuthentication`
1. Add the following:

   ```swift
   @State private var isUnlocked = false

   func authenticate() {
       let context = LAContext()
       var error: NSError?

       if context.canEvaluatePolicy(
           .deviceOwnerAuthenticationWithBiometrics,
           error: &error
       ) {
           let reason = "We want to ensure that only you can see your data."
           context.evaluatePolicy(
               .deviceOwnerAuthenticationWithBiometrics,
               localizedReason: reason
           ) { success, error in
               if success {
                   isUnlocked = true
               } else {
                   // The user did not authenticate.
               }
           }
       } else {
           // The device does not support biometrics.
       }
   }
   ```

1. Use the value of `isUnlocked` to decide what to render.
1. Provide an alternate way for users to authenticate
   such as entering a username and password
   in case there are reasons they cannot currently use Face ID.

## Testing in Simulator

To test in the Simulator, open the Simulator app
and select Features ... Face ID ... Enrolled.

To simulate a successful Face ID scan,
select Features ... Face ID ... Matching Face.

To simulate an unsuccessful Face ID scan,
select Features ... Face ID ... Non-matching Face.
