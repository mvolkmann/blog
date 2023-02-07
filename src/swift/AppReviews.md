---
eleventyNavigation:
  key: App Reviews
  parent: Swift
layout: topic-layout.njk
---

## Overview

Users can submit app reviews that include 1-5 starts and text.
They can do this by finding the app in the App Store
and tapping "Write a Review".
In addition, an app can ask the user to provide a review.
This post describes how to implement such requests in an app.

Regardless of the number of times the an app requests a review,
the system will only prompt the user up to three times per year.

Apple provides these basic guidelines:

- Make the request at a time that does not interrupt
  a task the user is attempting to complete.
- Avoid making a request immediately after the app launches.
- Avoid making a request as a result of a user action.

Presumably this leaves making the request based on a timer.

## Resources

- Apple Developer page {% aTargetBlank
  "https://developer.apple.com/documentation/storekit/requesting_app_store_reviews",
  "Requesting App Store reviews" %}
- Stewart Lynch YouTube video {% aTargetBlank
  "https://www.youtube.com/watch?v=fLR7E2579Dg", "App Review Request iOS 16" %}
