---
eleventyNavigation:
  key: Combine
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/combine",
"Combine" %} is a framework for processing values over time.

Combine defines the protocols {% aTargetBlank
"https://developer.apple.com/documentation/combine/publisher", "Publisher" %}
and {% aTargetBlank
"https://developer.apple.com/documentation/combine/subscriber", "Subscriber" %}.
Publishers deliver elements to subscribers and
subscribers receive a stream of elements from a publisher.

TODO: See the free, online book {% aTargetBlank
"https://heckj.github.io/swiftui-notes/", "Using Combine" %}.

## Publishers

### map method

This method takes a closure that receives elements from a `Publisher`
and transforms them in some way.
It returns a new `Publisher` that publishes the transformed values.

### tryMap method

This method is similar to the `map` method,
but the closure is allowed to throw an error.
If it does, publishing will stop.
