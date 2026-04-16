---
eleventyNavigation:
  key: Combine
  parent: Swift
layout: topic-layout.njk
---

## Overview

[Combine](<https://developer.apple.com/documentation/combine?v=1.1.1>) is a framework for processing values over time.

Combine defines the protocols [Publisher](<https://developer.apple.com/documentation/combine/publisher?v=1.1.1>)
and [Subscriber](<https://developer.apple.com/documentation/combine/subscriber?v=1.1.1>).
Publishers deliver elements to subscribers and
subscribers receive a stream of elements from a publisher.

TODO: See the free, online book [Using Combine](<https://heckj.github.io/swiftui-notes/?v=1.1.1>).

## Publishers

### map method

This method takes a closure that receives elements from a `Publisher`
and transforms them in some way.
It returns a new `Publisher` that publishes the transformed values.

### tryMap method

This method is similar to the `map` method,
but the closure is allowed to throw an error.
If it does, publishing will stop.

There are many more `try` versions of `Publisher` methods. These include
`tryAllSatisify`, `tryCompactMap`, `tryContains`, `tryDrop`, `tryFilter`,
`tryFirst`, `tryLast`, `tryMax`, `tryMin`, `tryPrefix`, `tryReduce`,
`tryRemoveDuplicates`, and `tryScan` methods.
