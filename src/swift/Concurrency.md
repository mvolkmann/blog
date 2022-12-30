---
eleventyNavigation:
  key: Concurrency
  parent: Swift
layout: topic-layout.njk
---

## Overview

Apple added the async/await system to Swift in 2021.
This removes most needs to use low-level concurrency mechanisms
such as mutexes and semaphores.
It also removes the need to use completion handlers (aka callbacks)
and some uses of the delegate pattern.

## Issues

Common issues encountered when writing code involving concurrency include:

- Deadlocks

  This occurs when two processes are waiting each other to finish
  so neither can finish.
  This is typically solved by using mutexes of semaphores.

  Mutex is short for "MUTually EXclusive lock".
  Each mutex is owned by one process at a time.
  Processes that wish to access a resource that is protected by such a lock
  cannot do so until they acquire the lock.

  A semaphore is similar to mutex, but can protect access to a code path.

- Starvation

  A thread is starved when it waits forever to gain access to a resource.

- Race Condition

  A race condition occurs when multiple processes update a resource at the same time.
  This results in unpredictable results in the best case
  and data corruption in the worst case.

- Livelocks

  Livelocks occur when resources give up the locks they hold too frequently.
  This can result in processes taking too long to complete or never completing
  because they do not hold locks long enough to do so.

## Other Approaches

The following libraries and frameworks for managing concurrency
predate the async/await system.

### POSIX Threads (pthreads)

{% aTargetBlank "https://man7.org/linux/man-pages/man7/pthreads.7.html",
"pthreads" %} are not specific to Apple operating systems
and are available in many operating systems.
They are implemented in C and have a steep learning curve.
Using pthreads requires manual thread management and use locks
such as mutexes and semaphores.

### NSThreads

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/nsthread",
class is part of the Apple Foundation framework.
It provides another low-level approach to managing concurrency,
but is somewhat easier to use than pthreads.
This class can be accessed from Objective-C instead of C.

### Grand Central Dispatch (GCD)

The {% aTargetBlank "https://developer.apple.com/documentation/DISPATCH",
"Dispatch" %} framework (aka Grand Central Dispatch)
is specific to Apple operating systems.
It offers a higher level API than NSThreads and pthreads.

While GCD has many features, the most common use is to
run code that updates the UI on the main thread.
For example:

```swift
DispatchQueue.main.async {
    // UI updating code goes here.
}
```

### NSOperation APIs

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/nsoperation",
"NSOperation" %} class is part of the Apple Foundation framework.
To use this:

- Create an instance of `OperationQueue`.
- Create one instance of `BlockOperation` for each concurrent operation.
- Add each `BlockOperation` instance to the `OperationQueue`
  by passing it to the `addOperation` method of the `OperationQueue`.
- Wait for all the operations to complete by calling the
  `waitUntilAllOperationsAreFinished` method of the `OperationQueue`.

## async/await keywords

The `async` and `await` keywords free developers from
the low-level details of thread management.
They allow concurrent code to be written in a manner
similar to procedural programming,
resulting in code that is easier to write and read.

Unlike concurrent code that uses completion handlers (aka callbacks),
using `async` and `await` does not result in deeply nested code.

To use this approach, add the `async` keyword
after the parameter list and before the return type
of all functions that run asynchronously.
Inside the function, add the `await` keyword
before all calls to other asynchronous functions.
For example:

```swift
function getCurrentCity() async throws -> String {
    let coordinates = await getCurrentCoordinates() // implemented elsewhere
    let address = await getAddress(of: coordinates) // implemented elsewhere
    return address.city
}
```

All `async` functions must be called from a concurrent context.
All `async` functions run in a concurrent context,
so they can call other `async` functions.
Another way to create a concurrent context is by creating a {% aTargetBlank
"https://developer.apple.com/documentation/swift/task", "Task" %} object.
For example:

```swift
import SwiftUI

struct ContentView: View {
    @State private var city = ""
    @State private var isShowingError = false
    @State private var message = ""

    var body: some View {
        VStack {
            Button("Get City") {
                Task {
                    do {
                        let currentCity = try await getCurrentCity()
                        // This updates the UI on the main thread
                        // and will be explained more later.
                        await MainActor.run { city = currentCity }
                    } catch {
                        message = "Error getting city: \(error)"
                        isShowingError = true
                    }
                }
            }
            Text("City: \(city)")
        }
        .alert(
            "Error",
            isPresented: $isShowingAlert,
            actions: {},
            message: { Text(message) }
        )
    }
}
```

## Continuations

- `withCheckedContinuation`
- `withCheckedThrowingContinuation`
- `withUnsafeContinuation`
- `withUnsafeThrowingContinuation`

## Structured Concurrency

### async let

### Task Groups

## Unstructured Concurrency

### Task

### Task Tree

## Actors

## Sendable Types

## Main Actor

## AsyncSequence
