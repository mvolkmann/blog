---
eleventyNavigation:
  key: Concurrency
  parent: Swift
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

## Overview

Apple added the async/await system to Swift in 2021.
This removes most needs to use lower level concurrency mechanisms
and Grand Central Dispatch (GCD).
It also removes the need to use completion handlers (aka callbacks)
and some uses of the delegate pattern.

## Resources

- Apple documentation on {% aTargetBlank
  "https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html",
  "Concurrency" %}.

- book {% aTargetBlank "https://swiftasyncbook.com",
  "Modern Concurrency on Apple Platforms" %} by Andrés Ibañez Kautsch

- iOS Conf SG 2022 talk from Daniel H. Steinberg,
  {% aTargetBlank "https://www.youtube.com/watch?v=FEqmYi-FaB8",
  "Willing Suspension of Disbelief" %}.

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
  The Dispatch framework provides the {% aTargetBlank
  "https://developer.apple.com/documentation/dispatch/dispatchsemaphore",
  "DispatchSemaphore" %} class.
  It does not provide a mutex implementation.

- Starvation

  A thread is starved when it waits forever to gain access to a resource.

- Race Condition

  A race condition occurs when multiple processes update a resource at the same time.
  This can produce unpredictable results in the best case
  and data corruption in the worst case.

- Livelocks

  Livelocks occur when resources give up the locks they hold too frequently.
  This can result in processes taking too long to complete or never completing
  because they do not hold locks long enough to do so.

## Other Approaches

The following libraries and frameworks for managing concurrency
predate the async/await system. They are now rarely needed,
but can be useful in very specific circumstances.

### POSIX Threads (pthreads)

{% aTargetBlank "https://man7.org/linux/man-pages/man7/pthreads.7.html",
"pthreads" %} are not specific to Apple operating systems
and are available in many operating systems.
They are implemented in C and have a steep learning curve.
Using pthreads requires manual thread management and
use of locks such as mutexes and semaphores.

### NSThreads

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/nsthread",
"NSThread" %} class is part of the Apple Foundation framework.
It provides another low-level approach to managing concurrency,
but is somewhat easier to use than pthreads.
This class can be accessed from Objective-C instead of C.

### Grand Central Dispatch (GCD)

The {% aTargetBlank "https://developer.apple.com/documentation/DISPATCH",
"Dispatch" %} framework (aka Grand Central Dispatch)
provides a higher level API than NSThreads and pthreads.
It was originally specific to Apple operating systems,
but since Swift has been open sourced it is now cross-platform.

While GCD has many features, the most common use is to
run code that updates the UI on the main thread.
For example:

```swift
DispatchQueue.main.async {
    // Code to update the UI goes here.
}
```

### NSOperation APIs

The {% aTargetBlank
"https://developer.apple.com/documentation/foundation/nsoperation",
"NSOperation" %} class is part of the Apple Foundation framework.
To use this:

- Create an instance of the {% aTargetBlank
  "https://developer.apple.com/documentation/foundation/nsoperationqueue",
  "NSOperationQueue" %} class.
- Create one instance of {% aTargetBlank
  "https://developer.apple.com/documentation/foundation/nsblockoperation",
  "NSBlockOperation" %} for each concurrent operation.
- Add each `NSBlockOperation` instance to the `NSOperationQueue`
  by passing it to the `addOperation` method of the `NSOperationQueue`.
- Wait for all the operations to complete by calling the
  `waitUntilAllOperationsAreFinished` method of the `NSOperationQueue`.

## Tasks, Queues, and Threads

Tasks (also referred to as "work items")
represent a body of work to be performed.
To schedule the work, a `Task` is added to a queue
in either a blocking fashion (runs synchronously)
or a non-blocking fashion (runs asynchronously).

A queue is responsible for determining when its tasks will run
and the threads on which they will run.
Each queue is either "serial" or "concurrent".
Both kinds execute their tasks in the order in which they were added.

Serial queues execute one task at a time
and each task can run on a different thread.
One use of a serial queue is to synchronize access to shared resources.

Concurrent queues can execute multiple tasks at the same time
which requires multiple threads.
The number of tasks executed simultaneously at any point in time
can vary based on conditions in the application
and the number of CPU cores in the device.
Tasks run on concurrent queues can finish in a different order
than they were started since each task can have a different duration.

Each queue collects tasks to be run at
a given priority or quality of service (QoS).
Applications can create any number of queues.
There are five "global queues" that are typically used
instead of custom queues.

While it is possible to write code that
explicitly runs a task on a specific thread,
doing so is discouraged because it removes
the ability of the system to manage thread usage.
When using queues, the system can optimize the use of threads
based of the number of CPU cores in the current device.
The system can decide the number of threads to use
and when each task should run.

The user interface should only be updated on the main thread.
This is achieved by adding such tasks to the main queue.
which only runs tasks on the main thread.
If the UI is updated on a thread other than the main thread,
it may have no effect or the application may crash.
TODO: Does it sometimes work?
The Swift compiler provides warnings when it detects
code that attempts to update the UI outside of the main thread.

There are five provided global queues that correspond to the six QoS levels,
listed here in order from highest to lowest priority.

- `userInteractive`

  This is the same as the main queue which is the only provided serial queue.
  All other provided queues are concurrent.

- `userInitiated`

  This is the same as the concurrent high priority queue.

- `default`

  This is a concurrent queue.

- `utility` and `unspecified`

  These are the same as the concurrent low priority queue.

- `background`

  This is a concurrent queue.

Additional queues using any of the QoS values can be created,
but typically only the provided queues are used.

To obtain a reference to a global queue for a given QoS
using one of the enum cases at {% aTargetBlank
"https://developer.apple.com/documentation/dispatch/dispatchqos/qosclass",
"DispatchQoS.QoSClass" %}:

```swift
// If qos is omitted, it defaults to `.default`.
let queue = DispatchQueue.global(qos)
```

To create a new queue:

```swift
let mySerialQueue = DispatchQueue(label: "my-queue-name")

let myConcurrentQueue =
    DispatchQueue(label: "my-queue-name", attributes: .concurrent)
```

To submit a task to a queue to run synchronously,
which blocks the caller until the task completes,
pass a closure to the `sync` method of the queue.

To submit a task to a queue to run asynchronously,
which does not wait for the task to complete and does not blocks the caller,
pass a closure to the `async` method of the queue.

## async/await keywords

The `async` and `await` keywords free developers from
the low-level details of thread management.
They allow concurrent code to be written in a manner
similar to procedural programming,
resulting in code that is easier to write and read.

Unlike concurrent code that uses completion handlers (aka callbacks),
using `async` and `await` does not result in deeply nested code.

Add the `async` keyword after the parameter list and before the return type
of all functions that run asynchronously.
Inside the function, add the `await` keyword
before all calls to other asynchronous functions.
For example:

```swift
func getCurrentCity() async throws -> String {
    let coordinates = await getCurrentCoordinates() // implemented elsewhere
    let address = await getAddress(of: coordinates) // implemented elsewhere
    return address.city
}
```

All `async` functions:

- must be called from a "concurrent context" (aka asynchronous context)
- run in a concurrent context, so they can call other `async` functions

The `await` keyword can choose to suspend execution of the current function.
This allows the thread executing the function to perform other work.
When the `await` keyword is applied to a function call,
it creates a "suspension point".
The code after the call is referred to as the "continuation".
When the function is resumed later, the continuation is executed,
possibly in a different thread than the one in which function execution began.

In iOS 15 and above, all Apple provided functions that
take a completion handler also have an `async` version.
This means there is no longer a need to
pass completion handlers to Apple's APIs.
For example, {% aTargetBlank
"https://developer.apple.com/documentation/foundation/urlsession",
"URLSession" %} has many async methods.
The resulting code is easier to write and read.

One way to create a concurrent context is it create a {% aTargetBlank
"https://developer.apple.com/documentation/swift/task", "Task" %} object.

The following sample app gets random jokes from a free, public API.
It uses the `async` and `await` keywords.
It also creates tasks in two ways,
using the `Task` initializer and the `task` view modifier.

```swift
import SwiftUI

struct ContentView: View {
    enum MyError: Error {
        case badResponseType, badStatus, noData
    }

    struct Joke: Decodable {
        let setup: String
        let punchline: String
    }

    @State private var isShowingError = false
    @State private var joke: Joke?
    @State private var message = ""

    private let apiURL =
        URL(string: "https://official-joke-api.appspot.com/random_joke")!

    private func getJoke() async -> Joke? {
        message = ""
        do {
            // The data method returns a tuple.
            // The type of response is URLResponse.
            // Cast it to HTTPURLResponse to get information from it.
            let (data, response) =
                try await URLSession.shared.data(from: apiURL)
            guard let response = response as? HTTPURLResponse else {
                throw MyError.badResponseType
            }
            guard response.statusCode == 200 else {
                throw MyError.badStatus
            }
            let joke = try JSONDecoder().decode(
                Joke.self,
                from: data
            )
            return joke
        } catch {
            message = error.localizedDescription
            isShowingError = true
            return nil
        }
    }

    var body: some View {
        VStack {
            Text("Jokester").font(.largeTitle)
            Spacer()
            if let joke {
                Text(joke.setup).font(.title).foregroundColor(.green)
                Text(joke.punchline).font(.title).foregroundColor(.red)
                    .padding(.top)
                Spacer()
                Button("Next") {
                    Task { self.joke = await getJoke() }
                }
                .buttonStyle(.borderedProminent)
            }
        }
        .padding()
        // This is a view modifier that is similar to onAppear,
        // but runs the closure passed to it in a `Task`
        // which provides an concurrent context.
        // If the view is removed, the `Task` is cancelled.
        .task {
            joke = await getJoke()
        }
        .alert(
            "Error",
            isPresented: $isShowingError,
            actions: {},
            message: { Text(message) }
        )
    }
}
```

Another free, public API that can be used in the code example above
provides a suggested activity.
To use this API:

- change the API URL to `https://www.boredapi.com/api/activity`
- replace the `Joke` struct with the `Activity` struct below
- replace references to `Joke` with `Activity`
- replace references to `joke` with `activity`
- display the value of `activity.activity`
  in place of `joke.setup` and `joke.punchline`.

```swift
struct Activity: Decodable {
    let activity: String
    let type: String
    let participants: Int
    let price: Double
    let link: String
    let key: String
    let accessibility: Double
}
```

## Continuations

Functions that take a completion handler (aka callback)
can be wrapped in a new `async` function that does not take a callback
in order to simplify their use.

In iOS 15 and above, all Apple provided functions that
take a completion handler also have an `async` version.
So it is not necessary to wrap the versions that take a completion handler.
However, functions in non-Apple frameworks may still use completion handlers
and these benefit from wrapping.

Suppose Apple had not provided an `async` method in `URLSession`
for retrieving data from a `URL`.
We could modify the code in the previous example as shown below.
While the `fetchActivityWithContinuation` function below
is somewhat complicated,
calling it does not require passing a completion handler.
This simplifies the code in callers.

```swift
    private func getJoke() async -> Joke? {
        message = ""
        do {
            let joke = try await fetchActivityWithContinuation()
            return joke
        } catch {
            message = error.localizedDescription
            return nil
        }
    }

    private func fetchActivityWithContinuation() async throws -> Joke {
        // The closure passed to `withCheckedThrowingContinuation`
        // is passed a `CheckedContinuation` object.
        // This has a `resume` method that must be called
        // when data is available or when an error occurs.
        try await withCheckedThrowingContinuation { completion in
            let task = URLSession.shared.dataTask(
                with: apiURL
            ) { data, response, _ in
                // The type of response is URLResponse.
                // Cast it to HTTPURLResponse to get information from it.
                guard let response = response as? HTTPURLResponse else {
                    completion.resume(throwing: MyError.badResponseType)
                    return
                }
                guard response.statusCode == 200 else {
                    completion.resume(throwing: MyError.badStatus)
                    return
                }
                guard let data = data else {
                    completion.resume(throwing: MyError.noData)
                    return
                }
                do {
                    let joke = try JSONDecoder().decode(Joke.self, from: data)
                    completion.resume(returning: joke)
                } catch {
                    completion.resume(throwing: error)
                }
            }

            task.resume()
        }
    }
```

The example above demonstrates using the {% aTargetBlank
"https://developer.apple.com/documentation/swift/withcheckedthrowingcontinuation(function:_:)",
"withCheckedThrowingContinuation" %} function.
If `completion.resume` is never called, the runtime error
"SWIFT TASK CONTINUATION MISUSE: ... leaked its continuation!"
will be triggered.
If `completion.resume` is called more than once, the runtime error
"SWIFT TASK CONTINUATION MISUSE: ... tried to resume its continuation
more than once" will be triggered.

There are three other similar functions that can be used.

- {% aTargetBlank "https://developer.apple.com/documentation/swift/withcheckedcontinuation(function:_:)", "withCheckedContinuation" %}
  is like `withCheckedThrowingContinuation`, but does not throw and
  the compiler will not allow calling `completion.resume(throwing: someError)`.

- {% aTargetBlank "https://developer.apple.com/documentation/swift/withunsafecontinuation(_:)", "withUnsafeContinuation" %}
  is like `withCheckedContinuation` but does not perform runtime checks
  to ensure that `completion.resume` is called exactly once.
  This makes it slightly faster.

- {% aTargetBlank "https://developer.apple.com/documentation/swift/withunsafethrowingcontinuation(_:)", "withUnsafeThrowingContinuation" %}
  is like `withCheckedThrowingContinuation` but does not perform runtime checks
  to ensure that `completion.resume` is called exactly once.
  This makes it slightly faster.

Use of the unsafe versions is generally not recommended.

## Structured Concurrency

Structured concurrency provides a way to
execute multiple tasks at the same time AND
write code in the order in which it is expected to run.
The async/await system provides two ways to do this,
`async let` and task groups.

### async let

An `async let` statement is a special variable declaration
whose value is computed asynchronously in a new, implicit child task.
These statements must be used inside a concurrent context
(either a closure passed to `Task` or an `async` function).

The work to compute the value of each `async let` variable
begins immediately and may occur in different threads.
The threads used are determined by the operating system
and cannot be dictated in code.

The `await` keyword must be used to get the values of these variables.
A single `await` can be used to wait for multiple values to be computed.
For example:

```swift
    @State private var activity: Activity?
    @State private var dogImage: DogImage?
    ...
    // This could use the API at https://www.boredapi.com/api/activity.
    private func getActivity() async throws -> Activity {
        ...
    }
    // This could use the API at https://dog.ceo/api/breeds/image/random.
    private func getDogImage() async throws -> DogImage {
        ...
    }
    ...
    Task {
        do {
            async let a = getActivity()

            // getDogImage can begin executing before getActivity completes.
            async let d = getDogImage()

            // This waits for both getActivity and getDogImage to complete.
            (activity, dogImage) = try await (a, d)
        } catch {
            message = error.localizedDescription
        }
    }
```

### Task Groups

Task groups enable computing a variable number of values concurrently.

In the previous example we only needed to compute two values concurrently,
an activity and a dog image.

Suppose we wanted to fetch a random number of dog images.
We can do one of the following:

- If the tasks cannot throw, call {% aTargetBlank
  "https://developer.apple.com/documentation/swift/withtaskgroup(of:returning:body:)",
  "withTaskGroup" %} which creates a {% aTargetBlank
  "https://developer.apple.com/documentation/swift/taskgroup", "TaskGroup" %}.
- If the tasks can throw, call {% aTargetBlank
  "https://developer.apple.com/documentation/swift/withthrowingtaskgroup(of:returning:body:)",
  "withThrowingTaskGroup" %} which creates a {% aTargetBlank
  "https://developer.apple.com/documentation/swift/throwingtaskgroup",
  "ThrowingTaskGroup" %}.

Each these take a closure that is passed the created group.
Inside the closure, call the `addTask` method of the group
once for each value to be computed.

The system will decided how many of the tasks to run concurrently.
Excess tasks will wait for running tasks to complete before they begin.

Tasks are not guaranteed to run in the order
in which they were added to the group.

Both the `TaskGroup` and `ThrowingTaskGroup` structs conform to the
{% aTargetBlank "https://developer.apple.com/documentation/swift/asyncsequence",
"AsyncSequence" %} protocol described later.
This means that the values of the tasks added to the group
can be obtained using a `for await` loop when the tasks cannot throw
or a `for try await` loop when the tasks can throw.

The following code demonstrates the downloading
a random number (1 to 5) of dog image URLs.

```swift
    private func getDogImages() async throws -> [DogImage] {
        var dogImages: [DogImage] = []
        let count = Int.random(in: 1 ... 5)

        try await withThrowingTaskGroup(of: DogImage.self) { group in
            // Add tasks to the group.
            for _ in 0 ..< count {
                // Each task *can* begin executing
                // as soon is it is added to the group.
                // .userInitiated is the highest priority.
                // If the priority argument is omitted,
                // it uses the `default` global queue.
                group.addTask(priority: .userInitiated) {
                    let dogImage = try await getDogImage()
                    return dogImage
                }
            }

            // Wait for each task in the group to finish.
            // Results are delivered to this for loop
            // in the order in which their task finished,
            // not in the order in which the tasks were added to the group.
            for try await dogImage in group {
                dogImages.append(dogImage)
            }
        }

        // Return the results of all the tasks.
        return dogImages
    }
```

## Unstructured Concurrency

Unstructured concurrency, like structured concurrency,
provides a way to execute multiple tasks at the same time.
However, it does not emphasize writing code
in the order in which it is expected to run.

### Task

Unstructured concurrency relies on creating {% aTargetBlank
"https://developer.apple.com/documentation/swift/task", "Task" %} objects
which are passed a closure that runs in a concurrent context.
The system typically runs the `Task` immediately, but can choose to
defer execution based on the number of tasks that are already running.

`Task` is a generic struct. When creating an instance,
specify the `Success` type which is the type of the value it can hold
and the `Error` type which is the type of error it can hold.
If a `Task` never throws, specify `Never` for the `Error` type.

The `Task` initializer can be passed the priority under which it should run.
When no priority is specified, the priority of the parent `Task` is used.
The available priorities from highest to lowest are:

- `.high` or `.userInitiated` (25)
- `.medium` (21)
- `.low` or `.utility` (17)
- `.background` (9)

To run asynchronous functions sequentially:

```swift
Task {
    await asyncFunc1()
    await asyncFunc2()
}
```

To run asynchronous functions concurrently:

```swift
Task {
    await asyncFunc1()
}
Task {
    await asyncFunc2()
}
```

Another way to create a `Task` is to apply the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/task(priority:_:)",
"task" %} view modifier which takes
an optional priority (defaults to `userInitiated`)
and a closure to execute inside a new `Task`.
This is similar to the {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/onappear(perform:)",
"onAppear" %} view modifier in that the closure passed to it
is executed before the view on which is applied appears.

A second {% aTargetBlank
"https://developer.apple.com/documentation/swiftui/view/task(id:priority:_:)",
"task" %} view modifier also takes an `id` argument
which is a value of any type that conforms to the `Equatable` protocol.
Like the other `task` view modifier, the closure passed to it
is executed before the view on which is applied appears.
Every time the value of the `id` argument changes,
the `Task` is cancelled (if still running)
and the closure is executed again in a new `Task`.
This can be useful in scenarios where the `id` represents a
query to be performed which provides new data to be displayed.

The following code demonstrates using the `task` view modifier
with the `id` argument:

<img alt="SwiftUI task view modifier" style="width: 50%"
  src="/blog/assets/SwiftUI-task-with-id.png?v={{pkg.version}}"
  title="SwiftUI task view modifier">

```swift
import SwiftUI

struct ContentView: View {
    enum MyError: Error {
        case badResponseType, badStatus
    }

    // This struct is needed to decode JSON from an API service.
    struct Place: Decodable {
        let latitude: String
        let longitude: String
        var placeName: String
        let state: String

        // We need this because the API returns JSON
        // with a key of "place name" which is not a valid
        // Swift property name due to containing a space.
        private enum CodingKeys: String, CodingKey {
            case latitude
            case longitude
            case placeName = "place name"
            case state
        }
    }

    // This struct is needed to decode JSON from an API service.
    struct ZIPInfo: Decodable {
        let country: String
        let places: [Place]
    }

    @State private var errorMessage = ""
    @State private var isShowingError = false
    @State private var message = ""
    @State private var zipCode = ""
    // This will hold data from an API service.
    @State private var zipInfo: ZIPInfo?

    // The API at this URL returns data about a given U.S. ZIP code.
    private var apiURL: URL {
        URL(string: "https://api.zippopotam.us/us/\(zipCode)")!
    }

    private func getInfo() async {
        errorMessage = ""
        message = ""
        zipInfo = nil

        // All U.S. ZIP codes consist of 5 digits.
        guard zipCode.count == 5 else { return }

        do {
            // The data method returns a tuple.
            // The type of response is URLResponse.
            // Cast it to HTTPURLResponse to get information from it.
            let (data, response) =
                try await URLSession.shared.data(from: apiURL)
            guard let response = response as? HTTPURLResponse else {
                throw MyError.badResponseType
            }
            if response.statusCode == 404 {
                message = "No match was found."
                return
            }
            guard response.statusCode == 200 else {
                throw MyError.badStatus
            }
            zipInfo = try JSONDecoder().decode(ZIPInfo.self, from: data)
        } catch {
            // If the zip code is changes while an API call
            // is running, the task will be cancelled.
            // We don't want to report that as an error.
            if error.localizedDescription != "cancelled" {
                errorMessage = error.localizedDescription
                isShowingError = true
            }
        }
    }

    var body: some View {
        VStack {
            Text("U.S. ZIP Code Information").font(.title)

            TextField("ZIP Code", text: $zipCode)
                .keyboardType(.numberPad)
                .textFieldStyle(.roundedBorder)
                .frame(width: 85)
                .onChange(of: zipCode) { _ in
                    if zipCode.count > 5 {
                        zipCode = String(zipCode.prefix(5))
                    }
                }

            if message.isEmpty {
                if let zipInfo, let place = zipInfo.places.first {
                    Text("\(place.placeName), \(place.state)").font(.title3)
                }
            } else {
                Text(message)
            }

            Spacer()
        }
        .padding()
        .task(id: zipCode) {
            await getInfo() // uses zipCode
        }
        .alert(
            "Error",
            isPresented: $isShowingError,
            actions: {},
            message: { Text(errorMessage) }
        )
    }
}
```

When a `Task` is saved in a variable:

- its value can be obtained using `let taskValue = await myTask.value`
- it can be notified that it is intended to be cancelled
  by calling `myTask.cancel()` before doing work

When a `Task` might be cancelled, it is responsible for
verifying whether it has been cancelled.
This can be done by testing the static `Bool` property `Task.isCancelled`.
When this is `true` the `Task` should gracefully stop the work it is doing.
Alternatively, a `Task` can call `try Task.checkCancellation()`
to throw a `CancellationError` if it has been cancelled.
If neither of these is done, cancelling the `Task` will have no effect.

The following code creates a `Task` inside in custom SwiftUI `View`
when it appears and cancels it when the view disappears:

```swift
import SwiftUI

struct Demo: View {
    @State private var task: Task<Void, Error>?

    var body: some View {
        VStack {
            Text("in Demo")
        }
        .onAppear {
            task = Task {
                print("in Task")
            }
        }
        .onDisappear {
            print("cancelling Task")
            task?.cancel()
        }
    }
}
```

The `Task` static property `isCancelled`
and the static method `checkCancellation`
apply to the `Task` inside which they are used.

The `Task` static method `sleep` takes a number of nanoseconds
and sleeps for at least that long. For example:

The `Task` static method `yield` lets higher priority tasks
jump in before continuing. For example: `await Task.yield()`.

```swift
try await Task.sleep(nanoseconds: 3 * 1_000_000_000) // 3 seconds
```

Many `async` methods in Apple frameworks check for cancellation
and stop their work gracefully.
One example is methods in the {% aTargetBlank
"https://developer.apple.com/documentation/foundation/urlsession",
"URLSession" %} class.

A `Task` inherits the following things from the `Task` that started it:

- running on the same actor
- running at the same priority
- cancellation status
- [task local variables](#task-local-variables) (described later)

In some cases it is desirable to start a new `Task`
that does not inherit from its parent `Task`.
To do this, call the static method `Task.detached`
instead of calling the `Task` initializer.

The following code demonstrates creating a `Task`
and cancelling it if it runs for too long.

```swift
import SwiftUI

// The following structs describe the JSON returned by
// the API endpoint https://randomuser.me/api/.

struct Location: Decodable {
    let street: Street
    let city: String
    let state: String
    let country: String
    let postcode: String

    // We need to decode this struct manually because
    // postcode can be a String or Int.
    // This requires defining the following enum.
    private enum CodingKeys: String, CodingKey {
        case street, city, state, country, postcode, coordinates, timezone
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        street = try container.decode(Street.self, forKey: .street)
        city = try container.decode(String.self, forKey: .city)
        state = try container.decode(String.self, forKey: .state)
        country = try container.decode(String.self, forKey: .country)
        do {
            // First try decoding postcode as a String.
            postcode = try container.decode(String.self, forKey: .postcode)
        } catch {
            // If it wasn't a String, try decoding postcode as an Int.
            postcode = try String(container.decode(Int.self, forKey: .postcode))
        }
    }
}

struct Name: Decodable {
    let title: String
    let first: String
    let last: String
}

struct Street: Decodable {
    let number: Int
    let name: String
}

struct User: Decodable {
    let name: Name
    let location: Location
    let email: String
}

struct Users: Decodable {
    let results: [User]
}

struct ContentView: View {
    enum MyError: Error {
        case badResponseType
        case badStatus(status: Int)
    }

    private let usersURL = URL(string: "https://randomuser.me/api/")!

    typealias UserTask = Task<User?, Error>

    @State private var isShowingError = false
    @State private var message = ""
    @State private var userTask: UserTask?
    @State private var user: User?

    private func cancel() {
        if let userTask {
            userTask.cancel()
            self.userTask = nil
        }
    }

    private func fetchUser() async {
        user = nil
        do {
            userTask = UserTask {
                // Simulate a long running task by sleeping for 1 second.
                try await Task.sleep(nanoseconds: 1_000_000_000)

                // If `userTask` is cancelled before the `data` method
                // completes, it will throw a "cancelled" error.
                let (data, response) =
                    try await URLSession.shared.data(from: usersURL)
                guard let res = response as? HTTPURLResponse else {
                    throw MyError.badResponseType
                }
                guard res.statusCode == 200 else {
                    throw MyError.badStatus(status: res.statusCode)
                }

                // We need to check whether this task has been cancelled
                // before doing more work.
                // Is so, one option is to return nil.
                // guard !Task.isCancelled else { return nil }

                // Another option is to throw a CancellationError.
                try Task.checkCancellation()

                let users = try JSONDecoder().decode(Users.self, from: data)
                return users.results.first
            }

            // This Task cancels `userTask` if runs for more than 1.2 seconds.
            Task {
                let seconds = 1.2
                let nanoseconds = seconds * 1_000_000_000
                try await Task.sleep(nanoseconds: UInt64(nanoseconds))
                cancel()
            }

            // If `userTask` has been cancelled, the value will be `nil`.
            user = try await userTask?.value
            message = ""

            // Once we get the value from the task, clear it
            // as an indication that it is no longer running.
            userTask = nil
        } catch {
            message = error.localizedDescription
            isShowingError = true
        }
    }

    private func render(user: User) -> some View {
        VStack(alignment: .leading) {
            let name = user.name
            let location = user.location
            let street = location.street
            Text("\(name.title) \(name.first) \(name.last)")
            // Using the String initializer prevents comma separators
            // when numbers are converted to strings.
            Text(String(street.number) + " " + street.name)
            Text("\(location.city), \(location.state)")
            Text("\(location.country) \(location.postcode)")
            Text(user.email)
        }
    }

    var body: some View {
        VStack(spacing: 20) {
            if userTask == nil {
                Button("Get Another User") {
                    Task { await fetchUser() }
                }
                .buttonStyle(.borderedProminent)
            }

            if let user {
                render(user: user)
            } else if userTask == nil {
                Text("The task was cancelled.")
            } else {
                Button("Cancel") {
                    cancel()
                }
                .buttonStyle(.bordered)
                ProgressView()
            }
            Spacer()
        }
        .padding()
        .task {
            await fetchUser()
        }
        .alert(
            "Error",
            isPresented: $isShowingError,
            actions: {},
            message: { Text(message) }
        )
    }
}
```

### Task Tree

When a `Task` create other tasks, those are considered to be
child tasks of their parent `Task`.
This creates a "task tree".

A parent `Task` is not considered finished
until all of its child tasks complete.

If an error is thrown by a `Task`, the `Task` ends
and the error is propagated to its parent `Task`.
Sibling tasks that are running or waiting to run are cancelled.

## Actors

Tasks can share mutable data across threads without danger of race conditions
by using an {% aTargetBlank
"https://developer.apple.com/documentation/swift/actor", "Actor" %}.

Actors:

- synchronize access to their mutable state (thread-safe)
- are a reference type like classes rather than a value type like structs
- are written similar to classes,
  substituting the `actor` keyword for the `class` keyword
- can conform to protocols, although it is difficult to implement
  methods that should be called in a synchronous context
- can obtain additional functionality from extensions

Accesses to actor properties and methods must occur in a
concurrent context (like a `Task` closure or an `async` function)
and be preceded by the `await` keyword.

Properties of an `actor` can only be modified inside a method of the `actor`.
Implement setter methods if this is needed.

`Actor` methods that have no danger of resulting in a race condition
can be marked with the `nonisolated` keyword.
This removes the need to call them in a concurrent context
and precede calls with the `await` keyword.

The following code demonstrates implementing a custom `Actor`
that maintains an array of `User` objects and
provides a method for adding a new `User`
that is obtained from an API endpoint.
The view model used by the UI asks the `Actor`
to add a new `User` every three seconds.
The UI also contains a `Button` that when tapped adds another `User`.

```swift
import SwiftUI

enum NetworkError: Error {
    case badResponseType
    case badStatus(status: Int)
    case noData
}

// The following structs describe the JSON returned by
// the API endpoint https://randomuser.me/api/.

struct Address: Decodable {
    let title: String
    let first: String
    let last: String
}

struct Location: Decodable {
    let street: Street
    let city: String
    let state: String
    let country: String
    let postcode: String

    // We need to decode this struct manually because
    // postcode can be a String or Int.
    // Manual decoding requires defining the following enum.
    private enum CodingKeys: String, CodingKey {
        case street, city, state, country, postcode, coordinates, timezone
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        street = try container.decode(Street.self, forKey: .street)
        city = try container.decode(String.self, forKey: .city)
        state = try container.decode(String.self, forKey: .state)
        country = try container.decode(String.self, forKey: .country)
        do {
            // First try decoding postcode as a String.
            postcode = try container.decode(String.self, forKey: .postcode)
        } catch {
            // If it wasn't a String, try decoding postcode as an Int.
            postcode = try String(container.decode(Int.self, forKey: .postcode))
        }
    }
}

struct Name: Decodable {
    let title: String
    let first: String
    let last: String
}

struct Street: Decodable {
    let number: Int
    let name: String
}

struct User: Decodable, Identifiable {
    let name: Name
    let location: Location
    let email: String

    var id: String { email }
}

struct Users: Decodable {
    let results: [User]
}

// This actor enables multiple threads to safely
// access an array of `User` objects and
// call the `addUser` method to add another user.
actor UsersActor {
    let usersURL = URL(string: "https://randomuser.me/api/")!

    // Multiple threads can safely access this property concurrently.
    var users: [User] = []

    // Multiple threads can safely call this method concurrently.
    func addUser() async throws {
        let (data, response) = try await URLSession.shared.data(from: usersURL)
        guard let res = response as? HTTPURLResponse else {
            throw NetworkError.badResponseType
        }
        guard res.statusCode == 200 else {
            throw NetworkError.badStatus(status: res.statusCode)
        }
        let decoded = try JSONDecoder().decode(Users.self, from: data)
        if let user = decoded.results.first {
            users.insert(user, at: 0)
        }
    }
}

// This view model allows the UI to access
// the array of `User` objects held by the actor above.
// It is not an option to make the custom actor above
// inherit from `ObservableObject` and use that as the view model
// because actors do not necessarily run on the main thread
// and ObservableObject subclasses must run on the main thread.
@MainActor
class UsersViewModel: ObservableObject {
    @Published var running = false
    @Published var users: [User] = []

    var timer: Timer?
    private let usersActor: UsersActor!

    init(usersActor: UsersActor) {
        self.usersActor = usersActor

        // Add a new user every three seconds.
        timer = Timer.scheduledTimer(
            withTimeInterval: 3,
            repeats: true
        ) { _ in
            Task {
                try await self.addUser()
            }
        }
        running = true
    }

    func addUser() async throws {
        try await usersActor.addUser()

        // Get all the users from UserActor.
        let actorUsers = await usersActor.users

        // Replace the array of users here
        // with the array in UsersActor.
        // This must be done on the main thread.
        await MainActor.run {
            users = actorUsers
        }
    }

    func cancel() {
        if let timer {
            timer.invalidate()
            running = false
        }
    }
}

struct ContentView: View {
    @StateObject private var viewModel =
        UsersViewModel(usersActor: UsersActor())

    func render(user: User) -> some View {
        VStack(alignment: .leading) {
            let name = user.name
            let location = user.location
            let street = location.street
            Text("\(name.title) \(name.first) \(name.last)")
            // Using the String initializer prevents comma separators
            // when numbers are converted to strings.
            Text(String(street.number) + " " + street.name)
            Text("\(location.city), \(location.state)")
            Text("\(location.country) \(location.postcode)")
            Text(user.email)
        }
        .padding(.top)
    }

    var body: some View {
        VStack {
            HStack {
                Button("Get Another User") {
                    Task {
                        try await viewModel.addUser()
                    }
                }
                .buttonStyle(.borderedProminent)

                if viewModel.running {
                    Button("Cancel") {
                        viewModel.cancel()
                    }
                }
            }

            List(viewModel.users) { user in
                render(user: user)
            }
        }
        .padding()
    }
}
```

## Sendable Types

In order to pass data from one `Task` to another,
the data must be thread-safe.
This is achieved by using data types that conform to the {% aTargetBlank
"https://developer.apple.com/documentation/swift/sendable",
"Sendable" %} protocol.

Passing data from one `Task` to another is not common.

The following types are `Sendable`:

- all `struct` and `enum` types
  unless they have properties that are not sendable.
- all `actor` types
- `class` types that explicitly conform to `Sendable`,
  are marked as `final` (other classes cannot inherit from it),
  and have no mutable properties
- methods and closures that are marked with the `@Sendable` attribute
  (Such closures can only capture sendable types.)

## Main Actor

{% aTargetBlank "https://developer.apple.com/documentation/swift/mainactor",
"MainActor" %} is a system-provided global actor
that performs its work on the main thread.

One way to ensure that code runs in the main thread
is to mark it with the `@MainActor` attribute.
Most types in SwiftUI and UIKit are marked with this.
It is recommend that all custom classes that inherit from `ObservableObject`
should do the same.

`@MainActor` can be applied to:

- type declarations for a `class`, `struct`, or `enum`
  (This causes all of their properties to be accessed on the main thread
  and all of their methods to execute on the main thread.)
- properties of a type
- methods of a type
- functions
- closures

Functions that are running in the context of a different actor
can call functions that will run in the `MainActor` context,
but they must call them asynchronously using `await` or `async let`.

When a function that is running on the main thread
calls an `async` method that returns a value,
it may run on a different thread,
but the assignment of the result to a local variable
will occur in the main thread.

Methods in a type to which `@MainActor` is applied
can be marked with `nonisolated` to allow it
to be called from any concurrent context.
This can improve their performance.
However, such methods cannot return the value of a property
in the type or a value computed from the properties.

To apply `@MainActor` to a closure, add it before the parameter list.
This is typically only done for closures that update the UI.
For example:

```swift
Task { @MainActor p1, p2 in
    ...
}

// This closure has no parameters.
Task { @MainActor in
    ...
}

// This is a longer alternative that does the same thing.
Task {
    await MainActor.run {
        ...
    }
}

// This is an even longer alternative that does the same thing.
// that was the approach before the async/await system was introduced.
DispatchQueue.main.async {
    ...
}
```

## Custom Global Actors

Recall that the purpose of an actor is to enable tasks to
share mutable data without danger of race conditions.
To simplify accessing a custom actor from many functions,
define a custom global actor.
To do so, apply the `@globalActor` attribute
to a struct that contains an `actor` definition and
makes an instance available through a `static` property named `shared`.
Then mark the types and functions that should run in the context of that actor
with an attribute that uses the global actor name.

For example:

```swift
@globalActor
struct MyGlobalActor {
    actor MyActor {
        ...
    }

    static let shared: MyActor = MyActor()
}

@MyGlobalActor
struct MyStruct {
    // All accesses to properties defined here
    // and all calls to methods defined here
    // will occur in the context of the `MyGlobalActor` actor.
    ...
}
```

## AsyncSequence

An {% aTargetBlank
"https://developer.apple.com/documentation/swift/asyncsequence",
"AsyncSequence" %} supports iterating over a sequence of values
that are obtained asynchronously.
Unlike a {% aTargetBlank
"https://developer.apple.com/documentation/swift/sequence", "Sequence" %}
which holds a collection of values,
an `AsyncSequence` just provides a way to access values.

A `TaskGroup` uses an `AsyncSequence` to provided its results.
The following line of code from the [Task Groups](#task-groups) section above
takes advantage of this:

```swift
for try await dogImage in group {
```

Instances of `AsyncSequence` support many of the same methods
found in the `Sequence` protocol such as `map`, `filter`, and `reduce`.
These return a new `AsyncSequence` instance
which enables method calls to be chained.
Some `Sequence` methods such as `dropFirst`
are not supported by `AsyncSequence`.

The work of retrieving values from an `AsyncSequence` does not
begin until it is used in a `for await` or `for try await` loop.
Chaining methods like `map`, `filter`, and `reduce`
only configure the processing that will occur
when code begins asking for values.

An `AsyncSequence` is always executed only one time
and the results are cached.
If an `AsyncSequence` is iterated over again, the cached results are returned.
TODO: Does it cache all the values even if it is a very long sequence?
TODO: See https://github.com/AndyIbanez/modern-concurrency-on-apple-platforms-book-code/issues/2.

It is not possible to ask an `AsyncSequence` for its count.

The {% aTargetBlank "https://developer.apple.com/documentation/foundation/url",
"URL" %} struct has a {% aTargetBlank
"https://developer.apple.com/documentation/foundation/url/3767315-lines",
"lines" %} property whose type is `AsyncLineSequence<URL.AsyncBytes>`.
This enables iterating over the lines found at a URL asynchronously.

The following code demonstrates
reading the lines in a CSV file found at a `URL`.
Each row of the CSV data provides information about a city.
There are ten columns in each row.
The last column holds a state abbreviation.
The `await` keyword must be used to wait for each line to be delivered.
The lines are filtered to only get data for cities in a certain state.

```swift
let citiesURL = "https://people.sc.fsu.edu/~jburkardt/data/csv/cities.csv"
let url = URL(string: citiesURL)!
let citiesInMissouri = url.lines.filter { line in
    let columns = line.components(separatedBy: ",")
    let state = columns.last!.trimmingCharacters(in: .whitespaces)
    return state == "MO"
}
for try await line in citiesInMissouri {
    print(line)
}
```

Other standard API methods that return an `AsyncSequence` include:

- {% aTargetBlank
  "https://developer.apple.com/documentation/foundation/filehandle/asyncbytes/3766668-lines",
  "FileHandle.standardInput.bytes.lines" %}
- {% aTargetBlank "https://developer.apple.com/documentation/foundation/url/3767316-resourcebytes", "URL.resourceBytes" %}
- {% aTargetBlank "https://developer.apple.com/documentation/foundation/urlsession/3767351-bytes", "URLSession.bytes" %}
- {% aTargetBlank "https://developer.apple.com/documentation/foundation/notificationcenter/3813137-notifications", "NotificationCenter.notifications" %}

Just like in synchronous `for` loops, the `continue` and `break` keywords
can be used in `for await` and `for try await` loops.

## Task Local Variables

The `@TaskLocal` attribute can be applied to
`static` property declarations in a type definition.
This allows data, referred to as "task local variables",
to be shared across tasks in the task tree.
These properties must be given a default value or have an optional type.

Task local variables in type instances that are created in a task
can be read and modified by any descendant tasks in the task tree.

To read the value of a task local variable,
precede its name with the `await` keyword.
For example:

```swift
let value = await SomeClass.someTaskLocalVariable
```

To modify the value of a task local variable, call the {% aTargetBlank
"https://developer.apple.com/documentation/swift/tasklocal/withvalue(_:operation:file:line:)-79atg",
"withValue" %} method on a binding to the property, passing it a new value.
For example:

```swift
SomeClass.$someTaskLocalVariable.withValue(someNewValue) {
    ... code to execute ...
}
```

The new value is only available in non-detached tasks that are
spawned by the closure passed to the `withValue` method.

`SomeClass` in the examples above can be replaced by `Self`
when inside the same class that defines the task local variable.

## Thread Sanitizer

A data race can occur when multiple concurrently running threads
access the same memory and at least one is modifying the memory.
This can cause unpredictable results, data corruption,
and application crashes.

Typically using actors and serial queues prevents data races.
But they can still occur when using concurrent queues.
The "Thread Sanitizer" (aka TSan) is a tool built into Xcode
that aids in detecting and debugging data races.
It is supported on all 64-bit platforms.
However, the app must be run in the Simulator rather than on a device.

To use the Thread Sanitizer in Xcode:

- Select "Edit Scheme..." from the dropdown menu to the left of the
  device dropdown which appears at the top center of the Xcode window.
- Select the "Run" category in the left nav.
- Select the "Diagnostics" tab.
- Check the "Thread Sanitizer" checkbox.
- Click the "Close" button.
- Rebuild the app. This will add code
  around all memory accesses to log them.
- Run the app in the Simulator.
  This will decrease the performance of the app,
  running up to 20 times slower.
- See warnings about data races in the "Issue Navigator"
  and on specific lines of code in code editor panels.

## Swift Async Algorithms

The Apple open source package {% aTargetBlank
"https://github.com/apple/swift-async-algorithms", "swift-async-algorithms" %}
provides types and functions for operating on asynchronous sequences
that provide values over time.
Examples include `debounce` and `throttle`.

In many cases this package can be used in place of the {% aTargetBlank
"https://developer.apple.com/documentation/combine", "Combine" %} framework.

To use this, add `AsyncAlgorithms` as a package dependency.
