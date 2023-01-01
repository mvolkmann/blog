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

## Queues and Threads

Tasks represent a body of work to be performed.
To schedule the work, a task is added to a queue
in either a blocking fashion (runs synchronously)
or a non-blocking fashion (runs asynchronously).

A queue is responsible for determining when its tasks will run
and the threads on which they will run.
Serial queues execute one task at a time
and each task can run on a different thread.
Concurrent queues can execute multiple tasks at the same time
which requires multiple threads.
Both kinds of queues execute their tasks in the order in which they were added.

Each queue collect tasks to be run at
a given priority or quality of service (QoS).
Applications can create any number of queues.
There are six "global queues" that are typically used
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
If the UI is updated on a thread other than the main thread,
it may have no effect or the application may crash.
TODO: Does it sometimes work?
The Swift compiler provides warnings when it detects
code that attempts to update the UI outside of the main thread.

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
"NSThread" %} class is part of the Apple Foundation framework.
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

The `await` keyword can choose to suspend execution of the current function.
This allows the thread that was executing the function to perform other work.
The code after the function call that uses `await` (the suspension point)
is referred to as a "continuation".
When the function is resumed later, the continuation is executed,
possibly in a different thread than the one in which function execution began.

In iOS 15 and above, all Apple provided functions that
take a completion handler also have an `async` version.
This means there is no longer a need to
pass completion handlers to Apple's APIs,
resulting in code that is easier to write and read.
For example, {% aTargetBlank
"https://developer.apple.com/documentation/foundation/urlsession",
"URLSession" %} has many async methods.

The following code demonstrates using the `async` and `await` keywords
with the `URLSession` class.

```swift
import SwiftUI

struct Activity: Decodable {
    let activity: String
    let type: String
    let participants: Int
    let price: Double
    let link: String
    let key: String
    let accessibility: Double
}

struct ContentView: View {
    @State private var activity: Activity?
    @State private var fetching = false
    @State private var message = ""

    let apiURL = URL(string: "https://www.boredapi.com/api/activity")!

    private func fetchActivity() async {
        fetching = true
        message = ""

        do {
            // The data method returns a tuple.
            // The type of response is URLResponse.
            // Cast it to HTTPURLResponse to get information from it.
            let (data, response) =
                try await URLSession.shared.data(from: apiURL)
            if let res = response as? HTTPURLResponse {
                if res.statusCode == 200 {
                    activity = try JSONDecoder()
                        .decode(Activity.self, from: data)
                } else {
                    message = "bad status \(res.statusCode)"
                }
            } else {
                message = "bad response type"
            }
        } catch {
            message = "error: \(error)"
        }

        fetching = false
    }

    var body: some View {
        VStack {
            Text("Are you bored?").font(.largeTitle)
            if fetching {
                Text("Waiting for suggestion ...")
                ProgressView()
            } else {
                Button("Get Suggested Activity") {
                    Task { await fetchActivity() }
                }
                .buttonStyle(.borderedProminent)

                if let activity {
                    Text(activity.activity + ".").font(.title)
                }
            }

            if !message.isEmpty {
                Text(message).foregroundColor(.red)
            }
        }
        .padding()
        // This is a view modifier that is similar to onAppear,
        // but runs the closure passed to it in a `Task`
        // which provides an async context.
        // If the view is removed, the `Task` is cancelled.
        .task {
            await fetchActivity()
        }
    }
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
    // This custom error type is passed to
    // `completion.resume(throwing: ...)` below.
    enum MyError: Error {
        case badResponseType, badStatus, noData
    }

    // This replaces the `fetchActivity function in the previous example
    // with one that calls the wrapping function
    // `fetchActivityWithContinuation`.
    private func fetchActivity() async {
        fetching = true
        message = ""

        do {
            activity = try await fetchActivityWithContinuation()
        } catch {
            message = error.localizedDescription
        }

        fetching = false
    }

    private func fetchActivityWithContinuation() async throws -> Activity {
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
                    let activity = try JSONDecoder().decode(
                        Activity.self,
                        from: data
                    )
                    completion.resume(returning: activity)
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
These statements must be used inside an async context
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
    private func getActivity() async throws -> Activity {
        ...
    }
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
We begin by calling {% aTargetBlank
"https://developer.apple.com/documentation/swift/withtaskgroup(of:returning:body:)",
"withTaskGroup" %} (when the tasks cannot throw)
which creates a {% aTargetBlank
"https://developer.apple.com/documentation/swift/taskgroup", "TaskGroup" %}
or by calling {% aTargetBlank
"https://developer.apple.com/documentation/swift/withthrowingtaskgroup(of:returning:body:)",
"withThrowingTaskGroup" %} (when the tasks can throw)
which creates a {% aTargetBlank
"https://developer.apple.com/documentation/swift/throwingtaskgroup",
"ThrowingTaskGroup" %}.
Inside the closure where the group is created we call the `addTask` method
once for each value to be computed.

The system will decided how many of the tasks to run concurrently.
Excess tasks will wait for running tasks to complete before they begin.

The following code demonstrates the downloading
a random number of dog image URLs, 1 to 5.

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

Tasks are not guaranteed to run in the order
in which they were added to the group.

## Unstructured Concurrency

Unstructured concurrency, like structured concurrency,
provides a way to execute multiple tasks at the same time.
However, it does not enable writing code
in the order in which it is expected to run.

### Task

Unstructured concurrency relies on creating {% aTargetBlank
"https://developer.apple.com/documentation/swift/task", "Task" %} objects
which are passed a closure that runs in an asynchronous context.
The system typically runs the task immediately, but can choose to
defer execution based on the number of tasks that are already running.

`Task` is a generic struct. When creating an instance,
specify the `Success` type which is the type of the value it will hold
and the `Error` type which is the type of error it can hold.
If a `Task` never throws, specify `Never` for the `Error` type.

The `Task` initializer can be passed the priority under which it should run.
When no priority is specified, the priority of the parent `Task` is used.
The available priorities from lowest to highest are:

- `.low` or `.background`
- `.medium` or `.utility`
- `.high` or `.userInitiated`

When a `Task` is saved in a variable:

- its value can be obtained using `let taskValue = await myTask.value`
- it can be notified that it is intended to be cancelled
  by calling `myTask.cancel()`

When a `Task` might be cancelled, it is responsible for verifying
whether it has been cancelled and gracefully stopping the work it is doing.
This can be done by testing the static `Bool` property `Task.isCancelled`.
Alternatively, call `try Task.checkCancellation()`
to throw a `CancellationError` if the `Task` has been cancelled.
If neither of these is done, cancelling the `Task` will have no effect.

Many `async` methods in Apple frameworks check for cancellation
and stop their work gracefully.
One example is methods in the `URLSession` class.

The `Task` static property `isCancelled`
and the static method `checkCancellation`
apply to the `Task` inside which they are used.

A `Task` inherits several things from the `Task` that started it including:

- running on the same actor
- running at the same priority
- cancellation status
- task local variables (described later)

In some cases it is desirable to start a new `Task`
that does not inherit from its parent `Task`.
To do this, create the task by calling `Task.detached` instead of `Task`.

The following code demonstrates creating a `Task`
and cancelling it if it runs for too long.

```swift
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
    // let coordinates: Coordinates
    // let timezone: Timezone

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

    private func fetchUser() async throws {
        let userTask = Task<User?, Error> {
            // If `userTask` is cancelled before the `data` method completes,
            // it will throw a "cancelled" error.
            let (data, response) =
                try await URLSession.shared.data(from: usersURL)

            guard let res = response as? HTTPURLResponse else {
                throw MyError.badResponseType
            }
            guard res.statusCode == 200 else {
                throw MyError.badStatus(status: res.statusCode)
            }

            // One option is to return nil if the task has been cancelled.
            // guard !Task.isCancelled else { return nil }

            // Another option is to throw a CancellationError
            // if this task has been cancelled.
            try Task.checkCancellation()

            let users = try JSONDecoder().decode(Users.self, from: data)
            return users.results.first
        }

        // If `userTask` runs for more than a tenth of second, cancel it.
        Task {
            let seconds = 0.1
            let nanoseconds = seconds * 1_000_000_000
            try await Task.sleep(nanoseconds: UInt64(nanoseconds))
            userTask.cancel()
        }

        // If `userTask` is cancelled, the value will be `nil`.
        user = try await userTask.value
    }
```

### Task Tree

When a `Task` create other tasks, those are considered to be
child tasks of their parent `Task`.
This creates a "task tree".

A parent `Task` is not considered finished
until all of its child tasks complete.

If an error is thrown by a `Task`, the `Task` ends
and the error is propagated to the parent `Task`.
Sibling tasks that are running or waiting to run are cancelled.

## Actors

Tasks can share mutable data without danger of race conditions
by using an {% aTargetBlank
"https://developer.apple.com/documentation/swift/actor", "Actor" %}.

Actors:

- are a reference type like classes rather than a value type like structs
- synchronize access to their mutable state
- are written similar to classes,
  substituting the `actor` keyword for the `class` keyword
- can conform to protocols, although it is difficult to implement
  methods that should be called in a synchronous context
- can obtain additional functionality from extensions

Accesses to actor properties and methods
must occur in an asynchronous context
and be preceded by the `await` keyword.

`Actor` methods that have no danger of resulting in a race condition
can be marked with the `nonisolated` keyword.
This removes the need to call them in an asynchronous context
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

// This view model allows the UI to access the array of `User` objects
// held by the actor above.
// It is not an option to make the custom actor above
// inherit from `ObservableObject` and use that as the view model
// because actors do not necessarily run on the main thread
// and ObservableObject subclasses must run on the main thread.
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
This is achieved by using {% aTargetBlank
"https://developer.apple.com/documentation/swift/sendable", "Sendable" %}.

Passing data from one `Task` to another is not common.

What types are sendable?

- All `struct` and `enum` types are sendable
  unless they have properties that are not sendable.
- All `actor` types are sendable.
- `class` types are sendable if they conform to `Sendable`,
  are marked as `final` (other classes cannot inherit from it),
  and all their properties are read-only.
- Methods and closures can be made sendable by
  marking them with the `@Sendable` attribute.
  Such closures can only capture sendable types.

## Main Actor

{% aTargetBlank "https://developer.apple.com/documentation/swift/mainactor",
"MainActor" %} is a system-provided global actor
that performs its work on the main thread.

One way to ensure that code runs in the main thread
is to mark it with the `@MainActor` attribute.
Most types in SwiftUI and UIKit are marked with this
and it is recommend that all classes that inherit from `ObservableObject`
should do the same.

`@MainActor` can be applied to:

- type declarations for a `class`, `struct`, or `enum`
  which causes all of their properties to be accessed on the main thread
  and all of their methods to execute on the main thread
- properties of a type
- methods of a type
- functions

Functions that are running in the context of a different actor
can call functions that will run in the `MainActor` context,
but they must call them asynchronously using `await` or `async let`.

Async methods may run on a different thread,
but the assignment of the result to a local variable
will occur in the main thread.

## AsyncSequence
