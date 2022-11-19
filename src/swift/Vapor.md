---
eleventyNavigation:
  key: Vapor
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://vapor.codes", "Vapor" %} is a web server framework
for macOS that is written in Swift.

## Installing

1. Install {% aTargetBlank "https://brew.sh", "Homebrew" %}.
1. Enter `brew install vapor`

## Project Creation

To create a Vapor project, enter `vapor new {project-name}`

## Server Running

To run the server, enter `vapor run`.
The server listens on port 8080 by default.
To verify that it is working,
browse localhost:8080 which outputs "It works!" or
browse localhost:8080/hello which outputs "Hello, world!".

There does not seem to be support for listening for code changes
and automatically restarting the server (like nodemon in Node.js).

## Defining Routes

To define new routes (or endpoints), edit `Sources/App/routes.swift`.
The `route` function defined here is called by the
`configure` function that is defined in `Sources/App/configure.swift`.
The `configure` function is called by `Sources/Run/main.swift`.

The route helper methods that map to HTTP methods are
`get`, `post`, `patch`, `put`, and `delete`.
The route helper method `on` accepts an HTTP method as a parameter.

The following code is from the `routes.swift` file.
It demonstrates defining routes that use
both path parameters and query parameters.

```swift
import Vapor

func routes(_ app: Application) throws {
    // route is /
    app.get { _ in
        "It works!"
    }

    // route is /hello
    // Return type of the function passed to `app.get`
    // specifies the kind of data returned.
    app.get("hello") { _ -> String in
        "Hello, world!"
    }

    // This demonstrates using path parameters.
    // route example is /greet/Tami
    // The `get` method takes any number of string arguments
    // that each represent a path part that can be
    // constant (like "greet") or variable (like ":name").
    // "*" matches any value.
    // "**" matches any number of path parts with any values.
    // (Can "**" only be used as the last argument?)
    app.get("greet", ":name") { req -> String in
        // This demonstrates getting the value of a variable path parameter.
        // Variable path parameters will never be nil,
        // so a forced unwrap is safe.
        guard let name = req.parameters.get("name") else {
            throw Abort(.badRequest)
        }
        return "Hello, \(name)!"
    }

    // This demonstrates using query parameters.
    app.get("divide") { req -> Double in
        guard let dividend: Double = req.query["dividend"] else {
            throw Abort(.badRequest)
        }
        guard let divisor: Double = req.query["divisor"] else {
            throw Abort(.badRequest)
        }
        return dividend / divisor
    }

    // This demonstrates another approach for using query parameters.
    // The struct below describes the supported query parameters.
    // They can be required or optional (by using `?`).
    struct Divide: Content {
        var dividend: Double
        var divisor: Double
    }

    // This uses the struct above to decode the query parameters.
    app.get("divide2") { req -> Double in
        let query = try req.query.decode(Divide.self)
        return query.dividend / query.divisor
    }
}
```

## Returning JSON

The following code is from the `routes.swift` file.
It demonstrates returning JSON.

```swift
import Vapor

func routes(_ app: Application) throws {
    // The Content protocol combines
    // Decodable, Encodable, and RequestDecodable.
    struct Address: Content {
        let street: String
        let city: String
        let state: String
        let zip: Int
    }

    app.get("address") { _ -> Address in
        Address(
            street: "123 Some Lane",
            city: "Somewhere",
            state: "CA",
            zip: 12345
        )
    }
}
```
