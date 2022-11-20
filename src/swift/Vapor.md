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
1. Add a package dependency to the Xcode project.
   1. Click the top entry in the Navigator.
   1. Click the project name.
   1. Click the "Package Dependencies" tab.
   1. Search for the URL https://github.com/vapor/vapor.
   1. Click the "Add Package" button.
      TODO: I thought this would add Vapor code completions and
      access to its source code, but it didnt!
   1. In the dialog that appears, click the "Add Package" button again.

## Project Creation

To create a Vapor project, enter `vapor new {project-name}`

## Running Server

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

## JSON Support

The {% aTargetBlank "https://docs.vapor.codes/basics/content/", "Content" %}
API supports parsing JSON in request bodies
and generating JSON for response bodies.

The following code is from the `routes.swift` file.
It demonstrates defining routes that return JSON.

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

    app.get("addresses") { _ -> [Address] in
        let a1 = Address(
            street: "123 Some Lane",
            city: "Somewhere",
            state: "CA",
            zip: 12345
        )
        let a2 = Address(
            street: "456 Some Street",
            city: "Nowhere",
            state: "CA",
            zip: 98765
        )
        return [a1, a2]
    }
}
```

## Database Support

Routes can perform CRUD operations on databases.
The supported databases are MongoDB, MySQL, PostgreSQL, and SQLite.

Object Relational Mapper (ORM) support is provided by the
{% aTargetBlank "https://docs.vapor.codes/fluent/overview/", "Fluent" %}
framework.

## HTML Generation

Routes can return server-rendered HTML using the
{% aTargetBlank "https://docs.vapor.codes/leaf/getting-started/", "Leaf" %}
templating language.

## Request Validation

Routes can validate the HTTP requests sent to them using the {% aTargetBlank
"https://docs.vapor.codes/basics/validation/", "Validation" %} API.
It can validate query parameters and request bodies.

One motivation for using this is that it provides human-readable error messages.

## Sending HTTP Requests

Routes can send HTTP requests to endpoints on other servers using the
{% aTargetBlank "https://docs.vapor.codes/basics/client/", "Client" %} API.
This is built on the {% aTargetBlank
"https://swift-server.github.io/async-http-client/docs/current/AsyncHTTPClient/index.html",
"AsyncHTTPClient" %} package which can also be used in Swift client apps.
However, the code is not any simpler than using {% aTargetBlank
"https://developer.apple.com/documentation/foundation/urlsession",
"URLSession" %}.

## Environments

Routes can use the {% aTargetBlank
"https://docs.vapor.codes/basics/environment/", "Environment" %} API
to modifier their behavior based on the current environment
such as production, development, or testing.
One use is to select the database instance that is accessed.
