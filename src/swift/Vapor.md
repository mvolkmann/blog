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

To define new routes (or endpoints), edit `Sources/App/routes.swift`.
The `route` function defined here is called by the
`configure` function that is defined in `Sources/App/configure.swift`.
The `configure` function is called by `Sources/Run/main.swift`.
