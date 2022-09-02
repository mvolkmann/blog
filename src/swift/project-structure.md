---
eleventyNavigation:
  key: Project Structure
  parent: Swift
layout: topic-layout.njk
---

## Overview

This post documents my recommended structure for SwiftUI projects.

## Groups

Most files in a project should be placed in a group.
Group names typically begin with an uppercase letter.
Should they contain spaces?

Recommended group names include:

- Config
- Extensions
- Models
- Services
- ViewModels
- Views

- spaces?

## Group and file order

## File name suffixes within groups

## When to use View Models

## When to use Services

- wrapping all network and external API access (like CloudKit, Core Data, and HealthKit)

## Where and how to define constants: enum cases vs. static properties

## Order of sections within files and use of pragma marks

## Indentation and wrapping style

## Use of SwiftLint and SwiftFormat

## Preference for async/await over completion handlers (callbacks)

- wrapping callback style code in async functions

## Common extensions

- Date, String, View, …
- copied from project to project
