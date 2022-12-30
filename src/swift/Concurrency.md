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

- Deadlocks: two processes are waiting each other to finish so neither can
- Starvation:
- Race Conditions:
- Livelocks:

## Low-level Approaches

### Mutexes

### Semaphores

### pthreads

### NSThreads

### Grand Central Dispatch (GCD)

### NSOperation APIs

## async/await keywords

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
