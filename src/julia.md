---
eleventyNavigation:
  key: Julia
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://julialang.org", "Julia" %} is a
dynamically typed programming language that provides high performance.
It is often used as an alternative to Python or Matlab for data visualization.

Julia has many things in common with Lua including:

- dynamic types
- no semicolons
- no parentheses are conditions in `if` statements and loops
- use of the `end`keyword
- 1-based indexes
- parallel assignment

Julia differs from Lua in that it is:

- much faster than Lua and Python
- much larger (not ideal for embedding in another app; 26MB vs 750KB)
- supports the ternary operator

Julia source files have an extension of `.jl`.

Julia is great for applications that involve math.
It supports a very concise syntax for defining functions like polynomials.

Julia uses the {% aTargetBlank "https://llvm.org", "LLVM" %}
compiler infrastructure.

## Installing

To install in macOS using Homebrew, enter `brew install julia`.

## Running

To start a REPL, enter `julia`.

To execute a Julia source file, enter `julia {file-name}.jl`.

## Examples

```julia
# Can precede a variable with a constant for implicit multiplication.
f(x) = 3x^2 + 7x - 2

for x = 0:5
  println("f($x) = $(f(x))")
end

g(x, y) = 3x^2 + 5x*y - 6y^2 + 4x - y + 8

# Can use Unicode characters and π is a predefined constant.
circleArea(r) = π*r^2
```

## Jupyter Notebooks

Jupyter Notebooks can be configured to run Julia code as described
{% aTargetBlank "https://datatofish.com/add-julia-to-jupyter/", "here" %}.

A popular alternative is {% aTargetBlank "https://juliahub.com", "JuliaHub" %}
which is the successor to JuliaBox.

## Outstanding Queestions

- Can Julia functions be called from Swift?
- Can a Julia function return more than one value?
- What happens if you call a function with too few or too many arguments?
- Are anonymous functions restricted to only having one statement?
- Is "broadcast" like the JavaScript Array map method?
- How can Julia use packages implemented in other languages?
