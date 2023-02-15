---
eleventyNavigation:
  key: SwiftLint
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://realm.github.io/SwiftLint/", "SwiftLint" %}
is a "tool to enforce Swift style and conventions".

## Installing

One way to install SwiftLint is to use
{% aTargetBlank "https://brew.sh", "HomeBrew" %}.
Enter `brew install swiftlint`.

For other options, see the {% aTargetBlank
"https://realm.github.io/SwiftLint/", "SwiftLint home page" %}.

## Running from a Terminal

To run SwiftLint from a terminal,
cd to a project directory and enter `swiftlint`.
This will output a list of each file that is evaluated
along with warning and error messages for all violations found.

To run SwiftLint, but suppress listing the names of each evaluated file,
enter `swiftlint --quiet`.

To fix all violations that can be automatically fixed,
enter `swiftlint --fix`.

## Running from Xcode

To configure Xcode to run SwiftLint every time the project is built:

1. Select the topmost entry in the file navigator.
1. Select the primary target.
1. Select the "Build Phases" tab.
1. Click the "+" in the upper-left.
1. Select "New Run Script Phase".
1. Rename the new run script phase to "SwiftLint".
1. Paste the following shell script code in the Shell input.
   This assumes that SwiftLint was installed using Homebrew.

   ```shell
   export PATH="$PATH:/opt/homebrew/bin"
   if which swiftlint > /dev/null; then
     swiftlint
   else
     echo "warning: SwiftLint not installed"
   fi
   ```

## Configuration

To configure SwiftLint, create the YAML file `.swiftlint.yml`
at the root of each project.
Here is an example:

```yaml
# This is a list of project subdirectories that should be checked.
included:
  - Source

# This is a list of project subdirectories that should not be checked.
excluded:
  - Carthage
  - Pods

# This is a list of rules to disable that are enabled by default.
disabled_rules:
  - todo

# This is a list of rules to enable that are disabled by default.
opt_in_rules:
  - closure_end_indentation
  - empty_count

# This customizes a specific rule.
identifier_name:
  min_length:
    error: 3
  excluded:
    - dx
    - dy
    - id

# This customizes a specific rule.
line_length:
  warning: 100
  error: 120
  ignores_urls: true
```

## Rules

For documentation on supported rules, see the {% aTargetBlank
"https://realm.github.io/SwiftLint/rule-directory.html", "Rule Directory" %}.

## Selective Disabling

To disable specific rules for a specific type or function definition,
add the following immediately before it:

```swift
// swiftlint:disable rule1 rule2 rule3
```

To do this for a section of code, surround it as follows:

```swift
// swiftlint:disable rule1 rule2 rule3
... code ...
// swiftlint:enable rule1 rule2 rule3
```
