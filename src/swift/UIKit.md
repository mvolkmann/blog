---
eleventyNavigation:
  key: UIKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

TODO: Add detail here.

## Relationship to SwiftUI

The table below identifies the SwiftUI views
that correspond to UIKit concepts:

| UIKit                                         | SwiftUI                        |
| --------------------------------------------- | ------------------------------ |
| `NSAttributedString`                          | `Text` with `AttributedString` |
| `UIActivityIndicatorView`                     | `ProgressView without a value` |
| `UIAlertController` with style `.actionsheet` | `ActionSheet`                  |
| `UIAlertController` with style `.alert`       | `Alert`                        |
| `UIButton`                                    | `Button`                       |
| `UICollectionView`                            | `LazyVGrid and LazyHGrid`      |
| `UIDatePicker`                                | `DatePicker`                   |
| `UIImageView`                                 | `Image`                        |
| `UILabel`                                     | `Text`                         |
| `UINavigationController`                      | `NavigationView`               |
| `UIProgressView`                              | `ProgressView with a value`    |
| `UISegmentedControl`                          | `Picker`                       |
| `UISlider`                                    | `Slider`                       |
| `UIStackView` with horizontal axis            | `HStack`                       |
| `UIStackView` with vertical axis              | `VStack`                       |
| `UIStepper`                                   | `Stepper`                      |
| `UISwitch`                                    | `Toggle`                       |
| `UITableView`                                 | `List`                         |
| `UITextField`                                 | `TextField`                    |
| `UITextField` with `isSecureTextEntry` true   | `SecureField`                  |
| `UITextView`                                  | `TextEditor` for plain strings |
