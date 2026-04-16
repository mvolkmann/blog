---
eleventyNavigation:
  key: Grid
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS 16 added the views [Grid](<https://developer.apple.com/documentation/swiftui/grid?v=1.1.1>)
and [GridRow](<https://developer.apple.com/documentation/swiftui/gridrow?v=1.1.1>)
that are used to arrange other views in a grid of rows and columns.

## Example App

See the example app [GridLayoutDemo](<https://github.com/mvolkmann/GridLayoutDemo?v=1.1.1>).
It is based on the YouTube video [SwiftUI Grids in iOS 16](<https://www.youtube.com/watch?v=ZU_6RejjIKU&v=1.1.1>)
by Stewart Lynch.

## `Grid`

A [Grid](<https://developer.apple.com/documentation/swiftui/grid?v=1.1.1>) contains a number of rows.
It is a container view that takes all the space offered to it.

Each row contains the same number of columns
and can contain a different number of cells.

Each cell can span any number of columns.
The sum of the cell spans in a row cannot exceed the number of columns.

## `GridRow`

A [GridRow](<https://developer.apple.com/documentation/swiftui/gridrow?v=1.1.1>) is a child of a `Grid` that
takes all the space inside its parent `Grid`.

A `GridRow` adds
a default amount of space (10) between each of its child cells
and a default amount of space (10) between each row.
To change these, pass the `horizontalSpacing` and/or `verticalSpacing`
arguments to the `Grid` initializer.

To set the vertical alignment of cells within a row,
pass the `alignment` argument to the `GridRow` initializer
with a value like `.bottom`, `.center`, or `.top`.

## Columns

To set the default horizontal alignment of all cells in a column,
apply the `gridColumnAlignment` view modifier to
the corresponding view in the first `GridRow`
with a value like `.leading`, `.center`, or `.trailing`.
The same alignment will be used for all views in that column.

## Cells

Cells are views that are children of a `GridRow`.
Calls are either "pushing" (expands) or "pulling" (doesn't expand).

Cells are assigned to a specific column based on their order and span sizes.
In rows where the sum of the cell spans is less than the number of columns,
the excess columns remain empty.

The height of each row defaults to the height of the tallest cell in the row.
The width of each column defaults to the width of the widest cell in the column.

By default cells are centered vertically within their row
and centered horizontally within their column.

To cause a cell to span multiple columns,
apply the `gridCellColumns(numberOfColumns)` view modifier to the cell.

To override the vertical alignment setting of the current `GridRow`
for a specific cell, apply the `gridCellAnchor` view modifier
with a value like `.bottom`, `.center`, or `.top`.

To set the default alignment of all cells within the columns they span,
pass the `alignment` argument to the `Grid` initializer
with a value one of the following values that are like compass directions:

- `.topLeading`, `.top`, `.topTrailing`,
- `.leading`, `.center`, `.trailing`,
- `.bottomLeading`, `.bottom`, `.bottomTrailing`

To override the alignment of a specific cell,
apply the `gridCellAnchor` view modifier with one of values listed above.

One way to add an "empty" cell to a row is to use:

```swift
Color.clear.gridCellUnsizedAxes([.horizontal, .vertical])
```

Using `EmptyView()` does not work.

## Resources

- [SwiftUI Grid](<https://sarunw.com/posts/swiftui-grid/?v=1.1.1>) by Sarunw
- [Mastering grid layout in SwiftUI](<https://swiftwithmajid.com/2022/08/10/mastering-grid-layout-in-swiftui/?v=1.1.1>) by Majid
- [Eager Grids with SwiftUI](<https://swiftui-lab.com/eager-grids/?v=1.1.1>) by Javier
