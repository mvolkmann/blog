---
eleventyNavigation:
  key: Pen
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

The `Pen` package supports turtle-like graphics.

To install the `Pen` package, clone the
[Morphic](https://github.com/Cuis-Smalltalk/Morphic.git) GitHub repository,
enter `Feature require: 'Pen'` in a Workspace, and "Do it".

The `Pen` class provides several examples in class methods.
Try entering these in a Workspace and "Do it".

```smalltalk
Display restoreAfter: [Pen example]
Pen exampleSketch. "drag around window to draw a thin, green trail"
Pen makeStar display.

```

```smalltalk
Display restoreAfter: [
    Display fillWhite.
    pen := Pen new.
    pen home. "center of window"
    pen location logAs: 'location'.
    pen direction logAs: 'direction'.
    pen squareNib: 10.
    pen color: (Color blue).
    pen down.
    "pen north. There is no south, west, or east."
    pen go: 50.
    pen turn: -90.
    pen go: 100.
    pen up.
    pen goto: 200@200. "will draw if pen is down"
    pen color: (Color red).
    pen down.
    pen go: 100.
    "pen place: 250@250." "will not draw even if pen is down"
]
```

## Pen instance methods

`Pen` instances have three instance variables,
`location`, `direction`, and `penDown`.

The `location` value is a `Point` instance that specifies an x-y coordinate.
It can be changed by sending `#go:`, `#goto`,
`location:direction:penDown:`, or `place:`.
It can be retrieved by sending `#location`.

The `direction` value is an angle in degrees where
zero is right, 90 is down, 180 is left, and 270 is up.
It defaults to 270 and can be reset to that by sending `#north` to the `Pen`.
It can be changed by sending `#turn:` or `#location:direction:penDown:`.
It can be retrieved by sending `#direction`.

Highlights include:

| Method                        | Description                                                 |
| ----------------------------- | ----------------------------------------------------------- |
| `color:`                      |                                                             |
| `defaultNib:`                 |                                                             |
| `direction`                   | answers current direction                                   |
| `down`                        | places pen down so it can draw                              |
| `dragon:`                     |                                                             |
| `drawLoopX:Y:`                |                                                             |
| `filberts:side:`              |                                                             |
| `fill:color:`                 |                                                             |
| `go:`                         | moves argument distance and draws if pen is down            |
| `goto:`                       | moves to argument `Point` and draws if pen is down          |
| `hilbert:side:`               |                                                             |
| `hilberts:`                   |                                                             |
| `home`                        | sets `location` to center of window                         |
| `location`                    | answers current pen location                                |
| `location:direction:penDown:` | sets `location`, `direction`, and `penDown`                 |
| `mandala:`                    |                                                             |
| `north`                       | sets `direction` to 270; why no `south`, `west`, or `east`? |
| `place:`                      | moves to `Point` without drawing even if pen is down        |
| `print:withFont:`             |                                                             |
| `roundNib:`                   |                                                             |
| `sourceForm:`                 |                                                             |
| `spiral:angle:`               |                                                             |
| `squareNib:`                  |                                                             |
| `turn:`                       | adds argument in degrees to `direction`                     |
| `up`                          | raise pen up so it cannot draw                              |
| `web`                         |                                                             |
