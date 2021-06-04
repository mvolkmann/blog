---
eleventyNavigation:
  key: Web Development Tips
layout: topic-layout.njk
---

This is a collection of tips related to web development
divided into categories.
It assumes that you already know HTML, CSS, and JavaScript
to some extent, but perhaps have not encountered
all the tips shared here.
This is very much a work in progress!

## HTML

### <input> type attribute

HTML5 introduced many "semantic" elements.
These better describe the intent of certain kinds of markup
than using a more generic element such as `div`.
They also provide more context to assistive technologies
such as screen readers.
Some of the most commonly used semantic elements are
`article`, `aside`, `details`, `figcaption`, `figure`, `footer`, `header`,
`main`, `mark`, `nav`, `section`, `summary`, and `time`.

In most cases the paragraph element `<p>` should be used instead of `<div>`
when the content is only text to be rendered.

In most cases `click` events should only be associated
with `<button>` elements, not with generic elements like `<div>`.

HTML5 added by values for the `input` element `type` attribute.
These can change the way the element is rendered
and provide additional input validation.
The values include `checkbox`, `color`, `date`, `datetime-local`,
`email`, `file`, `image`, `month`, `number`, `password`, `radio`,
`range`, `search`, `tel`, `text`, `time`, `url`, `week`,
and a few more less commonly used values.

HTML provides good form validation with nice error messages.
Consider using this before reaching for a form library
that is specific to a given web framework.
Often what works well is to:

- Enclose all the form elements such as
  `button`, `input`, `textarea`, and `select` in a `form` element.
- Include only one "submit" button.
  This is the default type of a `button` element.
  To include `button` elements to do not submit the form,
  set their `type` attribute to `"button"`.
- Mark required form elements with the `require` attribute.
- Use the appropriate `input` `type` attribute values
  such as `"email"` and `"tel"`.
- Use other `input` attributes such as:

  - `min` and `max` for allowed numeric ranges
  - `minlength` and `maxlength` for allowed text lengths
  - `pattern` for regular expressions to be matched

Use CSS pseudo-classes to style form elements based on their validity.
These include `:required`, `:optional`, `:valid`, `:invalid`, `:user-invalid`,
`:blank`, `:placeholder-shown`, `:in-range`, `:out-of-range`,

The `input` element fires many events, two of which are `change` and `input`.
A `change` event is fired when a user changes the value
AND focus leaves the element.
An `input` event is fired after every change to the value.
For example, if a user types "abc" into an `input`,
an `input` event will be fired after each character
whereas a `change` event will only be fired once
after focus leaves the `input`.

The value displayed in an `input` element is specified by an attribute
that is determined by the value of its `type` attribute.
For most `input` types the `value` attribute is used for this purpose.
But when the `type` is `"checkbox"` or `"radio"`,
the value is specified using the `checked` attribute.

An `input` element can have an associated `datalist`.
This cause the `input` to act like an "auto-complete".
TODO: Provide an example.
TODO: Can the datalist be dynamic?

## SVG

SVG is a great markup language for drawing and rendering text and images.
The basics steps to use SVG are:

- Start with `<svg viewBox="0 0 maxX maxY" xmlns="http://www.w3.org/2000/svg">`
  where `maxX` and `maxY` are the maximum values in those dimensions.
  The minimum values do not have to be zero, but those are common values.
- Include child elements for rendering specific kinds of things.
  Commonly used elements include:

  - `circle`
  - `ellipse`
  - `image`
  - `line`
  - `path`
  - `polygon`
  - `polyline`
  - `rect`
  - `text` and `tspan`

- Wrap a `g` element around groups of children
  that need to be positioned or manipulated as a group.
- End with `</svg>`.
  basics

The size of an SVG element can be specified
using the CSS `width` and `height` properties.

The color of lines, paths, and shapes can be specified
using the CSS `stroke` and `fill` properties.

## CSS

selectors
pseudo classes
pseudo selectors
carbonators
variables

flex box commonly used properties

grid layout basics

using rems to enable scaling fonts across site

CSS position with relative an absolute return

window.matchMedia can be used to support light and dark modes.

box-sizing: border-box

100% versus 100 VH or 100 VW

position fixed and position sticky

using normalize reset

CSS box model basics

CSS specificity

ways to center things including using absolute positioning and translate -50%

## JavaScript

Review of array methods including reduce

Accessing CSS variables from JavaScript

## Node

nr alias for "npm run"

npm scripts

## Browsers

Create bookmarks in the bookmark bar to `http://localhost:{port-number}`
for ports that are commonly used by web frameworks.
Common ports include 3000, 5000, and 8080.
These make it easy to test web apps that being developed and run locally.

## Editors

Emmett basics

## Miscellaneous

configuring ESLint

configuring Prettier

kill listening process script
