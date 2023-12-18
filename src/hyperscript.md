---
eleventyNavigation:
  key: Hyperscript
layout: topic-layout.njk
---

<figure style="width: 80%">
  <img alt="_hyperscript logo" style="border: 0"
    src="/blog/assets/hyperscript-logo.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://hyperscript.org", "_hyperscript" %} is
small programming language that can be used in HTML files
to implement interactive features such as event handling.
It also supports asynchronous operations such as
fetching data from a server by sending an HTTP request.

\_hyperscript is based on the
{% aTargetBlank "https://en.wikipedia.org/wiki/HyperTalk", "HyperTalk" %}
language which was used in Apple's
{% aTargetBlank "https://en.wikipedia.org/wiki/HyperCard", "HyperCard" %}.
Like HyperTalk, \_hypertalk uses an English-like syntax.

\_hyperscript is similar to {% aTargetBlank "https://alpinejs.dev", "Alpine" %}
and {% aTargetBlank "https://htmx.org", "HTMX" %}
in that they add attributes to HTML.
But hyperscript only adds one attribute, the underscore.

\_hyperscript was created by Carson Gross who also created
{% aTargetBlank "https://htmx.org", "HTMX" %}.

## Installing

To use \_hyperscript, just including the following `script` tag
in each HTML page that needs it.

```html
<script src="https://unpkg.com/hyperscript.org@0.9.12"></script>
```

## Using

To use \_hyperscript, add an attribute to HTML elements
whose name is an underscore.
For example:

```html
<html lang="en">
  <head>
    <script src="https://unpkg.com/hyperscript.org@0.9.12"></script>
  </head>
  <body>
    <button _="on click alert('got click')">Click Me</button>
  </body>
</html>
```

## TODO

Organize this content:

can combine with tailwind in order to use provided classes like hidden.
how good are the error messages if there is something wrong in the syntax?
Do error messages appear in the Dev tools console?
Do error messages appear on page load or only when hyperscript is triggered?
see the control flow section in the docs for conditional logic and iteration.
maybe Alpine is better for this because it allows using JavaScript syntax.
The then keyword allows specifying a sequence of hyperscript statements.
what is the difference between if else and if otherwise?

on click toggle .some-class on selector
click is an event.
toggle is an action

List all the supported events. click, load, …
List all the supported keywords and describe each one. toggle, transition, on, my, over to, seconds, then, …
is there a keyword that runs two things at once unlike the then keyword that runs them serially?
selectors can use the keyword me.

make sure you understand what the defer attribute does on a script tag.
resetting a form can be done in a single attribute with hyper script, but requires three things with Alpine, x-data, x-on, and an id on the form.
