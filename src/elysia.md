---
eleventyNavigation:
  key: Elysia
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="Elysia logo" style="border: none; width: 20%"
  src="/blog/assets/elysia-logo.png?v={{pkg.version}}"
  title="Elysia logo">

## Overview

{% aTargetBlank "https://elysiajs.com", "Elysia" %}
is a JavaScript HTTP server library that targets the Bun JavaScript runtime.

{% aTargetBlank "https://elysiajs.com", "Elysia" %} is a competitor to
<a href="/blog/topics/#/hono" target="_blank">Hono</a>.
It has slightly better performance than Hono, but only runs in Bun.
Both Elysia and Hono are significantly faster that Express.

This notes are currently minimal.

See the example Elysia project at
<a href="https://github.com/mvolkmann/elysia-demos"
target="_blank">elysia-demos</a>.

## Context

The <a href="https://elysiajs.com/essential/context.html"
target="_blank">Context</a> object that is passed to routes
has many properties and methods.

The most useful methods on the `Context` object
related to extracting data from a request
are described in the following table.
This assumes that the variable `c` holds a `Context` object
that was passed to a route function.
Typically this is destructured in the argument list.

| Action                       | Code                                |
| ---------------------------- | ----------------------------------- |
| get value of request header  | `c.headers['some-name']`            |
| get value of path parameter  | `c.params['some-name']`             |
| get value of query parameter | `c.query['some-name']`              |
| get value of text body       | `const text = c.body;`              |
| get form data from body      | `const formData = c.body;`          |
| get property from `formData` | `const value = formData.some-name;` |
| get value of JSON body       | `const object = c.body;`            |

Elysia determines how to parse request bodies based on
the value of the `Content-Type` request header.
See <a src="https://elysiajs.com/life-cycle/parse#explicit-body"
target="_blank">Explicit Body</a>.

The most useful methods on the `Context` object
related to creating a response
are described in the following table.

| Action                       | Code                                    |
| ---------------------------- | --------------------------------------- |
| set value of response header | `c.header('Some-Name', 'some value');`  |
| set status code              | `c.status(someCode);`                   |
| return text response         | `return 'some text';`                   |
| return JSON response         | `return someObject;`                    |
| return HTML response         | `return someJSX;`                       |
| return "Not Found" error     | `set.status = 404; return 'Not Found';` |
| redirect to another URL      | `set.redirect = 'someURL'`              |

Returning JSX that is converted to HTML requires the following setup:

1. `bun add @elysiajs/html`
1. Import with `import {html} from '@elysiajs/html';`
1. Configure with `app.use(html());`
