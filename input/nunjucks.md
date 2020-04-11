---
layout: layout.njk
tags: navItem
title: Nunjucks
---

Nunjucks is a template language implemented in JavaScript.
It supports features seen in most programming languages.

## VS Code extensions

The VS Code extension "Nunjucks" (ronnidc.nunjucks)
applies syntax highlighting in `.njk` files.

The VS Code extension "Nunjucks Snippets"
(luwenjiechn.nunjucks-vscode-snippets) defines Nunjucks snippets
to simplifying writing Nunjucks constructs in `.njk` files.

## Configuration

TODO: Cover other nunjucks-specific configuration.

## Data types

Nunjucks essential supports the same data types as JavaScript.
These include:

- Boolean: true, false
- Numbers: 2, 3.4
- Strings: "in double-quotes", 'in single-quotes'
- Arrays: [true, 2, 'test']
- Dicts: { key1: true, key2: 'text' }

## Operators

Nunjucks supports these mathematical operators:

- +, -, \*, /
- // (integer division)
- % (modulo)
- \*\* (exponentiation)

Nunjucks supports these comparison operators from JavaScript:

- ==, ===
- !=, !==
- <, <=, >=, >

Nunjucks supports these logical operators:

- and, or, not
- parentheses to group expressions

## Comments

The syntax for comments is:

{% raw %}

```njk
{# some comment #}
```

{% endraw %}

## Variables

To define or modify a variable:

{% raw %}

```njk
{% set name = value %}
```

{% endraw %}

Variables defined at the top-level are global.
Otherwise they are scoped to the construct in which they are defined.

## Rendering

To render an expression, enclose it in double-curly braces.
For example:

{% raw %}

```njk
{{ dog.name }}
```

{% endraw %}

## Filters

Nunjucks can render the result of passing a value through a filter.
There are many provided filters and custom filters can be implemented.
The provided filters are listed
[here](https://mozilla.github.io/nunjucks/templating.html#builtin-filters).
They include `abs`, `batch`, `capitalize`, `center`, `default`,
`dictsort`, `dump`, `escape`, `first`, `float`, `forceescape`,
`groupby`, `indent`, `int`, `join`, `last`, `length`, `list`,
`lower`, `nl2br`, `random`, `rejectattr`, `replace`, `reverse`,
`round`, `safe`, `selectattr`, `slice`, `sort`, `string`, `striptags`,
`sum`, `title`, `trim`, `truncate`, `upper`, `urlencode`, `urlize`,
and `wordcount`.

There are too many to describe, but here are some highlights:

- `dump` calls `JSON.stringify` on a value and is useful for debugging.
- `first` returns the first element of an array or the first character of a string.
- `last` returns the last element of an array or the last character of a string.
- `groupby` creates an array of arrays of objects from an array of objects
  based on a common property value.
- `join` concatenates an array of values, separated by a delimiter
- `nl2br` replaces newline characters with HTML `<br />` elements
- `random` returns a random element from an array
- `rejectattr` filters an array of objects, rejecting those where a given property passes a test
- `selectattr` filters an array of objects, keeping those where a given property passes a test
- `sort` sorts a data array. It is called as a function with three arguments.
  The first is a boolean indicating whether the sort should be in reverse order.
  The second is a boolean indicating whether the sort should be case insensitive.
  The third is the string name of the property on which the objects should be sorted.
  For example, {% raw %} `{% for team in hockey | sort(false, false, 'city') %}` {% endraw %}.
- `urlencode` encodes a URL using UTF-8.
- `urlize` turns URLs in a string into links.
  The string can contain any number of URLs.
  The result must be passed to the `safe` filter.
  Otherwise the resulting HTML is escaped and presented as plain text.
  For example, {{ 'before http://foo.bar after' | urlize | safe }}
  renders `before <a href="http://foo.bar">http://foo.bar</a> after`.

Here is an example of applying the `upper` filter to a string:

{% raw %}

```njk
{{ dog.name | upper }}
```

{% endraw %}

Another syntax is available for applying a filter
to a large amount of content.
For example:

{% raw %}

```njk
{% filter upper %}
  Mark is a software engineer at Object Computing, Inc.
{% endfilter %}
```

{% endraw %}

This form can only contain literal content, not other Nunjucks constructs.

Nunjucks also supports defining custom filters.
When using 11ty, they can be defined in the `.eleventy.js`.
For example, this filter writes the value of an expression
to the devtools console.

```js
eleventyConfig.addFilter('log', console.log);
```

TODO: Try this!

## Conditional logic

Content can be conditionally included using an `if` statement.
For example:

TODO: Change this example instead of copying from the official docs.

{% raw %}

```njk
{% if hungry %}
  I am hungry.
{% elif tired %}
  I am tired.
{% else %}
  I am good!
{% endif %}
```

{% endraw %}

Nunjucks supports using and `if` for an odd kind of ternary operator.
For example, this outputs "yellow" if `happy` is true.

{% raw %}

```njk
{{ "happy" if happy }}
```

{% endraw %}

This is similar, but outputs "gray" if `happy` is false.

{% raw %}

```njk
{{ "yellow" if happy else "gray" }}
```

{% endraw %}

## Iteration

A `for` loop iterates over the elements of an array
or the properties of an object.

If the variable `dogs` holds an array of objects,
we can iterate over the array as follows:

{% raw %}

```njk
{% for dog in dogs %}
  <p>{{ dog.name }} is a {{ dog.breed }}.</p>
{% else %}
  <p>Who let the dogs out?</p>
{% endfor %}
```

{% endraw %}

The current loop index is available in
`loop.index` (starts from 1) and `loop.index0` (starts from 0).

If the variable `dog` holds an object,
we can iterate over its properties as follows:

{% raw %}

```njk
{% for key, value in dog %}
  <p>{{key}} = {{value}}</p>
{% endfor %}
```

{% endraw %}

There is also support for asynchronous versions of this
(`asyncEach` and `asyncAll`) that most sites will not need.
It is documented
[here](https://mozilla.github.io/nunjucks/api.html#asynchronous-support).

## Function calls

JavaScript functions can be called using the same syntax as JavaScript.
For example, this calls a function and renders its result:

{% raw %}

```njk
{{ someFunction(arg1, arg2) }}
```

{% endraw %}

## Regular expressions

Regular expressions are defined by beginning with `r/`.
For example:

{% raw %}

```njk
{% set re = r/^a.*z$/i %}
{% if re.test(dog.name) %}
  This dog goes from a to z!
{% endif %}
```

{% endraw %}

The `i` option at the end of the regular expression
signifies case-insensitive matching.

## Sanitizing

When rendering HTML from a potentially untrusted source,
use the `safe` filter to sanitize it.
For example:

{% raw %}

```njk
{{ someHtml | save }}
```

{% endraw %}

## Includes

One template can include another.
This allows a template to be reused in many places.
The syntax is {% raw %} `{% include "`_`file-path`_`" %}` {% endraw %}.

For example:

{% raw %}

```njk
{% include "snippet.html" %}
```

{% endraw %}

This is useful when the content to be included
doesn't require any data to be supplied.
When data is required, consider using Nunjucks macros
which are described next.

## Macros

Macros are like functions that have parameters
and render something based on those.
Parameters can have default values that are used when a value is not provided.
For example:

{% raw %}

```njk
{% macro dogP(name, breed, gender='unknown') %}
<p>{{name}} is a {{gender}} {{breed}}.</p>
{% endmacro %}
```

{% endraw %}

A macro call looks like a function call.
For example:

{% raw %}

```njk
{% for dog in collections.dogsByName %}
  {{ dogP(dog.data.name, dog.data.breed, dog.data.gender) }}
{% endfor %}
```

{% endraw %}

Arguments can be specified positionally or by name.
For example:

{% raw %}

```njk
{% for dog in collections.dogsByName %}
  {{ dogP(gender=dog.data.gender, breed=dog.data.breed, name=dog.data.name) }}
{% endfor %}
```

{% endraw %}

TODO: Maybe Nunjucks macros can be used as an alternative to 11ty shortcodes.
TODO: Can they be defined globally?

## Blocks

Blocks in Nunjucks are like named slots in Svelte.
They allow a template to pass content into another template.
This is described [here](https://mozilla.github.io/nunjucks/templating.html#template-inheritance).

## Whitespace

{% raw %}
By default all whitespace is retained.
To trim whitespace before a construct, begin with `{%-` instead of `{%`.
To trim whitespace after a construct, end with `-%}` instead of `%}`.
Zach Leatherman recommends only using `{%-`.
{% endraw %}

## Global functions

Nunjucks provides some predefined functions
that address common needs.

- `range` is used to iterate over a range of integer values.
- `cycler` rotates through a set of values.
- `joiner` returns a function that outputs a given string
  each time it is called except for the first

## Other topics

I haven't described these topics yet:
call, extends, import, raw, super, verbatim
