---
layout: layout.njk
title: Shortcodes
---

Shortcodes define snippets of reusable content.
This is the closest thing 11ty provides to components
in web frameworks like React, Vue, Angular, and Svelte.
For example, here is a "dog" shortcode definition that is
defined in `.eleventy.js` to make it globally available.
When defined this way they are referred to as "universal shortcodes".

```js
eleventyConfig.addShortcode(
  'dog',
  (breed, name) => `
    <div class="dog">
      <div class="name">Name: ${name}</div>
      <div class="breed">Breed: ${breed}</div>
    </div>
  `
);
```

To use this shortcode:

{% raw %}

```njk
{% for dog in collections.dogsByName %}
  {{ dog dog.data.breed, dog.data.name }}
{% endfor %}
```

{% endraw %}

Shortcodes can be defined in any `.js` file.
For example, to define a shortcode that is only available
in and below the `company` directory,
define it in the file `company/company.11tydata.js`
as follows:

```js
module.exports = function ({firstName, lastName}) {
  return `<h1>${this.user(firstName, lastName)}</h1>`;
};
```

Shortcodes return a string of HTML that is often constructed
using a JavaScript template literal.
A downside of this approach is that it limits
the ability of tooling to detect syntax errors in the HTML.

TODO: Cover non-universal shortcodes?
TODO: Are these just plain JS functions that return a string of HTML?

TODO: Cover "paired shortcodes"?

If a snippet doesn't require any data to be supplied, a Nunjucks
[include](https://mozilla.github.io/nunjucks/templating.html#include)
can used as an alternative.

Another alternative is to use Nunjucks
[macros](https://mozilla.github.io/nunjucks/templating.html#macro).

Unlike web framework components, 11ty "components"
don’t have runtime behavior.
This makes them far simpler to implement.
All they do is render something based on data passed to them.

Two options to consider for implementing 11ty "components" are
JavaScript-focused shortcodes and markup focused Nunjucks macros.
TODO: See your svg.njk example in "my-project" and include that here.
TODO: Also see the "circle" shortcode defined in .eleventy.js.
