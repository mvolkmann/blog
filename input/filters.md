---
eleventyNavigation:
  key: Filters
layout: layout.njk
title: Filters
---

See <https://www.11ty.dev/docs/filters/>.

Filters take a value and return a potentially different value.
When used with Liquid and Nunjucks,
they appear inside interpolations after a vertical bar.
For example:

{% raw %}

```liquid
<h3>{{ title | uppercase }}</h3>
```

{% endraw %}

Universal filters can be used with
Handlebars, JavaScript, Liquid, and Nunjucks.
The provided universal filters are
`get*CollectionItem`, `log`, `slug`, and `url`.
Note that the `get*CollectionItem` and `log` filters
are coming in version 0.11.0.

You can implemented custom filters in the 11ty configuration file,
typically `.eleventy.js`.
Here is a filter that doesn't do or change anything:

```js
eleventyConfig.addFilter('myFilter', value => {
  return value;
});
```

Here is a custom filter for sorting.
It expects its value to be an array of objects.
It takes an argument that specifies the property in the objects
that should be used for sorting.

```js
eleventyConfig.addFilter('sort', (value, property) => {
  value.sort((item1, item2) => item1[property].localeCompare(item2[property]));
  return value;
});
```

This can be used in conjunction with the navigation plugin.
For example:

{% raw %}

```liquid
{{ collections.all | eleventyNavigation | sort('title') | eleventyNavigationToHtml | safe }}
```

{% endraw %}
