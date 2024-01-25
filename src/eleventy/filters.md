---
eleventyNavigation:
  key: Filters
  parent: Eleventy
layout: topic-layout.njk
tags: eleventy
---

Filters take a value and return a potentially different value.
When used with Liquid and Nunjucks,
they appear inside interpolations after a vertical bar.
For example:

{% raw %}

```liquid
<h3>{{ title | uppercase }}</h3>
```

{% endraw %}

Universal filters can be used with multiple template engines,
including Handlebars, JavaScript, Liquid, and Nunjucks.
The provided universal filters are
`get*CollectionItem`, `log`, `slug`, and `url`.
The "*" above can be replaced by "Previous", "Next",
or nothing to get the current page from a collection.
Note that the `get*CollectionItem`and`log` filters
are coming in version 0.11.0.

Nunjucks provides the following filters documented at
{% aTargetBlank
  'https://mozilla.github.io/nunjucks/templating.html#built-in-filters',
  'built-in-filters'
%}:
`abs`, `batch`, `capitalize`, `center`, `default`, `dictsort`,
`dump`, `escape`, `first`, `float`, `forceescape`, `groupby`,
`indent`, `int`, `join`, `last`, `length`, `list`, `lower`,
`nl2br`, `random`, `rejectattr`, `replace`, `reverse`, `round`,
`safe`, `selectattr`, `slice`, `sort`, `string`, `sum`, `title`,
`trim`, `truncate`, `upper`, `urlencode`, `urlize`, and `wordcount`.

The Nunjucks `sort` filter requires three arguments.
The first is boolean indicating whether the sort should be in reverse order.
The second is a boolean indicating whether the comparisons should be case-sensitive.
The third is the property on which to sort.
It can only sort on top-level properties. See
{% aTargetBlank 'https://github.com/11ty/eleventy/issues/911', 'issue 911' %}.

You can implement custom filters in the 11ty configuration file,
typically `.eleventy.js`.
Here is a filter that doesn't do or change anything:

```js
eleventyConfig.addFilter('myFilter', value => {
  return value;
});
```

Here is a custom filter for sorting that is
an alternative to using the one provided by Nunjucks.
It expects its value to be an array of objects.
It takes an argument that specifies the property in the objects
that should be used for sorting.

```js
eleventyConfig.addFilter('mySort', (value, property) => {
  value.sort((item1, item2) => item1[property].localeCompare(item2[property]));
  return value;
});
```

To sort on a property that isn't at the top of the items,
consider using the lodash
{% aTargetBlank 'https://lodash.com/docs/4.17.15#get', 'get function' %}
to get the values to be compared.

More more information, see
{% aTargetBlank 'https://www.11ty.dev/docs/filters/', 'Filters' %}.
