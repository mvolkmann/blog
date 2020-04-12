---
eleventyNavigation:
  key: Global Data
layout: layout.njk
title: Global Data
---

Global data can be defined in `.js` or `.json` files in the `_data` directory.
When defined in a `.js` file, it should export a function that returns the data.
For example, the file `_data/hockey.js` could contain:

```js
module.exports = () => [
  {city: 'Chicago', name: 'Blackhawks'},
  {city: 'St. Louis', name: 'Blues'}
];
```

TODO: Can the JavaScript code call a REST service to get the data?

To render this data in a template:

{% raw %}

```liquid
{%- for team in hockey -%}
  <p>The {{team.name}} play in {{team.city}}.</p>
{%- endfor -%}
```

{% endraw %}
