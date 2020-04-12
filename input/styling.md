---
eleventyNavigation:
  key: Styling
layout: layout.njk
title: Styling
---

To add CSS styling, configure the build process as shown above
and create `.css` files in the `assets/css` directory.

For example, to style nav links:

```css
nav {
  background-color: cornflowerblue;
  color: white;
}

li {
  margin-right: 1rem;
}

ul {
  display: flex;
  list-style: none;
  padding: 0.5rem;
}
```

One way to use the `.css` files is to add `link` tags in layout files.
Each layout file can include different `.css` files.
For example:

```html
<link rel="stylesheet" href="/assets/css/layout.css" />
<link rel="stylesheet" href="/assets/css/about.css" />
```

Another approach is the set a variable in template files
whose value is the path to a CSS file.
The variable can be used in a layout to include a `link` tag to the CSS file.
This allows each page template to include a different CSS file.

For example, a template can have this front matter:

```yaml
css: '/assets/css/about.css'
layout: layout.njk
```

A layout can use the variable like this:

{% raw %}

```liquid
{% if css %}
  <link rel="stylesheet" href="{{css}}" />
{% endif %}
```

{% endraw %}

There is no need to "scope" CSS to specific pages since each page
is built separately and loaded separately into the browser.
