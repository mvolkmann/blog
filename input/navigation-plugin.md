---
eleventyNavigation:
  key: Navigation Plugin
  parent: Plugins
layout: layout.njk
---

Eleventy provides a plugin that implements page navigation.
To install the navigation plugin, install it as follows:

```bash
npm install -D @11ty/eleventy-navigation
```

Then register it in `.eleventy.js` as follows:

```js
const navigationPlugin = require('@11ty/eleventy-navigation');

module.exports = eleventyConfig => {
  eleventyConfig.addPlugin(navigationPlugin);
};
```

Specify documents that should be included in the navigation
by adding the following front matter to each of them:

```yaml
---
eleventyNavigation:
  key: My Page Title
---

```

To use a different value for the rendered title than the key,
add a `title` property as follows:

```yaml
---
eleventyNavigation:
  key: myPageKey
  title: My Page Title
---

```

To use an ordering other than creation timestamp from oldest to newest,
add an `order` property to the documents as follows
where lower numbers come before higher:

```yaml
---
eleventyNavigation:
  key: My Page Title
  order: 7
---

```

If a document should appear as a child of another in the navigation,
add a `parent` property as follows:

```yaml
---
eleventyNavigation:
  key: My Page Title
  parent: myParentKey
---

```

To add a navigation link to a page that is not part of the Eleventy site,
create a document containing only front matter,
and add a `url` and `permalink` properties as follows.
Setting `permalink` to `false` prevents Eleventy from
creating a file for this in the output directory (typically `_site`).

```yaml
---
eleventyNavigation:
  key: Eleventy
  url: https://11ty.dev/
permalink: false
---

```

To render navigation links for the pages using Nunjucks,
add the following in a layout file:

{% raw %}

```liquid
{{
  collections.all |
  eleventyNavigation |
  eleventyNavigationToHtml |
  safe
}}
```

{% endraw %}

This looks for `eleventyNavigation` front matter in all documents.
To restrict this to a subset of the documents,
use a collection other than "all".

To get only the links that are descendants of a given parent key,
pass the parent key to the `eleventyNavigation` filter as follows:

{% raw %}

```liquid
{{
  collections.all |
  eleventyNavigation('myParentKey') |
  eleventyNavigationToHtml |
  safe
}}
```

{% endraw %}

To render breadcrumbs to the key of the currently selected document:

{% raw %}

```liquid
{% set crumbs = collections.all | eleventyNavigationBreadcrumb(eleventyNavigation.key) %}
{% for crumb in crumbs %}
  <a class="crumb" href="{{ crumb.url | url }}">{{ crumb.title }}</a>
  ...
{% endfor %}
```

{% endraw %}

To customize how the links are rendered,
use custom markup instead of `eleventyNavigationHtml`.
This can associated custom CSS classes with rendered elements
to support custom styling.
For example, we can style the link for the currently selected page
to be a different color than the others:

{% raw %}

```liquid
{% set navPages = collections.all | eleventyNavigation %}
<ul>
{% for entry in navPages %}
  <li{% if entry.url == page.url %} class="active"{% endif %}>
    <a href="{{ entry.url | url }}">{{ entry.title }}</a>
  </li>
{% endfor %}
</ul>
```

{% endraw %}

To add descriptive text to links, add the `excerpt` property
in document front matter.
To render this when using `eleventyNavigationToHtml`
pass the `showExcerpt` option as follows:

{% raw %}

```liquid
{{
  collections.all |
  eleventyNavigation |
  eleventyNavigationToHtml({ showExcerpt: true }) |
  safe
}}
```

This adds colon and the excerpt text after
each navigation link that has an `excerpt` value.

To display `excerpt` values in a different way,
use a `for` loop to iterate over the navigation entry
and render `entry.excerpt` any way you like.

{% endraw %}
For more details, see
[Navigation Plugin](https://www.11ty.dev/docs/plugins/navigation).
