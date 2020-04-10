---
layout: layout.njk
title: Front Matter
---

Files processed by 11ty can begin with front matter.
This starts with a line containing only three dashes
and ends with the same kind of line.
Lines between these define variables.
Variables can be used other files and can represent many kinds of values
including CSS property values, icons, URLs, ...

By default this expects YAML syntax.
For example:

```yaml
---
layout: layout.njk
title: Dogs
---

```

To use JavaScript syntax instead, add `js` after the opening dashes.
For example:

```js
---js
{
  layout: 'layout.njk',
  title: 'Dogs'
}
---
```

Some special variables recognized by 11ty include:

- layout:
  uses a layout template found in the `_includes` directory
- tags:
  single string or array of collection names to which this data belongs
- date: overrides the default file creation date to change sort order
- pagination:
  enables iteration over data in front matter to
  output multiple HTML files from this template
- permalink:
  changes the output target of the template
- dynamicPermalink:
  enables or disables template syntax in permalink values (default is true)
- templateEngineOverride:
  overrides the template engine used for this file
- eleventyExcludeFromCollections:
  true to exclude this content from collections

TODO: Are there any Nunjucks-specific variables?
TODO: See your front matter question at
TODO: https://github.com/11ty/eleventy/issues/916#issuecomment-583764086.
