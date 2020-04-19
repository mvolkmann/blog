---
eleventyNavigation:
  key: Permalinks
  parent: Eleventy
layout: layout.njk
tags: eleventy
---

11ty permalinks specified in front matter change
the path of the output files produced within the `_site` directory.
They are useful when the source directory structure
does not match the desired output directory structure.

When permalinks are specified,
any links to those pages must be updated to match.
Previously generated files under `_site` are not removed
after permalinks are added or modified,
so delete the `_site` directory and build again.

For example, the file `dogs.njk` would normally
generate `_site/dogs/index.html`.
To change this to `_site/animals/canines/index.html`,
add the following front matter in `dogs.njk`:

```yaml
permalink: 'animals/canines/'
```

The trailing slash is required.
Including `index.html` is optional,
but that is the file that is actually created.

An error is reported if multiple files target the same permalink path.

Permalink values can include variable references.
For example:

{% raw %}

```yaml
permalink: 'animals/{{species | slug}}'
```

{% endraw %}

Quotes must surround values that include variable references.
Using the `slug` filter encodes the value to ensure a valid URL.
This filter is provided by 11ty, not Nunjucks.

Permalinks can be used in conjunction with pagination
to change the paths to the files produced for each page.
This is described in the next section.
