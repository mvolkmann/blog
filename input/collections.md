---
layout: layout.njk
tags: navItem
title: Collections
---

Collections are defined by specifying tags in front matter.
They are represented by an array of objects representing pages
whose properties are the non-tag front matter values
and some page-related properties added by 11ty.
By default their order is based on creation date from oldest to newest.

For example, documents that each describe a specific dog
can have front matter similar to the following.

```yaml
tags: dog,
name: dasher
breed: whippet
```

Multiple tags can be specified by listing them
in square brackets separated by commas.
This allows the data from a template to be in more than one collection.
For example, `tags: [dog, pet]`.
Quotes are only required around tag names that contain special characters.

All templates can access `collections.dog` to
iterate over all the data related to dogs.

For example:

{% raw %}

```njk
{%- for dog in collections.dog -%}
  <p>{{ dog.data.name}} is a {{dog.data.breed}}.</p>
{%- endfor -%}
```

{% endraw %}

Note the use of `.data` to access the variables specified for a dog.

A layout can specify tags that become the default tags
for all pages that use the layout.
These can be overridden by specifying a different value for `tags` in a page.

Page-related properties that are added to collection objects by 11ty include:

- data: an object that holds the front matter variables
- date: the date and time at which the template file was created, not modified
- fileSlug: the part of the page URL that uniquely identifies it
- filePathStem: the file path in the page URL that uniquely identifies it
- inputPath: relative file path to the source template file
- outputPath: path to the output HTML file relative to the top of the project
- template: an object that holds lots of 11ty-specific data
- templateContent: the string of HTML produced from the template
- url: the page URL

Storing the front matter variables in the `data` object
avoids name collisions with other page-related properties.

This information can be used to generate navigation links
in the main layout.
To do this, add a tag to each template that represents
a page that should be reachable from a nav link.
Also add a `title` variable to each of these templates.
For example, we can add the following front matter in `about.md`:

```yaml
tags: nav
title: About
```

Then generate navigation links in the main layout as follows:

{% raw %}

```njk
<nav>
  <ol>
    {% for nav in collections.nav %}
      <li>
        <a href="{{nav.url}}">{{nav.data.title}}</a></li>
      </li>
    {% endfor %}
  </ol>
</nav>
```

{% endraw %}

The order of items in each collection is based on the creation timestamp
of the associated template file, from oldest to newest.
To iterate in reverse order use the `reverse` filter as follows:

```njk
{# This is commented out to prevent Nunjucks processing.
{% for nav in collections.nav | reverse %}
#}
```

For more information on 11ty collections,
see [collections](https://www.11ty.dev/docs/collections/).
