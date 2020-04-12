---
eleventyNavigation:
  key: Collections
layout: layout.njk
---

Collections of documents are defined by specifying tags in front matter.
They are represented by an array of objects
that describe everything about a document.
This includes front-matter data, document content,
and properties added by 11ty.
The order of these objects within the array matches
the order of the document creation dates from oldest to newest.

For example, documents that each describe a specific dog
can have front matter similar to the following.

```yaml
tags: dog
name: Dasher
breed: whippet
```

Multiple tags can be specified by listing them
in square brackets separated by commas.
This allows a document to be in more than one collection.
For example, `tags: [dog, pet]`.
Quotes are only required around tag names that contain special characters.

All templates can access `collections.dog` to
iterate over all the documents with a tag of "dog".

For example:

{% raw %}

```liquid
{%- for dog in collections.dog -%}
  <p>{{ dog.data.name}} is a {{dog.data.breed}}.</p>
{%- endfor -%}
```

{% endraw %}

Note the use of `.data` to access the front matter variables
specified for a dog.

A layout can specify tags that become the default tags
for all pages that use the layout.
These can be overridden by specifying a different value for `tags` in a page.

Page-related properties that are added to collection objects by 11ty include:

- `data`: an object that holds the front matter variables
- `date`: the date and time at which the template file was created, not modified
- `fileSlug`: the part of the page URL that uniquely identifies it
- `filePathStem`: the file path in the page URL that uniquely identifies it
- `inputPath`: the relative file path to the source template file
- `outputPath`: the path to the output HTML file relative to the top of the project
- `template`: an object that holds lots of 11ty-specific data
- `templateContent`: the string of HTML produced from the template
- `url`: the page URL

Storing the front matter variables in the `data` object
avoids name collisions with other page-related properties.

A good way to implement page navigation in an 11ty site is to use
the [navigation plugin](/blog/navigation-plugin).
Another way is to add a tag to each template that represents
a document that should be reachable from a nav link.
Also add a `title` variable to each of these templates.
For example, we can add the following front matter in `about.md`:

```yaml
tags: nav
title: About
```

Then generate navigation links in a layout as follows:

{% raw %}

```liquid
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
of the associated document, from oldest to newest.
To iterate in reverse order use the `reverse` filter as follows:

{% raw %}

```liquid
{% for nav in collections.nav | reverse %}
```

{% endraw %}

To iterate over a collection in an order
that is not based on document creation timestamps,
add code in the Eleventy configuration file (typically `.eleventy.js`)
that adds a new collection that is sorted in the desired way.

For example:

```js
eleventyConfig.addCollection('dogsByName', collection => {
  // Get only the documents that have a tag of "dog".
  const dogs = collection.getFilteredByTag('dog');
  // Sort the dogs on their name.
  dogs.sort((dog1, dog2) => dog1.data.name.localeCompare(dog2.data.name));
  return dogs;
});
```

Use the collection `dogsByName` in a `for` loop
to iterate in the new sorted order.
For more information on 11ty collections,
see [collections](https://www.11ty.dev/docs/collections/).
