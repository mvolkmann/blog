---
eleventyNavigation:
  key: Collection Sorting
layout: layout.njk
title: Collection Sorting
---

Collections are sorted on the creation date of their documents.
To iterate over a collection in a different order,
add code in `.eleventy.js` that adds
a new collection that is sorted in the desired way.

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

This can be used allow each of the templates in the `nav` collection
described above to specify its order in the list of nav links.
To do this, add a `navOrder` variable to the front matter of each of these templates.
For example:

```yaml
navOrder: 3
```

Add a collection in `.eleventy.js` that holds
all the items in the `nav` collection,
but sorted on the `navOrder` values.
For example:

```js
eleventyConfig.addCollection('orderedNav', collection => {
  // Get only the documents that have a tag of "dog".
  const navs = collection.getFilteredByTag('nav');
  navs.sort((nav1, nav2) => nav1.data.navOrder - nav2.data.navOrder);
  return navs;
});
```

Finally, change the layout that renders the nav links
to iterate over `collections.orderedNav` instead of `collections.nav`.
