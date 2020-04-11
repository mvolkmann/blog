---
layout: layout.njk
tags: navItem
title: Pagination
---

Pagination is the 11ty term for producing multiple files from one input file.
It operates on an array of data.
The `size` variable specifies the number of
array elements to be rendered on each page.
Often it is set to 1 to render each element on a separate page.
The array data can come from anywhere in the data cascade.

By default the URL for each page produced ends with a slash and a page number.

For example:

```yaml
pagination:
  data: employees # data over which to paginate
  size: 7 # number of employees per page
```

The global variable `pagination` holds related data.

- `pagination.data` holds the value of the
  pagination `data` property from the front matter.
- `pagination.pages` holds an array of items for each page.
  When the size is one the array holds each page item.
  When the size is greater than one it holds arrays of the items on each page.
  When `data` is an object rather than an array, it holds keys instead of items.
- `pagination.items` is an array of the objects
  to be rendered on the current page.
- `pagination.pages.length` is the number of pages.
- `pagination.pageNumber` holds the zero-based index of the current page.
- `pagination.firstPageLink` holds the URL of the first page (alias `first`).
- `pagination.firstPageHref` is the same, but omits `index.html` at end.
- `pagination.previousPageLink` holds the URL of the previous page (alias `previous`).
- `pagination.previousPageHref` is the same, but omits `index.html` at end.
- `pagination.nextPageLink` holds the URL of the next page; (alias `next`).
- `pagination.nextPageHref` is the same, but omits `index.html` at end.
- `pagination.lastPageLink` holds the URL of the last page (alias `last`).
- `pagination.lastPageHref` is the same, but omits `index.html` at end.
- `pagination.pageLinks` holds an array of all the page links (alias `links`).
- `pagination.href` holds an object with keys
  "previous", "next", "first", and "last" whose values are those hrefs.
- `pagination.hrefs` holds an array of all the page hrefs.

The URL for each page is `/{pagination.data}/{zero-based-index}`.
The URL for the first page page is simply `/{data-name}`.
The URL `/{data-name}/0` is not valid.

`pagination.items` holds the current page item when the size is 1
or an array of the current page items when size is greater than 1.
To add an alias for this add an `alias` property
to the `pagination` front matter.
For example:

```yaml
pagination:
  ...
  alias: items
```

With this alias in place, we can iterate over `items`
instead of `pagination.items` to render all the items on the current page.

To display "Page m of n":

```njk
<div>
  Page {{pagination.pageNumber + 1}} of {{pagination.pages.length}}
</div>
```

To render links to the first, previous, next, and last pages
so each is only rendered when it makes sense:

```njk
<nav class="pagination">
  <ol>
    {% if pagination.pageNumber > 0 %}
      <li><a href="{{pagination.href.first}}">First</a></li>
    {% endif %}
    {% if pagination.href.previous %}
      <li><a href="{{pagination.href.previous}}">Prev</a></li>
    {% endif %}
    {% if pagination.href.next %}
      <li><a href="{{pagination.href.next}}">Next</a></li>
    {% endif %}
    {% if pagination.pageNumber < pagination.pages.length - 1 %}
      <li><a href="{{pagination.href.last}}">Last</a></li>
    {% endif %}
  </ol>
</nav>
```

To render links to all the pages except the current by number:

```njk
<nav class="pagination">
  <ol>
    {% for href in pagination.hrefs %}
      {%if loop.index0 !== pagination.pageNumber %}
        <li><a href="{{href}}">Page {{ loop.index }}</a></li>
      {% endif %}
    {% endfor %}
  </ol>
</nav>
```

Note that Nunjucks provides the variable `loop` that holds
an object with the properties `index` (1-based) and `index0` (0-based).
These hold the current loop index.

To style the anchor tags as buttons:

```css
.pagination {
  background-color: transparent;
}

.pagination ol {
  display: flex;
  list-style: none;
  padding: 0.5rem 0; /* to match "a" padding below */
}

.pagination a {
  border: solid gray 1px;
  border-radius: 0.5rem;
  padding: 0.5rem;
  text-decoration: none;
}
```

To style the anchor for the current page differently that the others,
define a CSS rule for `a.current` and
set the `class` on the `a` elements as follows:

```njk
<a
  class="{{'current' if loop.index0 === pagination.pageNumber else ''}}"
  href="{{href}}"
>
```

To paginate data in reverse order,
add the `reverse` property to the `pagination` front matter.

```yaml
pagination:
  ...
  reverse: true
```

When `data` is an object rather than an array,
the items will be keys instead of items.
To change this so the items are the property values,
add the `resolve` property to the `pagination` front matter.

```yaml
pagination:
  ...
  resolve: values
```

To produce different data to be paginated from existing data,
use the `before` front matter property whose value is
a JavaScript function that takes the data and returns new data.
This can create new data to paginate by filtering, sort, and transforming.
Using this requires all the front matter
to be converted from YAML to JavaScript.
For example, the following filters the employees
so only those age 50 and older are retained
and it makes their names all uppercase:

```yaml
---js
{
  layout: 'employees-layout.njk',
  title: 'Employees',
  pagination: {
    data: 'employees',
    size: 7,
    alias: 'items',
    before(employees) {
      return employees.
        filter(emp => emp.employee_age >= 50).
        map(emp => ({
          ...emp,
          employee_name: emp.employee_name.toUpperCase()
        }));
    }
  }
}
---
```

To change the paths of the files produced for each page, specify a permalink.
For example, if the file `employees.njk` uses pagination then
it would normally generate files like `_site/employees/2/index.html`
where 2 is a page number.
To change this so it generates files like
`_site/workers/group-2/index.html`, add the following front matter:

```yaml
permalink: 'workers/group-{{pagination.pageNumber}}/index.html'
```

When a template uses pagination and gets data from a REST service
as shown above, the REST service is invoked once,
then again for each page.
So if pagination results in three pages,
there will be four calls to the REST service.
TODO: Why is this?
TODO: See the issue you created about pagination and .11ty.js files
TODO: at https://github.com/11ty/eleventy/issues/919.
