---
eleventyNavigation:
  key: Data Cascade
  parent: Eleventy
layout: topic-layout.njk
---

Data in an 11ty project is held in variables.
Pages can access these variables.
Variables can be defined in multiple places, and
the "cascade" defines which definition takes precedence.

The places where variables can be defined,
from highest to lowest precedence, include:

- `{template-name}.{markup-extension}` in front matter
- front matter in layouts used by the template
- `{template-name}.11tydata.js` file in same directory
- `{template-name}.11tydata.json` file in same directory
- `{dir-name}.11tydata.js` file in same directory
- `{dir-name}.11tydata.json` file in same directory
- `{dir-name}.11tydata.js` files in ancestor directories
- `{dir-name}.11tydata.json` files in ancestor directories
- `{data-name}.js` file in the `_data` directory
- `{data-name}.json` file in the `_data` directory

JavaScript files can generate data and
retrieve it from REST services.
The use of the directory `_data` was inspired by Jekyll.

Data from a file whose name starts with `{template-name}.11tydata.`
is only available in template files in the same directory
whose name starts with `{template-name}.`.

Data from a file whose name starts with `{dir-name}.11tydata.`
is available in all template files in the same directory and below.

Directory files that are closer to a template file
take precedence over those higher in the directory hierarchy.
TODO: It seems layout front matter is given the lowest precedences,
TODO: but that does not match the docs.
TODO: See this issue you created at <https://github.com/11ty/eleventy/issues/915>.

Let's walk through a comprehensive example to demonstrate
all the possible sources of data for a template.
Consider the following directory structure.
Each of these files define an array of dogs.
Any one of them can be used as the source of this data.
The files are numbered to indicate their precedence in
providing data used in `dogs.md` with 1 being the highest.

- \_includes
  - layout.njk (10)
- \_data
  - dogs.js (8)
  - dogs.json (9)
- level1
  - level1.11tydata.js (6)
  - level1.11tydata.json (7)
  - level2
    - dogs.11tydata.js (2)
    - dogs.11tydata.json (3)
    - level2.11tydata.js (4)
    - level2.11tydata.json (5)
    - dogs.md (1)

Here is example content from `_data/dogs.js`.
It exports an array of dogs.

```js
module.exports = [
  {name: 'Dasher', breed: 'Whippet'},
  {name: 'Maisey', breed: 'Treeing Walker Coonhound'},
  {name: 'Ramsey', breed: 'Native American Indian Dog'},
  {name: 'Oscar', breed: 'German Shorthaired Pointer'}
];
```

Here is example content from `_data/dogs.json`.
It defines a value for the global variable `dogs`.

```json
[
  {"name": "Dasher", "breed": "Whippet"},
  {"name": "Maisey", "breed": "Treeing Walker Coonhound"},
  {"name": "Ramsey", "breed": "Native American Indian Dog"},
  {"name": "Oscar", "breed": "German Shorthaired Pointer"}
]
```

Here is example content from `level1/level1.11tydata.js`,
`level1/level2/level2.11tydata.js`, and `level1/level2/dogs.11tydata.js`.
They all export an object with a `dogs` property
whose value is an array of dogs.

```js
module.exports = {
  dogs: [
    {name: 'Dasher JS1', breed: 'Whippet'},
    {name: 'Maisey JS1', breed: 'Treeing Walker Coonhound'},
    {name: 'Ramsey JS1', breed: 'Native American Indian Dog'},
    {name: 'Oscar JS1', breed: 'German Shorthaired Pointer'}
  ]
};
```

Here is example content from `level1/level1.11tydata.json`,
`level1/level2/level2.11tydata.json`, and `level1/level2/dogs.11tydata.json`.
They all define an object with a `dogs` property
whose value is an array of dogs.

```json
[
  {"name": "Dasher", "breed": "Whippet"},
  {"name": "Maisey", "breed": "Treeing Walker Coonhound"},
  {"name": "Ramsey", "breed": "Native American Indian Dog"},
  {"name": "Oscar", "breed": "German Shorthaired Pointer"}
]
```

Here is the content of `level1/level2.dogs.md`.
It defines an array of dogs using YAML syntax.

```md
---
dogs:
  - name: Dasher
    breed: Whippet
  - name: Maisey
    breed: Treeing Walker Coonhound
  - name: Ramsey
    breed: Native American Indian Dog
  - name: Oscar
    breed: German Shorthaired Pointer
layout: layout.njk
---
```

# Dogs

{% raw %}

```liquid
{% for dog in dogs %}
  {{dog.name}} is a {{dog.breed}}.
{% endfor %}
```

{% endraw %}

The set of variables obtained from these sources is merged,
but their values are not.
For example, if a template defines `tags` to be `['dog', 'pet']`
and its layout defines `tags` to be `['canine', 'creature', 'pet']`
the result will be `['dog', 'pet']`.

As another example, consider a template with the following front matter:

```yaml
layout: myLayout.njk
title: 'Template Title'
```

Suppose `myLayout.njk` contains the following front matter:

```yaml
score: 7
title: 'Layout Title'
```

The data available in the template will be:

```js
{
  layout: myLayout.njk,
  score: 7,
  title: 'Template Title'
}
```
