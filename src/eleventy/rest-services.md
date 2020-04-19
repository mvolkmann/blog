---
eleventyNavigation:
  key: REST Services
  parent: Eleventy
layout: layout.njk
tags: eleventy
---

One way to make data from REST services available to templates
is to add `.js` files under the `_data` directory
whose names are the name of a global variable to set.

For example, the global variable `employees` can be set
by creating the file `_data/employees.js`
that uses the Fetch API to call a REST service.
To use the Fetch API, `npm install -d node-fetch`.

This file could contain the following:

```js
const fetch = require('node-fetch');

module.exports = async () => {
  const url = 'https://dummy.restapiexample.com/api/v1/employees';
  const res = await fetch(url);
  const response = await res.json();
  // This REST service returns an object with the properties
  // success and data where data is an array of employee objects.
  return response.data;
};
```

The employees can be rendered on a page like this:

{% raw %}

```liquid
{% for employee in employees %}
  <p>{{ employee.employee_name}} is {{employee.employee_age}} years old.</p>
{% endfor %}
```

{% endraw %}

We can also make data from a REST service available to
only a single template or only templates in and below a given directory.
For example, suppose the file `company/employees.md`
needs access to the employee data.
We can create the file `company/employees.11tydata.js`
or `company/company.11tydata.js` containing the following:

```js
const fetch = require('node-fetch');

module.exports = async () => {
  const url = 'https://dummy.restapiexample.com/api/v1/employees';
  const res = await fetch(url);
  const response = await res.json();
  // Any number of front matter variables
  // can be included in the returned object.
  return {employees: response.data};
};
```
