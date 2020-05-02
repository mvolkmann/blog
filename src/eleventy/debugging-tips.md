---
eleventyNavigation:
  key: Debugging Tips
  parent: Eleventy
layout: topic-layout.njk
---

To see the value of a variable in the devtools console,
use the 11ty `log` filter which calls `console.log`.
This writes to the terminal window where the Eleventy server is running,
not to the DevTools console.

For example:

{% raw %}

```liquid
{% set temp = myVariable | log %}
```

TODO: Do you have to use an assignment in order to
TODO: avoid rendering something to the browser?

{% endraw %}

To render the content of an array or object within a page,
use the Nunjucks `dump` filter which calls `JSON.stringify`.

For example:

{% raw %}

```liquid
{{ myVariable | dump }}
```

{% endraw %}
