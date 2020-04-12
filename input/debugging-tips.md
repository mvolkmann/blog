---
eleventyNavigation:
  key: Debugging Tips
layout: layout.njk
---

To see the value of a variable in the devtools console,
use the 11ty `log` filter which calls `console.log`.

For example:

{% raw %}

```liquid
{{ myVariable | log }}
```

{% endraw %}

To see the content of an array or object within a page,
use the Nunjucks `dump` filter which calls `JSON.stringify`.

For example:

{% raw %}

```liquid
{{ myVariable | dump }}
```

{% endraw %}
