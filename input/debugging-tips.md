---
layout: layout.njk
tags: navItem
title: Debugging Tips
---

To see the value of a variable in the devtools console,
use the 11ty `log` filter.
For example:

{% raw %}

```liquid
{{myVariable | log}}
```

{% endraw %}

To see the content of an array or object within a page,
use the Nunjucks `dump` filter which calls `JSON.stringify`.
For example, `<div>myVariable = {{myVariable | dump}}`.
