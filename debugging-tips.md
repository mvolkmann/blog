---
layout: layout.njk
title: Debugging Tips
---

To see the value of a variable in the devtools console,
use the 11ty `log` filter.
For example:

```njk
{# This is commented out to prevent Nunjucks processing.
{{myVariable | log}}
#}
```

To see the content of an array or object within a page,
use the Nunjucks `dump` filter which calls `JSON.stringify`.
For example, `<div>myVariable = {{myVariable | dump}}`.
