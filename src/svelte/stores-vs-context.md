---
eleventyNavigation:
  key: Stores vs. Context
  parent: Svelte
layout: topic-layout.njk
---

On 4/21/2021 Tan Li Han posted an excellent {% aTargetBlank
"https://twitter.com/lihautan/status/1385027047659950083?s=20",
"tweet thread" %} on choosing between
storing data in a Svelte store or in context.
This is my summary of his advice.

## Dynamic data

If the data is dynamic, meaning that it will
change during the running of the web app
and components need to react to the changes, use a store.

### dynamic + global case

If the store should be available to any component,
define and export it in a file like `stores.js`
and import it where needed.
I frequently use this approach.

### dynamic + local case

If descendant components need to use a different store
based on the ancestor that rendered them,
define the store in ancestor components, place it in context,
and retrieve the store from context in the descendant components.
This approach can also be used to limit the scope
in which a given store is accessible,
unlike the "dynamic + global" approach
which makes stores available to any component.
In my experience, this option is rarely needed.

## Static data

If the data is static, meaning that it will
remain the same during the running of the web app,
consider whether the value used by a component
should depend on which ancestor component renders it.

### static + global case

If it does not depend on the ancestor component,
define a constant in some file and import that everywhere it is needed.
I frequently use this approach.

### static + local case

If it does depend on the ancestor component,
set it in context in ancestor components
and get it from context in descendant components.
In my experience, this option is rarely needed.
