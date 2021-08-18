---
eleventyNavigation:
  key: Svelte Material UI
  parent: Svelte
  order: 9
layout: topic-layout.njk
---

{% aTargetBlank "https://sveltematerialui.com/", "Svelte Material UI" %}
is a library of Svelte components that implement Material UI.
For a list of supported components, click the link above
and see the list in the left nav.

To install it in a Svelte project:

1. `cd` to the project root directory.

1. `npm install svelte-material-ui`

1. `npm install @smui/common`

1. Copy `node_modules/svelte-material-ui/bare.css`
   to `public/svelte-material-ui.css`.

1. Add the following line in `public/index.html` after the last `<link>` tag:

   ```html
   <link rel="stylesheet" href="/svelte-material-ui.css" />
   ```

Here's an example of using the `Button` and `Switch` components
in a Svelte component.

<img alt="app screenshot" class="keep-size"
  src="/blog/assets/svelte-material-ui.png?v={{pkg.version}}">

```html
<script>
  import Button from '@smui/button';
  import Switch from '@smui/switch';

  export let name;

  let clicked = 0;
  let likeIceCream = false;
</script>

<main>
  <Button on:click={() => clicked++}>Click Me</Button>
  <p>clicked = {clicked}</p>

  <Switch bind:checked={likeIceCream} />
  <span>Do you like ice cream?</span>
  <p>likeIceCream = {likeIceCream}</p>
</main>
```
