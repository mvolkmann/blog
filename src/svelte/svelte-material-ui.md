---
eleventyNavigation:
  key: Svelte Material UI
  parent: Svelte
  order: 9
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://sveltematerialui.com/", "Svelte Material UI" %} (SMUI)
is a library of Svelte components that implement Material UI.
It is implemented in TypeScript.
It uses Sass for styling theme files.

For a list of supported components, click the link above
and see the list in the left nav.

The creator of SMUI, Hunter Perrin, created a great video
detailing how to get started using this library on
{% aTargetBlank "https://www.youtube.com/watch?v=OyjZ7dezADw", "YouTube" %}.

## Installing

To install it in a Svelte project:

1. `cd` to the project root directory.

1. Install the individual components that will be used.
   For example, `npm install -D @smui/button`.

1. Install smui-theme with `npm install -D smui-theme`.

1. Create theme files by entering `npx smui-theme template src/theme`.
   This creates `src/theme/_smui-theme.scss` and
   `src/theme/dark/_smui-theme.scss`.
   To change the theme, including colors, modify these files.

1. Add the following scripts in `package.json`:

   ```json
   "smui-dark": "smui-theme compile static/smui-dark.css -i src/theme/dark",
   "smui-light": "smui-theme compile static/smui-light.css -i src/theme",
   "theme": "npm run smui-light && npm run smui-dark",
   ```

1. Enter `npm run theme` to generate CSS files in the `static` directory.
   THIS IS CRITICAL!
   Repeat this every time a theme file is modified
   or a new SMUI component is installed.

1. Edit `src/app.html` and add the following lines before `%svelte.head%`:

   ```html
   <link
     rel="stylesheet"
     href="https://fonts.googleapis.com/icon?family=Material+Icons"
   />
   <link
     rel="stylesheet"
     href="https://fonts.googleapis.com/css?family=Roboto:300,400,500,600,700"
   />
   <link
     rel="stylesheet"
     href="https://fonts.googleapis.com/css?family=Roboto+Mono"
   />
   <link rel="stylesheet" href="/smui-light.css" />
   ```

## Colors

To customize theme colors:

- Browse {% aTargetBlank "https://materialui.co/colors/", "MUI Colors" %}
  to see names for recommended colors.
- Edit `.scss` files for light and dark themes under `src/themes`
  These already import `@material/theme/color-palette`
  which provides access to those colors.
- For example, change the value for `$primary` to `color-palette.$indigo-400`
  and run `npm run theme` to update the theme `.css` files in `src/static`.
- Make a similar change to the other Sass color variables.

## Top App Bar

To add a top app bar:

- Enter `npm install -D @smui/top-app-bar`.
- Copy the example code for a "page level" standard top app bar from
  {% aTargetBlank "https://sveltematerialui.com/demo/top-app-bar/",
  "Top App Bar" %}.
- Create the file `src/routes/__layout.svelte` and paste the code there.
  This will be used as the layout for every page.
- Replace the contents of the `AutoAdjust` element with `<slot />`.
- Enter `npm install -D @smui/icon-button`
  because that component is used in the pasted code.
- Enter `npm run theme` to update the theme `.css` files.
- Create the file `static/global.css` and add the following content:

  ```css
  body {
    margin: 0;
  }

  main {
    padding: 1rem;
  }
  ```

- Edit `src/app.html` and add the following after the existing `link` elements:

  {% raw %}

  ```html
  <link rel="stylesheet" href="/global.css" />
  ```

  {% endraw %}

## Light/Dark Modes

To add the ability to toggle between light and dark mode,
change `src/routes/__layout.svelte` to the following:

{% raw %}

```html
<script lang="ts">
  import {onMount} from 'svelte';
  import type {TopAppBarComponentDev} from '@smui/top-app-bar';
  import TopAppBar, {Row, Section, Title, AutoAdjust} from '@smui/top-app-bar';
  import IconButton from '@smui/icon-button';

  let darkTheme: boolean;
  let topAppBar: TopAppBarComponentDev;

  $: modeLabel = `switch to ${darkTheme ? 'light' : 'dark'} mode`;

  // This icon represents the mode to which the user can switch.
  $: modeIcon = darkTheme ? 'light_mode' : 'dark_mode';

  onMount(() => {
    darkTheme = window.matchMedia('(prefers-color-scheme: dark)').matches;
  });

  const toggleMode = () => (darkTheme = !darkTheme);
</script>

<svelte:head>
  {#if darkTheme === undefined}
  <link
    rel="stylesheet"
    href="/smui-light.css"
    media="(prefers-color-scheme: light)"
  />
  <link
    rel="stylesheet"
    href="/smui-dark.css"
    media="screen and (prefers-color-scheme: dark)"
  />
  {:else if darkTheme}
  <link rel="stylesheet" href="/smui-light.css" media="print" />
  <link rel="stylesheet" href="/smui-dark.css" media="screen" />
  {:else}
  <link rel="stylesheet" href="/smui-light.css" />
  {/if}
</svelte:head>

<TopAppBar bind:this="{topAppBar}" variant="standard">
  <Row>
    <section>
      <IconButton class="material-icons">menu</IconButton>
      <title>Standard</title>
    </section>
    <section align="end" toolbar>
      <IconButton
        aria-label="{modeLabel}"
        class="material-icons"
        on:click="{toggleMode}"
        title="{modeLabel}"
      >
        {modeIcon}
      </IconButton>
    </section>
  </Row>
</TopAppBar>
<AutoAdjust {topAppBar}>
  <slot />
</AutoAdjust>
```

{% endraw %}

The code above uses
<a href="https://fonts.google.com/icons?selected=Material%2BIcons"
rel="noopener" target="_blank">Material Icons</a>.

## Example Component

Here's an example of using the `Button` and `Switch` components
in a Svelte component.

<img alt="app screenshot" class="keep-size"
  src="/blog/assets/svelte-material-ui.png?v={{pkg.version}}">

{% raw %}

```html
<script>
  import Button from '@smui/button';
  import Switch from '@smui/switch';

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

{% endraw %}
