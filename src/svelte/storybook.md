---
eleventyNavigation:
  key: Svelte and Storybook
  parent: Svelte
  order: 5
layout: topic-layout.njk
---

## Overview

When developing web applications it is useful to be able to
render a single component rather than run the entire app.
This is especially true when the component
does not appear on the first rendered page.
For example, suppose we are developing a shopping app with
the typical flow of login, add products to cart, and checkout.
While developing a component that appears on the checkout page
it is tedious to have to retrace all of these steps
in order to test changes to the new component.

[Storybook](https://storybook.js.org) solves this! It provides a web UI where
a list of components in various states appear in the left nav.
Clicking one renders only that component in the main area where
we can interact with it in all the ways it supports.
It also provides a nice way to demo each component to stakeholders
and focus on the design and styling of each component.

## Storybook with Svelte

Storybook can be used with many web frameworks.
Let's dive into how it can be used with Svelte,
appropriate as I'm the author of the Manning book
[Svelte and Sapper in Action](https://www.manning.com/books/svelte-and-sapper-in-action).
Detailed instructions for using Storybook with Svelte components
can be found in the Storybook documentation
at <https://storybook.js.org/docs/guides/guide-svelte/>.

To add the use of Storybook to an existing Svelte application,
`cd` to the top application directory and enter
`npx -p @storybook/cli sb init --type svelte`.
This takes a couple of minutes to complete
and results in the following changes:

- It installs all the development dependencies needed by Storybook.
- It adds the following npm scripts in `package.json`:

  ```json
  "storybook": "start-storybook",
  "build-storybook": "build-storybook"
  ```

- It creates the `.storybook` directory, which
  contains the `addons.js` file that
  registers the use of Storybook actions and links.
  Actions provide a way to log user interactions,
  such as a certain button being pressed.
  Links provide a way to add links in stories
  that can be clicked to navigate to other stories,
  as an alternative to clicking a link in the left nav.

  This directory also contains the `config.js` file that
  configures the use of Storybook to automatically
  import all files found in the `stories` directory
  whose names end with `.stories.js`.

- It creates the `stories` directory,
  which contains the `button.svelte` file
  that defines a simple Svelte component.
  It also contains the `index.stories.js` file that
  registers the `Button` component with Storybook.
  These files just provide an example
  of registering a component with Storybook.
  Typically source files for the components being demonstrated are
  found in the `src` directory rather than the `stories` directory.

The supplied `Button` component dispatches an event when it is clicked.
The `index.stories.js` file listens for the event and calls the
`action` function that it imports from `@storybook/addon-actions`.
Strings passed to the `action` function are logged
at the bottom of the main section of the Storybook UI.
The Clear button in the lower-right corner of the Actions area
clears the list of logged actions.

To run Storybook locally, enter `npm run storybook`.

To add components to Storybook,
add one `.stories.js` file for each component
in the `stories` directory.
These files should be similar to the
provided `index.stories.js` file.

Each story renders a component in a different state.
It is common for a `.stories.js` file to define multiple
stories that render the same component in different states.
Each state has a name that is displayed in the left nav
below the component name.
The two states for the provided `Button` component
are "Text" and "Emoji".

When new `.stories.js` files are created, refresh the browser
where Storybook is running to see them in the left nav.
When an existing `.stories.js` file is modified,
Storybook automatically detects the changes and
displays the modified story at the bottom of the left nav.

Let’s create a new Svelte component and implement some Storybook stories for it!
Start by entering the following commands:

```bash
npx degit sveltejs/template storybook-demo
cd storybook-demo
npm install
npx sb init
```

Add the file `src/Pie.svelte` containing the code below.
This component renders a progress indicator in the form of a pie chart.
It uses SVG to drag the pie and takes advantage of the
Svelte `tweened` function to animate changes to the value.

```html
<script>
  import {tweened} from 'svelte/motion';

  export let bgColor = 'tan';
  export let fgColor = 'blue';
  export let size = 50;
  export let value; // 0 to 100

  const store = tweened(value, {duration: 500});

  $: half = size / 2;
  $: viewBox = `0 0 ${size} ${size}`;
  $: circumference = 2 * Math.PI * half;

  let dashArray = '';

  $: {
    const v = Math.max(0, Math.min(100, value));
    store.set(v);
    const dash = ((v / 100) * circumference) / 2;
    dashArray = `${dash} ${circumference - dash}`;
  }
</script>

<svg height="{size}" width="{size}" {viewBox}>
  <circle class="bg" fill="{bgColor}" r="{half}" cx="{half}" cy="{half}" />
  <circle class="fg" r={half / 2} cx={half} cy={half} fill="transparent"
  stroke={fgColor} stroke-width={half} stroke-dasharray={dashArray} />
</svg>

<style>
  svg {
    transform: rotate(-90deg);
  }
</style>
```

Add the file `src/stories/Pie.stories.js` containing the following
which renders the `Pie` component with the values 0, 30, 90, and 100:

```js
import Pie from '../Pie.svelte';

export default {
  title: 'Pie',
  component: Pie,
  argTypes: {
    value: {control: 'number'}
  }
};

const Template = props => ({
  Component: Pie,
  props
});

export const Pie0 = Template.bind({});
Pie0.args = {value: 0};

export const Pie30 = Template.bind({});
Pie30.args = {value: 30};

export const Pie90 = Template.bind({});
Pie90.args = {value: 90};

export const Pie100 = Template.bind({});
Pie100.args = {value: 100};
```

Enter the command `npm run storybook`.
This starts the Storybook server and opens the Storybook page in your default web browser.
Click the `Pie` component in the left nav to expand it
and click "Pie 30" which renders the following:

<img alt="Pie story" class="keep-size"
  src="/blog/assets/svelte-storybook-1.png?v={{pkg.version}}">

## Adding Controls

It would be nice if we could interactively
change the value used by the `Pie` component.
One way to do this is to introduce a new Svelte component
that includes a slider for changing the value
and then add a Storybook story for it.

Here is that component, defined in the file `src/stories/PieWithControl.svelte`:

```html
<script>
  import Pie from '../Pie.svelte';

  let value = 30;
</script>

<main>
  <div>
    <label for="value">Value</label>
    <input id="value" type="range" bind:value />
  </div>
  <Pie {value} />
</main>
```

And here is the new Storybook story for this component
defined in the file `src/stories/PieWithControl.stories.js`:

```js
import PieWithControl from './PieWithControl.svelte';

export default {
  title: 'PieWithControl',
  component: PieWithControl
};

const Template = props => ({Component: PieWithControl});

export const Primary = Template.bind({});
```

When this story is selected in Storybook,
the following is rendered:

<img alt="PieWithControl story" class="keep-size"
  src="/blog/assets/svelte-storybook-2.png?v={{pkg.version}}">

Now the slider value can be changed to render the `Pie` component
with any value from zero to 100.

## Static Deploy

Sometimes it is desirable to generate and deploy a static version
of Storybook, including all the registered components,
to allow others to view the components.
To generate a static version of Storybook,
enter `npm run build-storybook`.
This creates the `storybook-static` directory
containing all the required HTML and JavaScript files.
All required CSS rules are compiled into the JavaScript.
This directory can be copied to any web server.
To view it locally, simply open the `index.html` file in a web browser.

## Conclusion

Storybook provides a nice way to
see a list of components used by an app,
demonstrate them in various states,
and to interact with them.
When a bug is discovered in a component,
it can be debugged in the context of Storybook,
which is typically faster than debugging in an app that uses it
because there is no need to navigate to where the component is used.
