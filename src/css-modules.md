---
eleventyNavigation:
  key: CSS Modules
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://github.com/css-modules/css-modules", "CSS Modules" %}
provides a way to scope CSS rules with selectors that are class names
to a component in a web framework.
It does not support scoping CSS rules with
selectors that only target specific HTML elements.

Web frameworks that have built-in support for CSS scoping such as
Svelte and Vue have no need for CSS Modules.
CSS Modules are mostly commonly used in React
and web frameworks builtin on React such as Next.js.

## Installing

{% aTargetBlank
"https://nextjs.org/docs/app/building-your-application/styling/css-modules",
"Next.js" %} projects and React projects created with {% aTargetBlank
"https://create-react-app.dev", "create-react-app" %}
automatically include support for CSS Modules.

React projects created in other ways may require configuration.
Options include:

- {% aTargetBlank "https://github.com/gajus/babel-plugin-react-css-modules",
  "babel-plugin-react-css-modules" %}.
- Webpack module {% aTargetBlank
  "https://github.com/webpack-contrib/css-loader", "css-loader" %}
- PostCSS plugin {% aTargetBlank
  "https://github.com/madyankin/postcss-modules", "postcss-modules" %}

## Component Styles

To define CSS rules that should be scoped to a component, create a file
whose name matches that of the source file that defines the component.
The recommended file extension is `.module.css`.

CSS Module source files use standard CSS syntax,
but support a small number of extensions including:

- `:global` to define rules that are global and
  therefore not scoped to the component
- `:local` to switch back to local scope inside a global rule
- `composes` to include properties from another rule in rules where this appears
- `@value` to define variables, but it's better to use
  standard CSS custom properties (variables) instead

It is recommended to use camelCase names for
CSS classes define in CSS Modules source files.
The reason is that these become properties on an exported JavaScript object.
Referencing properties with snake_case or kebab-case names is somewhat tedious.

The following file, `MyComponent.module.css` defines styles
that will be scoped to the component `MyComponent`.

```css
.title {
  color: purple;
  font-size: 1.5rem;
  font-weight: bold;
}

.myComponent p {
  color: blue;
}
```

The following file, `MyComponent.tsx` defines
a component that uses the styles defined above.

```js
import React from 'react';

// The name "styles" here can be changed to anything,
// but this name is commonly used.
import styles from './MyComponent.module.css';

type Props = {
  title: string,
  content: string
};

export default function MyComponent({title, content}: Props) {
  return (
    <section className={styles.myComponent}>
      <div className={styles.title}>{title}</div>
      <p>{content}</p>
    </section>
  );
}
```

This component can be used as follows:

```js
<MyComponent title="My Story" content="Once upon a time ..." />
```

The `styles` object imported above will have a value similar to the following:

```js
MyComponent.tsx x: styles = {
  myClass: 'MyComponent_myClass__ac2E0',
  myComponent: 'MyComponent_myComponent__SsuQ7',
  __checksum: '94c8de6fb949'
}
```

The hash values at the ends of the generated class names
are the key to scoping styles to a component.
By default, they only change with the file name changes,
not when the CSS rules change.
There are several ways to change the strategy for generating these hash values
if desired, including specifying a `localIdentHashFunction`.
