---
eleventyNavigation:
  key: Svelte from React Components
  parent: Svelte
layout: topic-layout.njk
---

When converting a React component to a Svelte component
there are many common changes that are needed.
This page summarizes the most common of these.

{% raw %}

- Copy the JavaScript code into a `<script>` element in the Svelte component.
- Copy the topmost returned JSX to the HTML portion of the Svelte component.
- Copy the CSS into a `<style>` element in the Svelte component.
- In JSX elements that specify a `className` prop, change it to `class`.
- Change `style` attribute values from objects to strings.
  For example, replace `{backgroundColor: tan, color: red}`
  with `"background-color: tan; color: red"`.
- In JSX elements that specify event handing (such as `onClick`),
  change it to an `on` directive (such as `on:click`).
- Change `{condition && jsx}` to `{#if condition}html{/if}`.
- Change `{condition ? trueJsx : falseJsx}` to
  `{#if condition}trueHtml{:else}falseHtml{/if}`.
- Change `{array.map(element => jsx)}` to
  `{#each array as element}html{/each}`.
- Change `{array.map((element, index) => jsx)}` to
  `{#each array as element, index}html{/each}`.
- Change uses of the `useState` hook to just use a variable.
- Change uses of the `useRef` hook to use `bind:this={ref}`.
- Change uses of the `useEffect` hook that should only run
  on mount to use `onMount`.
- Change uses of the `useEffect` hook that should only run
  on mount and update to use `onMount` and `afterUpdate`.
- Change uses of the `useEffect` hook that should only run
  when certain variables change to a reactive statement.
- If something isn't updating properly after a prop value change,
  you probably forgot to use `$:` somewhere.

{% endraw %}
