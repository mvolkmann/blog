---
eleventyNavigation:
  key: Component Libraries
  parent: Svelte
layout: topic-layout.njk
---

## Overview

The number of component libraries for Svelte is growing!
This post provides an overview of some of the best options.

## Carbon Components Svelte

{% aTargetBlank "https://carbon-components-svelte.onrender.com",
"Carbon Components Svelte" %} is a library of UI components from IBM
that implement their {% aTargetBlank "https://www.carbondesignsystem.com",
"Carbon Design System" %}.

This library provides an extensive collection of components.
To my eye these all have a flat, plain look.

## Svelte Material UI

{% aTargetBlank "https://sveltematerialui.com/", "Svelte Material UI" %} (SMUI)
is a large library of Svelte components that implement Material UI.
It is implemented in TypeScript and uses Sass for style theme files.

For a list of supported components, click the link above
and see the list in the left nav.

The creator of SMUI, Hunter Perrin, created a great
{% aTargetBlank "https://www.youtube.com/watch?v=OyjZ7dezADw", "YouTube" %}
video detailing how to get started using this library.

For details on installing and using this library, see my post at
{% aTargetBlank "/blog/topics/#/blog/svelte/svelte-material-ui",
"Svelte Material UI" %}.

For an example of using this library, see my GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/sveltekit-smui-demo", "sveltekit-smui-demo" %}.

<div>
  <figure style="width: 25%">
    <img alt="SMUI single select closed"
      src="/blog/assets/smui-single-select1.png?v={{pkg.version}}"
      title="SMUI single select closed">
    <figcaption>SMUI single select in closed state</figcaption>
  </figure>
</div>

<div>
  <figure style="width: 60%">
  <img alt="SMUI single select open"
    src="/blog/assets/smui-single-select2.png?v={{pkg.version}}"
    title="SMUI single select open">
    <figcaption>SMUI single select in open state</figcaption>
  </figure>
</div>

## svelte-calendar

{% aTargetBlank "https://6edesign.github.io/svelte-calendar/",
"svelte-calendar" %} is a date picker component for Svelte.
It includes a large amount of animation.
For some users this may feel like too much animation
and be more of a distraction than a positive feature.
See the demo at the link above.

## svelte-datepicker

{% aTargetBlank "https://github.com/beyonk-adventures/svelte-datepicker",
"svelte-datepicker" %} from {% aTargetBlank "https://beyonk.com", "Beyonk" %}
is a component library that provides calendar, date picker,
date range picker, and time selection.

For an example, see this {% aTargetBlank
"https://svelte.dev/repl/d812e880c6934f9e9a7cf9f760eddc11?version=3.31.2",
"REPL" %}.

This renders a selected date range in a nice way,
but changing the start and end dates feels clumsy.

## svelte-fullcalendar

{% aTargetBlank "https://github.com/YogliB/svelte-fullcalendar",
"svelte-fullcalendar" %} is a Svelte wrapper around the framework independent
library {% aTargetBlank "https://fullcalendar.io", "FullCalendar" %}.
This is a very popular open source library that also has a paid tier
offering advanced features.

For an example of using this library, see my GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/sveltekit-smui-demo", "sveltekit-smui-demo" %}.

<img alt="svelte-fullcalendar" style="width: 70%"
  src="/blog/assets/svelte-fullcalendar.png?v={{pkg.version}}"
  title="svelte-fullcalendar">

## Svelte MultiSelect

{% aTargetBlank "https://svelte-multiselect.netlify.app",
"Svelte MultiSelect" %} is an input component that displays a dropdown list
of options from which the user can make multiple selections.
Each selected option is displayed as a "chip"
with an "x" that can be clicked to deselect it.

For an example of using this component, see my GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/sveltekit-smui-demo", "sveltekit-smui-demo" %}.

<img alt="svelte-multiselect" style="width: 70%"
  src="/blog/assets/svelte-multiselect.png?v={{pkg.version}}"
  title="svelte-multiselect">

## svelte-time-picker

{% aTargetBlank "https://gitlab.com/public-e-soa-com/svelte-time-picker#readme",
"svelte-time-picker" %} is a time picker component for Svelte.
It uses the lollypop style, which many users may find confusing.
The hour is selected by dragging a circle around the perimeter of a clock face.
When the mouse is released, the clock face changes
to display minutes instead of hours.
It is not obvious how to return to the hour clock face,
but this is done by click the hour number above the clock face.

For an example of using this component, see my GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/sveltekit-smui-demo", "sveltekit-smui-demo" %}.

<img alt="svelte-time-picker hour" style="width: 40%"
  src="/blog/assets/svelte-time-picker-hour.png?v={{pkg.version}}"
  title="svelte-time-picker hour">
<img alt="svelte-time-picker minute" style="width: 40%"
  src="/blog/assets/svelte-time-picker-minute.png?v={{pkg.version}}"
  title="svelte-time-picker minute">

## HTML input type="date"

Using the HTML `input` element with a `type` of "date" works,
but has a number of issues documented in {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/date#using_date_inputs",
"MDN" %}. Chief among the issues is consistent browser support.
For this reason, I recommend using a date picker component.

<img alt="input type date" style="width: 40%"
  src="/blog/assets/input-type-date.png?v={{pkg.version}}"
  title="input type date">

## Custom Date Picker

I have implemented a date picker that is
not yet available in its own GitHub repository.
However, it can be found in my GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/sveltekit-smui-demo", "sveltekit-smui-demo" %}.
See the file `DatePicker.svelte`.

<img alt="Svelte custom date picker single" style="width: 60%"
  src="/blog/assets/svelte-custom-date-picker-single.png?v={{pkg.version}}"
  title="Svelte custom date picker single">

<img alt="Svelte custom date picker range" style="width: 70%"
  src="/blog/assets/svelte-custom-date-picker-range.png?v={{pkg.version}}"
  title="Svelte custom date picker range">

## Custom Date Range

I have implemented a date range picker that is
not yet available in its own GitHub repository.
However, it can be found in my GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/sveltekit-smui-demo", "sveltekit-smui-demo" %}.
See the file `DateRange.svelte`.

<img alt="custom date range" style="width: 80%"
  src="/blog/assets/svelte-custom-date-range.png?v={{pkg.version}}"
  title="custom date range">

## HTML input type="time"

Using the HTML `input` element with a `type` of "time"
is a reasonable option for allowing users to select a single time value.

For an example of using thi , see my GitHub repository {% aTargetBlank
"https://github.com/mvolkmann/sveltekit-smui-demo", "sveltekit-smui-demo" %}.

<img alt="input type time" style="width: 30%"
  src="/blog/assets/input-type-time-chrome.png?v={{pkg.version}}"
  title="input type time">

## Toast UI Calendar

The Toast UI {% aTargetBlank "https://ui.toast.com/tui-calendar", "Calendar" %}
component is an alternative to the FullCalendar component described earlier.
It seems to have a large number of features and is free.
But so far I haven't been able to get it to work.
