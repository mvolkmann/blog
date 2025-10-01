---
eleventyNavigation:
  key: ARIA
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.w3.org/WAI/standards-guidelines/aria/",
"Accessible Rich Internet Applications" %} ARIA defines
attributes and roles that make web content more accessible.

There are around 50 defined ARIA attributes and around 48 roles,
but only a handful of these are commonly used.
In general, whenever native HTML elements and their attribute
can be used instead, those are preferred.

## Commonly Used ARIA Attributes

### aria-controls

Apply this to elements that users can interact with to modify the
appearance of other elements, including changing whether they are visible.
For example, this can be applied to a `button` element
when clicking it toggles the visibility of a `p` element.

### aria-describedby

Apply this to elements whose content describes another element.
This differs from the attributes `aria-label` and `aria-labelledby`
which provide a short name for an element.
This attribute identifies another element that provides supplementary,
often longer, instructions, context, or validation messages.
It acts like an accessible tooltip or hint.

For example:

```html
<button aria-describedby="launch-desc">Launch</button>
<p id="launch-desc">
  Click the "Launch" button to launch missiles that may cause destruction.
</p>
```

This attribute can refer to multiple elements. For example:

```html
<input type="password" aria-describedby="password-error password-hint" />

<!-- This element is perhaps always visible. -->
<p id="password-hint" class="hint-text">
  The password must be 8-20 characters long and include one special character.
</p>

<!-- This element is perhaps only visible after invalid input is entered. -->
<div id="password-error" role="alert" style="display: none;">
  The password must include a special character.
</div>
```

### aria-disabled

Apply this to elements that are not form controls, but play the role of one,
and should be considered to be in a disabled state.
Typically it is best to use form elements like `button` and `input`,
and use their `disabled` attribute instead of the `aria-disabled` attribute.

### aria-expanded

Apply this to elements that control whether their content or
the content of another element is expanded/displayed or collapsed/hidden.
When expanded, use `aria-expanded="true"`.
When collapsed, use `aria-expanded="false"`.

If the element controls the content of another element,
also include the `aria-controls` attribute to identify that relationship.

One example of using this is rendering a tree of data where
users can click a parent node to toggle whether its child nodes are visible.
The elements representing parent nodes
should include the `aria-expanded` attribute.

### aria-hidden

Apply this elements that should be hidden from assistive technologies.
Examples include:

- purely decorative content, such as icons or images
- offscreen or collapsed content, such as menus

### aria-invalid

Apply this to `input` elements using JavaScript after validation code
determines whether the value is valid.
Possible attribute values include "true", "false", "grammar", and "spelling".
Any other values are treated the same as "true".
When the `input` value is valid, this attribute can be remove,
or its value can be set to "false" or an empty string.

### aria-label

Apply this to elements to give then an accessible name.
For example, a `button` element that deletes some associated data
and only contains a trash can icon,
can have the attribute `aria-label="delete"`.
Typically the `label` element is used in place of this
when displaying the accessible name is desired.

### aria-labelledby

Apply this to elements whose accessible name is provided by another element.
The other element does not need to be visible.

### aria-required

Apply this to elements that play the role of a form control
and require the user to enter/select a value.
When the elements `input`, `select`, and `textarea` require a value,
use the `required` attribute instead of the `aria-required` attribute.

## Roles

ARIA supports two categories of roles.

"Landmark" roles are applied to elements that contain a section of the page.

"Widget" roles are applied to elements not normally used as
form controls but are configured to behave like them.
For example, they make it possible implement button-like functionality
using a `div` element in an accessible way.
It's best to use the native HTML form control elements when possible.

### Landmark Roles

- `banner`

  Apply this to an element that contains the main header content.
  Typically the `header` element is used instead of
  applying this role to some other kind of element.

- `complementary`

  Apply this to elements that contain information that
  supports the main content, such as a sidebar.
  Typically the `aside` element is used instead of
  applying this role to some other kind of element.

- `contentinfo`

  Apply this to elements that contain document metadata
  such as copyright and author information.

- `main`

  Apply this to a single element that holds the main content of the page.
  Typically the `main` element is used instead of
  applying this role to some other kind of element.

- `navigation`

  Apply this to elements contain navigational links
  such as menu or table of contents.
  Typically the `nav` element is used instead of
  applying this role to some other kind of element.

- `search`

  Apply this to elements that contain search functionality.
  Typically the `search` element is used instead of
  applying this role to some other kind of element.

### Widget Roles

- `alert`

  Apply this to elements that

- `button`

  Apply this to elements that have been configured to act as buttons.
  Typically the `button` element is used instead of
  applying this role to some other kind of element.

- `checkbox`

  Apply this to elements that have been configured to act as checkboxes.
  Typically the `input` element with a `type` attribute of "checkbox" is
  used instead of applying this role to some other kind of element.

- `dialog`

  Apply this to elements that represent a modal window or alert that requires
  user interaction and temporarily blocks access to other parts of the page.
  Typically the `dialog` element or a `div` element with a `popover` attribute
  is used instead of applying this role to some other kind of element.

- `tab`

  Apply this to elements that controls the visibility of a tab pane.

- `tablist`

  Apply this to elements that contain a set of "tab" elements.
