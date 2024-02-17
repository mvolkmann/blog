---
eleventyNavigation:
  key: Web Components
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="Web Components logo" style="border: 0"
    src="/blog/assets/web-components-logo.svg?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://www.webcomponents.org/introduction", "Web components" %}
"are a set of web platform APIs that allow you to create
new custom, reusable, encapsulated HTML tags to use in web pages and web apps."

## Custom Elements

{% aTargetBlank "https://html.spec.whatwg.org/multipage/custom-elements.html",
"Custom Elements" %} provide a way to define and use custom HTML elements.

## Shadow DOM

The {% aTargetBlank "https://dom.spec.whatwg.org/#interface-shadowroot",
"Shadow DOM" %} provides a way to encapsulate the content and styling
of a custom element.

## ES Modules

{% aTargetBlank "https://html.spec.whatwg.org/multipage/webappapis.html#integration-with-the-javascript-module-system",
"ES Modules" %} define the mechanisms for
exporting and importing JavaScript modules.

## HTML Template

The HTML {% aTargetBlank
"https://html.spec.whatwg.org/multipage/scripting.html#the-template-element",
"template element" %} provides a way to define an HTML fragment
that can be cloned and inserted multiple times into a DOM tree.
