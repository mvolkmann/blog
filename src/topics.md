---
layout: top-layout.njk
---

<link rel="stylesheet" href="/blog/assets/topics.css">

{# TODO: Is the eleventy-navigation package still needed? #}

<nav>
  {% import 'macros.njk' as macros with context %}
  {{ macros.renderNavList(collections.nav) }}
</nav>

<section class="topic-content">
  <iframe name="frame" src="../welcome/" title="topic content"></iframe>
</section>
