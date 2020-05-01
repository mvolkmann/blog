---
layout: top-layout.njk
---

<link rel="stylesheet" href="/blog/assets/topics.css">

<nav>
  {% import 'macros.njk' as macros with context %}
  {% set entries = collections.all | eleventyNavigation | navSort() %}
  {{ macros.renderNavList(entries) }}
</nav>

<section class="topic-content">
  <iframe name="frame" src="../welcome/" title="topic content"></iframe>
</section>
