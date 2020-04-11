---
layout: layout.njk
order: 1
tags: [navItem, intro]
title: Static site generators
---

Static site generators (SSGs) are an alternative
to CMS tools (like WordPress and Drupal)
and graphical site builders (like Squarespace and Wix).

The primary benefits of SSGs are:

- Static sites provide excellent runtime performance.
- Static sites are more secure because all API interactions
  occur during site generation, not when users interact with the site.
  This means there is less of a chance that server hacking
  can affect the content of a site.
- The supported markup/templating options are typically
  easier to write and understand than HTML.
- Development of an SSG site can start from
  a pre-built site to speed development.

See [staticgen.com](https://www.staticgen.com/) for a list
of SSGs that can be sorted on various criteria.

Key differentiators between SSGs include:

- supported template languages (Angular, EJS, Go, Haml,
  Handlebars, HTML, JavaScript, Liquid, Markdown, Marko,
  Mustache, Nunjucks, Pug, React, Swig, Twig, Vue, ...)
- the implementation language (Go, JavaScript, PHP, Python, Ruby, ...)
  which affects your ability to customize the software
- ability to configure the SSG to use
  existing content files and directory structures
- license (Apache-2.0, Jinja2, MIT, ...)

For static sites that obtain data from API services,
a common strategy is to automatically regenerate them
on a scheduled basis such as every night.
This is typically quite easy to implement.

Another strategy is to store all the files for a site
in a repository such as GitHub and automatically
rebuild and redeploy the site any time changes are pushed.
