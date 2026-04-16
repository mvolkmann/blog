---
eleventyNavigation:
  key: GitHub Pages
  parent: GitHub
  title: Pages
layout: topic-layout.njk
---

GitHub Pages provides a very easy, free way to host static web sites.
The steps to use it are:

1. Create a GitHub repository.
1. Add static assets such as HTML, CSS, JavaScript, and image files.
1. Browse the GitHub repository.
1. Click "Settings" near the upper-right.
1. Scroll to the "GitHub Pages" section.
1. In the "Source" drop-down, select "master branch".
1. Wait about 30 seconds.
1. Browse `https://{username}.github.io/{repo-name}/`.

Here is a simple `index.html` file that results in a web site
with a perfect Lighthouse score for both desktop and mobile.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Simple Site</title>
    <meta charset="utf-8" />
    <meta name="description" content="simple site" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
  </head>
  <body>
    Welcome to my simple site!
  </body>
</html>
```

To make changes to the site:

1. Modify existing files and add new ones.
1. Commit the changes.
1. Push changes to the master branch.

The site will update automatically in about 30 seconds.
