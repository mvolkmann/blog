---
eleventyNavigation:
  key: Accessibility Testing
  parent: Eleventy
layout: layout.njk
tags: eleventy
---

It is recommended to perform accessibility testing on all sites.
Two recommended tools are Lighthouse and axe.

Lighthouse is built into the Chrome browser.
To run it on a site:

1. Browse the site.
1. Open the browser DevTools.
1. Click the "Audits" tab.
1. Press the "Generate report" button.
1. Fix all issues that are identified.
1. To rerun the audit, click the icon of a circle with a diagonal slash
   to clear the current report and press the "Generate Report" button again.
1. Run the audit with the "Device" radio button set to "Desktop"
   and then again with "Mobile".
1. Keep going until you get a score of 100 in every category
   for both "Desktop" and "Mobile".

The axe tool is available as a Chrome extension.
To install it in Chrome:

1. Browse <https://www.deque.com/axe/>.
1. Click the "Download axe" button.
1. Click the "Add to Chrome" button.

To run Axe on a web site:

. Browse the web site to be tested.
. Open the browser DevTools.
. Click the axe tab.
. Click the "Analyze" button.
. Click each issue identified in the left nav
to see a detailed description on the right.

To navigate between multiple instances of the same issue type,
click the "<" and ">" buttons in the upper-right.

To see the rendered element associated with the issue,
click "Highlight".

To see the DOM element associated with the issue,
click "</> Inspect Node".

To rerun the tests after code changes,
press "Run again".
