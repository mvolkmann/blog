---
eleventyNavigation:
  key: Progressive Web Apps (PWAs)
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<img alt="PWA logo" style="border: none; width: 30%"
  src="/blog/assets/pwa-logo.svg?v={{pkg.version}}"
  title="PWA logo">

## Overview

Progressive Web Apps (PWAs) enable using web technologies
to implement mobile apps.

Supported features include background sync, bluetooth, camera access,
contact access, device motion, file access, geolocation, offline mode,
push notifications, and touch gestures.

To open a PWA in a phone, browse its URL from the a mobile web browser.
PWAs can be installed which results in adding an app icon to the home screen.

Advantages of PWAs over native mobile applications include the ability to:

- allow users to access apps by URL
  instead of needing to download from an app store
- bypass app store review
- avoid app store cut of purchase prices (such as Apple's 30% cut)
- provide automatic app updates
- implement using widely known web technologies
- run on web, Android, and iOS with a single code base

## Evaluating Readiness

To determine if a web app can be used as a PWA:

- Use a desktop computer or laptop to open the app in the Chrome web browser.
- Open the DevTools.
- Click on the "Lighthouse" tab.
- Click the "Analyze page load" button.
- Look for the "Progressive Web App" score.
