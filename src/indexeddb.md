---
eleventyNavigation:
  key: IndexedDB
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

{% aTargetBlank "https://www.w3.org/TR/IndexedDB/", "IndexedDB" %}
is a JavaScript-based, object-oriented database
that is supported by modern web browsers.

Unlike relational databases that are composed of
tables with records that are composed of columns,
IndexedDB databases are composed of stores that hold objects with properties.

Objects within stores can be retrieved by their key, and by optional indexes.

Modifications occur within transactions that can be committed or rolled back.

All accesses to IndexedDB databases are performed asynchronously,
so they do not impact the interactivity of a web application
by blocking the main thread.

## Storage Quotas

The storage quota available to IndexedDB varies based on
the web browser being used and the total amount of storage on the device.

In Chrome and Chromium-based browsers (including Edge),
each domain (or origin) can use up to 60% of total storage space.
For example, on a phone with 128 GB of memory,
a maximum of 76.8 GB can be used per domain.
The available memory is typically much lower due to
the operating system and applications consuming a large part of the memory.

In Safari, each domain can use up to 20% of the total storage space.
If a PWA is saved to the home screen,
the limit is increased to 60% of the total storage space.
There is also a quota of 80% of the total storage space across all domains.

Web applications can obtain an estimate of the storage space
available to their domain with the following:

```js
const estimate = await navigator.storage.estimate();
```

The `estimate` variable holds an object with three properties.
The `quota` property gives the maximum number of bytes available.
The `usage` property gives the number of bytes currently being used.
The `usageDetails` property is an object that describes
how the memory is currently being used.

If an attempt is made to use more storage space than the quota allows,
a `QuotaExceedError` will be thrown.

Previously stored data can be evicted if
the available storage is close to being exhausted or
the quota across all domains is exceeded.

In Safari, if the setting "Prevent Cross-Site" Tracking" is turned on,
data for any origin that hasn't had an user interactions in the last week
will be evicted.
This is clearly bad web apps that intended to
store data for long periods of time.

## DevTools

To see the contents of IndexedDB databases in Chrome:

- open the DevTools
- click the "Application" tab
- in the left nav under the "Storage" section, expand "indexedDB"
- click the name of a store
- all the keys and their object values will be displayed
- objects can be deleted, but not modified

To see the contents of IndexedDB databases in Chrome:

- open the "Web Inspector"
- click the "Storage" tab
- in the left nav, expand "Indexed Databases"
- expand a specific database
- click a specific store
- all the keys and their object values will be displayed

## Common Operations

### Create a database

```js
const dbName = 'myDB';
const version = 1;
const request = indexedDB.open(dbName, version);

request.onerror = event => {
  console.error('IndexedDB error: ', event);
};

// This is called the first time a database is used
// and again each time the version number changes.
request.onupgradeneeded = event => {
  const db = request.result;
};

request.onsuccess = () => {
  const db = request.result;
  const txn = db.transaction('dogs', 'readwrite');

  txn.oncomplete = () => {
    db.close();
  };
};
```

## Delete a database

## Create an index

## Delete an index

## Create an object

## Query for a single object

## Query for multiple objects

## Modify an object

## Delete an object
