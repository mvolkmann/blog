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

## IndexedDB Interfaces

### IDBFactory

The `IDBFactory` interface provides access to databases.
Instances support the following operations.

- open database

  ```js
  const version = 1;
  const request = indexedDB.open('db-name', version);

  request.onerror = event => {
    console.error('failed to open database:', event);
  };

  request.onsuccess = () => {
    const db = request.result;
    // Use the database.
  };

  // This is called the first time a database is used
  // and again each time the version number changes.
  request.onupgradeneeded = event => {
    const db = request.result;
  };
  ```

- delete database

  ```js
  const request = indexedDB.deleteDatabase('db-name');

  request.onerror = event => {
    console.error('failed to delete database:', event);
  };

  request.onsuccess = event => {
    console.log('deleted database');
    // event.result should be undefined
  };
  ```

### IDBDatabase

The `IDBDatabase` interface provides a connection to a database.
Instances support the following operations.

- create store

```js
// Options are optional.  autoIncrement defaults to false.
const options = {autoIncrement: true, keyPath: 'property-name'};
const store = db.createObjectStore('store-name', options);
```

- delete store

  ```js
  db.deleteObjectStore('store-name');
  ```

- create transaction

  ```js
  const mode = 'readwrite'; // or 'readonly' (default)
  const stores = ['db-name']; // can be one string or an array of them
  const txn = db.transaction(stores, mode);
  ```

- close database connection

  ```js
  db.close();
  ```

### IDBTransaction

- delete a store

  ```js
  const store = txn.objectStore('store-name');
  ```

- abort transaction (rolls back)

  ```js
  txn.abort();
  ```

- commit transaction

  This is not normally needed because transactions
  automatically commit when all requests are satisfied.

  ```js
  txn.commit();
  ```

- listen for events

  ```js
  txn.onabort = () => {
    console.log('transaction aborted');
  };

  txn.oncomplete = () => {
    console.log('transaction completed');
  };

  txn.onerror = event => {
    console.error('transaction error:', event);
  };
  ```

### IDBObjectStore

### IDBRequest

### IDBCursor

## Common Operations

## Create an index

```js
const db = request.result;
const store = db.createObjectStore('dogs', {keyPath: 'id'});
// Include multiple property names in the
// 2nd parameter array for a compound index.
store.createIndex('breed', ['breed'], {unique: false});
```

## Delete an index

## Create an object

## Query for a single object

## Query for multiple objects

## Modify an object

## Delete an object
