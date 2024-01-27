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
IndexedDB databases are composed of stores that hold records with properties.

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
- all the record keys and values will be displayed
- records can be deleted, but not modified

To see the contents of IndexedDB databases in Chrome:

- open the "Web Inspector"
- click the "Storage" tab
- in the left nav, expand "Indexed Databases"
- expand a specific database
- click a specific store
- all the record keys and values will be displayed

## IndexedDB Interfaces

The IndexedDB API defines many interfaces that implementations implement.
The following subsections summarized the most important
properties and methods of these interfaces.

### IDBFactory

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBFactory",
"IDBFactory" %} interface provides access to databases.
Instances support the following operations.

#### open database

```js
const version = 1;
// indexedDB is a global property on the window object.
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
  // The event may be an instance of the IDBVersionChangeEvent interface
  // and have the properties "oldVersion" and "newVersion".
  const db = request.result;
};
```

#### delete database

```js
const request = indexedDB.deleteDatabase('db-name');

request.onsuccess = event => {
  console.log('deleted database');
  // event.result should be undefined
};

request.onerror = event => {
  console.error('failed to delete database:', event);
};
```

### IDBDatabase

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBDatabase",
"IDBDatabase" %} interface provides a connection to a database.
Instances support the following operations.

#### create store

```js
// Options are optional.  autoIncrement defaults to false.
const options = {autoIncrement: true, keyPath: 'property-name'};
const store = db.createObjectStore('store-name', options);
```

#### delete store

```js
db.deleteObjectStore('store-name');
```

#### create transaction

```js
const mode = 'readwrite'; // or 'readonly' (default)
const stores = ['db-name']; // can be one string or an array of them
const txn = db.transaction(stores, mode);
```

#### close database connection

```js
db.close();
```

### IDBTransaction

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBTransaction",
"IDBTransaction" %} interface provides an asynchronous transaction
over a set of stores in a common database.
Instances support the following properties and methods.

#### get database associated with transaction

```js
const db = txn.db;
```

#### get store names associated with transaction

```js
const storeNames = txn.objectStoreNames;
```

#### get existing store

```js
const store = txn.objectStore('store-name');
```

#### delete existing store

```js
const store = txn.objectStore('store-name');
```

#### commit transaction

This is not normally needed because transactions
automatically commit when all requests are satisfied.

```js
txn.commit();
```

#### abort transaction (rolls back)

```js
txn.abort();
```

#### listen for events

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

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore",
"IDBObjectStore" %} interface represents a database store
which is similar to a table in a relational database.
Instances support the following operations.

#### get store name

```js
const name = store.name;
```

#### get key path

```js
const keyPath = store.keyPath;
```

#### get index names

```js
const indexNames = store.indexNames;
```

#### get transaction associated with store

```js
const txn = store.transaction;
```

#### add record to store

```js
const request = store.add(value, key);

request.onsuccess = event => {
  console.log('added record');
};

request.onerror = event => {
  console.error('failed to add record:', event);
};
```

#### delete all records from store

```js
const request = store.clear();

request.onsuccess = event => {
  console.log('cleared store');
};

request.onerror = event => {
  console.error('failed to clear store:', event);
};
```

#### get number of records in store

The `count` method can:

- get the number records in the store (no argument passed)
- determine if a record with a given key exists, returning 0 or 1
  (string key passed)
- get the number of records whose keys fall in a given range
  (instance of {% aTargetBlank
  "https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange",
  "IDBKeyRange" %} passed)

```js
const query = ...;
const request = store.count(query); // query is optional

request.onsuccess = event => {
  const count = request.result;
  console.log('count =', count);
};

request.onerror = event => {
  console.error('failed to count records:', event);
};
```

#### create index for store

```js
const breedIndex = store.createIndex('breed-index', ['breed'], {unique: false});

// This creates a "compound index".
const nameBreedIndex = store.createIndex(
  'name-breed-index',
  ['name', 'breed'],
  {unique: false}
);
```

#### delete records

```js
const request = store.delete('some-key'); // or IDBKeyRange

request.onsuccess = event => {
  console.log('records were deleted');
};

request.onerror = event => {
  console.error('failed to delete records:', event);
};
```

#### delete index

```js
store.deleteIndex('index-name');
```

#### get record with key

```js
const request = store.get('some-key');

request.onsuccess = event => {
  const record = request.result;
  console.log('record =', record);
};

request.onerror = event => {
  console.error('failed to get record:', event);
};
```

#### get records

The `getAll` method can:

- get all the records in a store (no argument passed)
- get the record with a given key (string key passed)
- get all the records whose keys fall in a given range
  (instance of {% aTargetBlank
  "https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange",
  "IDBKeyRange" %} passed)
  This is useful for pagination.

```js
const query = ...;
const request = store.getAll(query); // query is optional
// Optionally pass a count as the second argument
// to limit the number of records returned.

request.onsuccess = event => {
  const records = request.result; // an array
  console.log('records =', records);
};

request.onerror = event => {
  console.error('failed to get record:', event);
};
```

#### get keys

The `getAllKeys` method gets an array of all the keys of existing records
that fall in a specified {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange",
"IDBKeyRange" %}.

#### open index over all records

```js
const index = store.index('index-name');
index.openCursor().onsuccess = event => {
  const cursor = event.target.result;
  // See IDBCursor methods.
};
```

#### open cursor over records in a key range

```js
const request = store.openCursor();
request.onsuccess = event => {
  const cursor = event.target.result;
  // See IDBCursor methods.
};
```

#### open cursor to iterate over keys in a key range

The `openKeyCursor` method is similar to the `openCursor` method,
but iterates over keys instead of records.

#### upsert a record

If you have a cursor that refers to an existing record,
it is preferable to use the cursor method `update` instead of this.

```js
const request = store.put(newRecord);

request.onsuccess = event => {
  const key = request.result;
  console.log('upserted record with key', key);
};

request.onerror = event => {
  console.error('failed to upsert record:', event);
};
```

### IDBRequest

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBRequest",
"IDBRequest" %} interface represents the result of an asynchronous operation.
It is common to set the `onsuccess` and `onerror` properties
to a callback function that is passed an event object.

#### get result

```js
const result = request.result;
```

#### get error associated with request, if any

```js
const error = request.error;
```

#### get store or index associated with request

```js
const source = request.source;
```

#### get transaction associated with request

```js
const txn = request.transaction;
```

### IDBCursor

#### iterate over all records

```js
if (cursor) {
  const record = cursor.value;
  // Use record properties.
  cursor.continue();
} else {
  console.log('processed all records');
}
```

#### get store or index associated with cursor

```js
const source = cursor.source;
```

#### get key of current record

```js
const key = cursor.key;
```

#### get request that created cursor

```js
const request = cursor.request;
```

#### advance to next record

```js
cursor.continue();
```

#### advance to record with given key

```js
cursor.continue(key);
```

#### delete referenced record

```js
const request = cursor.delete();

request.onsuccess = event => {
  console.log('deleted record');
};

request.onerror = event => {
  console.error('failed to delete record:', event);
};
```

#### update referenced record

```js
const request = cursor.update(newRecord);

request.onsuccess = event => {
  console.log('updated record');
};

request.onerror = event => {
  console.error('failed to update record:', event);
};
```

### IDBIndex

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex",
"IDBIndex" %} interface provides efficient retrieval of records in a store
based on one or more properties of the records.

#### get name of index

```js
const name = index.name;
```

#### get key path of index

```js
const keyPath = index.keyPath;
```

#### determine if index is unique

```js
const unique = index.unique;
```

#### get store associated with index

```js
const store = index.objectStore;
```

### IDBKeyRange

The {% aTargetBlank
"https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange",
"IDBKeyRange" %} interface describes a range of keys.
It is used to retrieve matching keys or records from a store.

To create a range, call one of the following static methods:

- `lowerBound`: lower bound can be inclusive or exclusive; no upper bound
- `upperBound`: upper bound can be inclusive or exclusive; no lower bound
- `bound`: lower and upper bound can both be inclusive or exclusive
- `only`: single key

The properties of instances are `lower`, `upper`,
`lowerOpen` (`boolean`), and `upperOpen` (`boolean`)

#### test if a key is in range

```js
const included = range.includes(key);
```
