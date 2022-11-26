---
eleventyNavigation:
  key: MongoDB
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

MongoDB is currently the most popular NoSQL database.
Unlike relational databases that use SQL and are organized
into tables that contains rows, and rows that contain columns,
MongoDB stores _documents_ in _collections_.
Each collection can have multiple indexes to make queries faster.
These indexes are implemented as {% aTargetBlank
"https://en.wikipedia.org/wiki/B-tree", "B-tree" %} data structures.
The documents are JSON objects that are
stored in a binary JSON format called BSON.

Unlike relational databases that
define the structure of a database with a schema, MongoDB does not
use a schema to restrict what can be stored in a database.
This speeds up development when the structure changes often,
since no schema changes are required.
It also allows properties present in the objects of a collection to vary.

In practice, all the documents that are added to a
given collection typically have the same structure.
For example, documents describing a person
might all have the following structure:

```js
{
  _id: ObjectId("5e4984b33c9533dfdf102ac8"),
  firstName: "Mark",
  lastName: "Volkmann",
  address: {
    street: "123 Some Street",
    city: "Somewhere",
    state: "Missouri",
    zip: 12345
  },
  favoriteColors: ["yellow", "orange"]
}
```

Each document has an `_id` property that
holds a unique identifier within its collection.
Documents in other collections can have properties that refer to these.
This can be used to simulate what are
called _joins_ in relational databases.

This post provides enough detail to enable
implementing CRUD operations on collections of data.
It does not cover more advanced topics, such as
query optimization, replication, sharding, backups, and security.
For detail on these and many other MongoDB topics,
see the Manning book {% aTargetBlank
"https://www.manning.com/books/mongodb-in-action", "MongoDB in Action" %}.

We’ll look at how to perform various actions from JavaScript code.

## Installing MongoDB

There are multiple ways to install MongoDB,
and they vary based on the operating system of the computer.
For details, see the {% aTargetBlank
"https://docs.mongodb.com/guides/server/install/", "MongoDB documentation" %}.
We will look at one way of installing for each operating system.

### Windows

Follow these steps to install MongoDB on Windows:

1. Browse https://www.mongodb.com/try/download/community.
1. Click Server at the top to bypass
   requesting a free trial of MongoDB Atlas.
1. In the Version drop-down, select the version labeled “current release”.
1. In the OS drop-down, select “Windows x64”.
1. In the Package drop-down, select “msi”.
1. Click the Download button.
1. Double-click the downloaded `.msi` file
   and follow the installer instructions.
1. Open a command prompt window.
1. Create the MongoDB data directory by entering `md \data\db`.

This will install MongoDB as a Windows service.
It will consume resources even when not in use.
When it’s not needed, the service can be stopped or it can be uninstalled.

### Linux

Follow these steps to install MongoDB on Linux:

1. Browse to https://www.mongodb.com/try/download/community.
1. Click Server at the top to bypass
   requesting a free trial of MongoDB Atlas.
1. In the Version drop-down, select the version labeled “current release”.
1. In the OS drop-down, select your version of Linux.
1. In the Package drop-down, select “TGZ”.
1. Click the Download button.
1. Open a terminal window.
1. `cd` to the the directory where the `.tgz` file was downloaded.
1. Enter `sudo tar -C /opt -xf {file-name}.tgz`.
1. Add the bin subdirectory of this directory to the `PATH` environment variable.
   For example, `/opt/mongodb-linux-x86_64-ubuntu1604-4.2.3/bin`.
1. Create the MongoDB data directory by entering `sudo mkdir -p /data/db`.
1. Set permissions on this directory by entering `sudo chmod +rw /data/db`.

### macOS

One option for installing on macOS is to follow the same
basic steps as for Linux.
Another approach is as follows:

1. Install {% aTargetBlank "https://brew.sh/", "Homebrew" %}.
1. Enter `brew tap mongodb/brew`.
1. Enter `brew install mongodb-community`.

## Starting the database server

The steps for starting a MongoDB server
vary based on the operating system of the computer.

- On Windows, open a command prompt and enter `mongod`.

- On Linux, open a terminal window and enter `mongod`.

- On macOS, open a terminal window and
  enter `mongod --config /usr/local/etc/mongod.conf --fork`.

## MongoDB shell

MongoDB shell is a kind of REPL that supports using
JavaScript to interact with a MongoDB database.

There are also many free tools that provide a GUI
for performing the same operations.
Some to consider can be found at
{% aTargetBlank "www.guru99.com/top-20-mongodb-tools.html", "here" %}.
One great option is {% aTargetBlank
"https://www.mongodb.com/products/compass", "MongoDB Compass" %}.

To start the MongoDB shell, enter `mongo`.
This displays a prompt where JavaScript-based MongoDB commands can be entered.

The table below is a cheat sheet of commands that can be entered
to perform common operations.
The commands found in it use the following placeholders:

- `{db}` represents the name of a database.
- `{coll}` represents the name of a collection.
- `{obj}` represents a JavaScript object that describes a document.

It is best to use valid JavaScript names for collections.
For example, avoid including dashes in their names.

| Action                                                            | Command                                                                                        |
| ----------------------------------------------------------------- | ---------------------------------------------------------------------------------------------- |
| List existing databases                                           | `show dbs`                                                                                     |
| Create a new database                                             | None; databases are automatically<br>created when a collection is added                        |
| Make a given database the current one                             | `use {db}`                                                                                     |
| Show the current database                                         | `db.getName()`                                                                                 |
| Delete the current database                                       | `db.dropDatabase()`                                                                            |
| List the collections in the current database                      | `show collections` or `db.getCollectionNames()`                                                |
| Create a collection in the current database                       | `db.createCollection('{coll}')`                                                                |
| Add a document to a collection                                    | `db.{coll}.insert({obj})`                                                                      |
| Get the number of documents in a collection                       | `db.{coll}.find().count()` or<br>`db.{coll}.find().length()`                                   |
| Output the first 20 documents in a collection                     | `db.{coll}.find()`                                                                             |
| Output the next 20 documents in a collection                      | `db.{coll}.find().skip(20)`                                                                    |
| Output the first 20 documents in a collection that match criteria | `db.{coll}.find({criteria})`                                                                   |
| Delete one document from a collection that matches criteria       | `db.{coll}.deleteOne({criteria})`                                                              |
| Delete all documents from a collection that match criteria        | `db.{coll}.deleteMany({criteria})`                                                             |
| Update one document in a collection that matches criteria         | `db.{coll}.updateOne({criteria}, {$set: {updates}})`                                           |
| Update all documents in a collection that match criteria          | `db.{coll}.updateMany({criteria}, {$set: {updates}})`                                          |
| Add an index to a collection                                      | `db.{coll}.createIndex({ {prop-name}: 1 });`<br>1 for ascending order, -1 for descending order |
| Delete a collection                                               | `db.{coll}.drop()`                                                                             |
| Exit from the shell                                               | `exit`                                                                                         |

The current database starts as "test", even if no such database exists yet.

Here is an example session:

```js
// We don’t have an “animals” database yet.
show dbs

// A database doesn’t have to exist to “use” it.
use animals

db.createCollection('dogs')

// Now we have an “animals” database because we created a collection in it.
show dbs

db.createCollection('cats')

// The “animals” database now contains the collections “dogs” and “cats”.
db.getCollectionNames();

db.dogs.insert({breed: 'Whippet', name: 'Dasher'})
db.dogs.insert({breed: 'TWC', name: 'Maisey'})
db.dogs.insert({breed: 'NAID', name: 'Ramsay'})
db.dogs.insert({breed: 'GSP', name: 'Oscar'})

// This outputs the first 20 documents in the “dogs” collection.
db.dogs.find()

// This outputs only the dogs with a breed of “Whippet”.
db.dogs.find({breed: 'Whippet'})

// This deletes all the dogs with a breed of “Whippet”.
db.dogs.deleteMany({breed: 'Whippet'})

<8> This shows that the collection no longer contains Whippets.
db.dogs.find()

// This changes the name of all dogs with a breed of “GSP”.
db.dogs.update({breed: 'GSP'}, {$set: {name: 'Oscar Wilde'}})

// This changes the name of the one dog with a given ID.
db.dogs.update({_id: ObjectId('some-id')}, {$set: {name: 'Oscar Wilder'}})

// This deletes the “dogs” collection.
db.dogs.drop()

// This shows that we no longer have a “dogs” collection.
db.getCollectionNames()

// This deletes the “animals” database.
db.dropDatabase()

// This shows that we no longer have an “animals” database.
show dbs
```

## MongoDB Compass

TODO: Add more detail.

To find documents based on multiple property values,
use a query like the following:

```text
{ type: 3, itemNumber: '' }
```

For documents that have date properties,
to find those from today and newer
use a query like the following:

```text
{ createdAt: { $gt: new Date('2022-11-25') } }
```

To find documents with a property that matches a regular expression,
use a query like the following:

```text
{ itemNumber: { $regex: /^Comet/ } }
```

## MongoDB from JavaScript

Now that we have seen how to use the MongoDB shell,
let’s perform the same operations from a JavaScript program.

There are several open source libraries for using MongoDB
from various programming languages.
The official library for Node.js is simply called `mongodb`.
To install it, enter `npm install mongodb`.

The following code performs all the same operations
shown in the previous MongoDB shell session.

```js
const MongoClient = require('mongodb').MongoClient;

// MongoDB thinks localhost is a different database instance than 127.0.0.1.
// The mongo shell uses 127.0.0.1, so we use that to hit the same instance.
// I thought maybe this was an issue with my /etc/host file,
// but I commented out all the lines that associated 127.0.0.1
// with something other than localhost and it didn't change anything.
//const url = 'mongodb://localhost:27017';
const url = 'mongodb://127.0.0.1:27017';

// These are recommended MongoDB options to avoid deprecation warnings.
const options = {useNewUrlParser: true, useUnifiedTopology: true};

async function logCollection(coll) {
  let result = await coll.find().toArray();
  console.log(coll.collectionName, 'contains', result);
}

async function logCollections(db) {
  const items = await db.listCollections().toArray();
  console.log(
    'collections are',
    items.map(item => item.name)
  );
}

async function logDatabases(client) {
  const dbs = await client.db().admin().listDatabases();
  console.log(
    'databases are',
    dbs.databases.map(db => db.name)
  );
}

// Until we have support for top level await in JavaScript,
// all uses of the "await" keyword must be in an "async" function.
async function doIt() {
  let client;
  try {
    client = await MongoClient.connect(url, options);
    // Show that we do not yet have an "animals" database.
    await logDatabases(client);

    // Use the "animals" database.
    const db = client.db('animals');

    // Create two collections in the "animals" database.
    const dogs = await db.createCollection('dogs');
    const cats = await db.createCollection('cats');

    // Show that we now have an "animals" database.
    await logDatabases(client);

    // Show that the collections were created.
    await logCollections(db);

    // Add four documents to the "dogs" collection.
    await dogs.insertOne({breed: 'Whippet', name: 'Dasher'});
    await dogs.insertOne({breed: 'TWC', name: 'Maisey'});
    await dogs.insertOne({breed: 'NAID', name: 'Ramsay'});
    await dogs.insertOne({breed: 'GSP', name: 'Oscar'});

    // Show that there are four documents in the "dogs" collection.
    const count = await dogs.countDocuments();
    console.log('dog count =', count);

    // Show the documents in the "dogs" collection.
    await logCollection(dogs);

    // Find all the Whippets in the "dogs" collection.
    result = await dogs.find({breed: 'Whippet'}).toArray();
    console.log('whippets are', result);

    // Delete all the Whippets from the "dogs" collection.
    console.log('deleting Whippets');
    await dogs.deleteMany({breed: 'Whippet'});

    // Show that the "dogs" collection no longer contains Whippets.
    await logCollection(dogs);

    // Update the name of all GSPs in the "dogs" collection.
    console.log('updating GSP name');
    await dogs.updateMany({breed: 'GSP'}, {$set: {name: 'Oscar Wilde'}});
    await logCollection(dogs);

    // Find a specific dog in the "dogs" collection.
    const dog = await dogs.findOne({name: 'Oscar Wilde'});

    // Update the name of a specific dog in the "dogs" collection.
    await dogs.updateOne({_id: dog._id}, {$set: {name: 'Oscar Wilder'}});
    await logCollection(dogs);

    // Delete the "dogs" collection.
    await dogs.drop();

    // Show that the "animals" database
    // no longer contains a "dogs" collection.
    logCollections(db);

    // Delete the "animals" database.
    await db.dropDatabase();

    // Show that the "animals" database no longer exists.
    await logDatabases(client);
  } catch (e) {
    console.error(e);
  } finally {
    if (client) client.close();
  }
}

doIt();
```
