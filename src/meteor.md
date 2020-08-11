---
eleventyNavigation:
  key: Meteor
layout: topic-layout.njk
---

### Overview

Meteor ({% aTargetBlank 'https://meteor.com/', 'meteor.com' %})
is an open-source, full-stack, JavaScript
platform for building web and mobile (Android and iOS) applications.
It is referred to as a platform instead of a framework
because it works with other frameworks.
It is used to host both client-side and server-side code.

Meteor is built on Node.js.
It has good integration with the MongoDB NoSQL database.
It supports a publish and subscribe mechanism to synchronize
user interfaces (UIs) with backend data, providing real-time updates.

Meteor uses its own build systems and JavaScript bundler.
It does not use Webpack, Rollup, or Parcel.

Meteor has its own frontend framework called Blaze.
But it also supports other popular options that can be used in its place
such as Angular, React, Svelte, Vue, and Cordova (for Android and iOS apps).
(Note that on August 10, 2020 Adobe announced they
will no longer support development of Cordova.)

Meteor was initially released in 2012.
It gained immediate attention for its novel use of WebSockets
as an alternative to HTTP to achieve real-time updates.
But the attention faded quickly as other frameworks garnered more notice.
It is seeing some resurgence in 2020.

Tiny ({% aTargetBlank 'https://www.tinycapital.com/', 'tinycapital.com' %})
a Canadian technology holding company,
acquired Meteor from the Meteor Development Group in October, 2019.
This occurred after most of the Meteor Development Group team
transitioned to working on Apollo GraphQL.
The new Meteor company supports both Meteor and Galaxy.
Galaxy is a commercial cloud hosting platform for Meteor applications.

Companies that use Meteor include Qualcomm, Mazda, IKEA, and Honeywell.

Enterprise support is available in order to have
guaranteed support response times and service-level agreements (SLAs).

Meteor provides many command-line tools. These are summarized
{% aTargetBlank 'https://docs.meteor.com/commandline.html', 'here' %}.
Enter `meteor help` for help on using these.

### Key Benefits

The key benefits of using Meteor are:

- One install provides client, server (Node.js), and database (MongoDB) setup.
- Client and server code can be developed in a single language,
  JavaScript or TypeScript.
- The UI can be implemented using any popular web framework.
- User account management and authentication is provided.
  It even supports OAuth logins.
- Reactivity is provided through the use of
  `Tracker`, `ReactiveVar`, `ReactiveDict`, and `Session`.
- Changes to data in MongoDB collections can be published to all
  connected clients using WebSockets so UIs can stay in sync.
  Clients subscribe to publications of interest.
- There is no need to implement REST services or GraphQL queries
  if using Meteor Methods with WebSockets is acceptable.
  Meteor Methods are implemented in JavaScript or TypeScript
  and are described in detail later.

All of this enables implementing significant functionality
in a small amount of code.

### Installing

For platform-specific instructions on installing Meteor,
see {% aTargetBlank 'https://www.meteor.com/install', 'here' %}.
On Linux or macOS, enter `curl https://install.meteor.com | sh`.
On Windows, install Chocolatey and enter `choco install meteor`.

This installs several tools used by Meteor including MongoDB and TypeScript.

### Directory Structure

To create the initial directory structure for a new Meteor app
and run the starter app:

- Enter `meteor create {app-name}`
- Enter `cd {app-name}`
- Enter `meteor npm install`
- Enter `meteor`

This is covered in more detail later in the "Todo App" section.

The recommended directory structure for Meteor applications
is described below.

- `client`
  - `main.html`
  - `main.css`
  - `main.js`
- `server`
  - `main.js`
- `imports`
  - `\*.js`
- `public`

  This holds static assets such as images and fonts.
  When referencing these files, do so as if they were at the top level,
  not including `public/`.

- `private`

  This holds files that are only accessible from server code
  using the Assets API.

- `.meteor`

Additional files can be added to all the directories listed above
expect `.meteor`.
All client-side files should be placed in the `client` directory
and all server-side files should be placed in the `server` directory.
Files that are shared by client and server code
should be placed in the `imports` directory.
The `.meteor` directory holds files that Meteor uses for bookkeeping
and these files should not be manually modified.

In the past Meteor eagerly loaded files outside the `imports` directory
and lazily loaded files inside the `imports` directory.
As of Meteor 1.7 it no longer makes this distinction
and all code is lazily loaded.
So there is no longer a requirement to have an `imports` directory.
However, it is still a useful location for
files that are shared between client and server code.

### MongoDB

MongoDB is a popular, powerful NoSQL database.
There are many commands and options to master,
but the following presents some of the basics
for using the MongoDB console in conjunction with Meteor.

To start the MongoDB console, enter `meteor mongo`.

To see a list of all the databases, enter `show dbs`.

To use the Meteor database, enter `use meteor`.

To see a list of collections in the current database, enter `show collections`.

To see the first 20 documents in a given collection,
enter `db.{coll-name}.find()`.

To delete all the documents in a collection, enter `db.{coll-name}.drop()`.

To reset the "meteor" MongoDB database,
deleting all documents in all collections,
stop the Meteor server and enter `meteor reset`.
This can be useful to start an app in a pristine state.

### Collections

The client-side of Meteor applications can obtain data in many ways
including REST calls and GraphQL queries
that connect to any kind of database.
The data can be stored in clients in many ways including
JavaScript variables, session storage, and local storage.

The most common way for a Meteor application to store data is in "collections".
These are associated with a MongoDB collection.
They can be accessed from both client and server code.
When data is added to a collection, updated, or deleted,
client code that uses it typically updates automatically.
This is a significant benefit over using REST services
that require the use of polling to get data updates.

The following code demonstrates
creating a MongoDB collection named "Tasks",
inserting a document into the collection, and finding it.
MongoDB prefers for the unique id of each document
to be held in property named `_id`.

```js
const Tasks = new Mongo.Collection('tasks');
Tasks.insert({_id: 't1', text: 'buy milk'});
const task = Tasks.findOne({_id: 't1'});
```

The same code can be used in client or server code
when the Meteor "insecure" package is installed.
(Later we will look at securing Meteor applications.)
When run server-side, this code creates a MongoDB collection and populates it.
When run client-side, it creates a client-side cache
using the "Minimonogo" library which provides an
in-memory JavaScript implementation of the MongoDB API.

Client code must subscribe to a publication
in order to receive updates from the server.
This keeps the client-side cache (implemented by Minimongo)
in sync with a subset of the data in MongoDB on the server.
This is accomplished by using the Meteor Distributed Data Protocol (DDP)
to send WebSocket messages in both directions.

It is also possible to create a "local collection"
that only exists on the client or on the server.
These are Minimongo collections that are
not synchronized with a MongoDB collection.
They are useful for data that only resides in memory
in cases where using the MongoDB API to access it is convenient.
An alternative is to use a `ReactiveVar` or `ReactiveDict`.

To create a local collection:

```js
const myCollection = new Mongo.Collection(null);
```

or

```js
const myCollection = new Mongo.Collection('some-name', {connection: null});
```

To support associates between documents in different collections,
see the Meteor package
{% aTargetBlank 'https://atmospherejs.com/cultofcoders/grapher',
'cultofcoders:grapher' %}.

### Schemas for Collections

A schema can be associated with a collection
in order to provide validation when documents are added or updated.
They use the npm package
{% aTargetBlank 'https://github.com/aldeed/simpl-schema', 'simpl-schema' %}
which must be installed by entering `npm install simpl-schema`.

For example, a Tasks collection can be associated with a schema as follows:

```js
import SimpleSchema from 'simpl-schema';
Tasks.schema = new SimpleSchema({
  createdAt: {type: Date, defaultValue: new Date()},
  done: {type: Boolean, defaultValue: false, optional: true},
  owner: {type: String},
  text: {type: String},
  username: {type: String, optional: true}
});
```

Note that specifying a `defaultValue` for a property
does not imply that it is `optional`.
That must also be specified.

Schema validation is not performed automatically.
It is done by explicitly passing an object to the `validate` method.
For example, `Tasks.schema.validate(newTask)`.
If the object passed to `validate` does not match the schema,
this throws a `ValidationError` which describes the difference.
Note that extra properties not specified in the schema are not allowed.

The `collection2` Meteor package can be used to automatically validate
every insert, update, and upsert performed for a given collection.
In addition it coerces values to the expected types when possible,
removes properties not in the schema, and
assigns default values specified with the `defaultValue` property.
To install `collection2`, enter `meteor add aldeed:collection2`.
To use `collection2` on our `Tasks` collection:

```js
Tasks.attachSchema(Tasks.schema);
```

Creating collections whose documents can be deeply nested
or can contain large arrays is not recommended.
This is because DDP, which is used to send messages to clients
that describe new and updated documents,
is only able to describe top-level properties and their values.
For example, if a document contains a property whose value is a large array
and a single element is modified, DDP will send the entire array to clients.
This works fine, but is inefficient.

### Methods

Meteor Methods are functions that can reside only on the server
or on both the client and server.
They are meant to be called from client code.
The server instance is invoked using a
Remote Procedure Call (RPC) that utilizes WebSockets.
By convention, "Method" is written with a capital "M"
to distinguish it from normal JavaScript methods.

Meteor Methods are an alternative to REST calls implemented using HTTP.
An issue with using Meteor Methods instead of REST
is that they can only be called from the same Meteor app.

Common uses for Methods include
inserting a document in a collection,
deleting a document from a collection,
and updating a document in a collection.
There is typically no need to return anything to the client
because changes in the MongoDB database are communicated back to the client
through WebSocket messages that keep Minimongo in the client synchronized.
Retrieving documents from collections
is typically done by subscribing to them.

Methods can also be used to perform CRUD operations on SQL databases.
It this case it makes sense for the methods on only reside on the server
and for them to return data needed by the client.

To implement a Method,
import `Meteor` from the `meteor/meteor` package
and pass an object literal to `Meteor.methods`.
The properties of the object are Method definitions.
This registers the Methods with Meteor's DDP system.
The `Meteor.methods` function can be called any number of times
to register additional Methods.

The Methods can accept any number of parameters with any JavaScript types.

The `check` function in the `meteor/check` package
can be used to check the types of parameters at runtime.
Note that TypeScript types are only checked at compile-time.
The `check` function throws a `Match.Error` if unexpected types are passed.
For details on the `check` function,
see {% aTargetBlank 'https://docs.meteor.com/api/check.html', 'here' %}.

To report other kinds of parameter validation errors,
use `throw new ValidationError(message)`.

To report most other errors, use
`throw new Meteor.Error(errorId, message[, details])`.
The error id is typically the Method name
followed by a period and an error name.
If a generic JavaScript error is thrown instead using
`throw new Error(message)`, the details will only appear on the server
and clients will only be notified that an internal server error occurred.

To call a Method from client code,
import `Meteor` from the `meteor/meteor` package
and call `Meteor.call` passing it the name of a Method,
any number of arguments, and a callback function.
This triggers the Method call lifecycle which has six parts.

1. If the Method is defined on the client, it is executed there.
   This typically updates Minimongo and corresponding UI updates are made.
1. A JSON-based DDP message is constructed and sent to the server
   using a WebSocket connection.
   This includes a message name (`msg` property set to "method"),
   Method name (in the `method` property),
   arguments (in the `params` property),
   and a generated Method id (in the `id` property).
1. The Method executes on the server, possibly updating MongoDB.
1. A return value is sent to the client in a message that includes
   a message name (`msg` property set to "result"),
   the previously generated Method id (in the `id` property),
   and the result (in the `result` property).
1. Any affected data in Minimongo is updated, but the UI is not yet updated.
1. The callback function passed to `Meteor.call` is passed
   an error description (if there was an error) and the result value.
   Now the UI is updated using data from Minimongo.

If the call succeeds, the error description is `undefined`
and the result is set to whatever value the Method returns.
If the call fails, the error description is set to an object
that contains the properties
`error` (an HTTP status code even though HTTP isn't used),
`reason` (concise message), `message` (long message), and more.
It does not throw a JavaScript `Error`, so try/catch cannot be used.

Methods that throw should do so using
`throw new Meteor.error(methodName, message)`.
Throwing a normal JavaScript `Error` will not
return a detailed error message to the client.

Let's implement a simple Method that just adds two numbers.
It is not a typical Method because it does not update a collection.
We will see several examples of Methods that update a collection later.
To verify that the Method is called on both the client and server side,
a `console.log` call is added.
It will be output in the DevTools console AND
in the terminal window where the server is running.

Create the directory `imports` and
the file `imports/methods.js` containing the following:

```js
import {check} from 'meteor/check';
import {Meteor} from 'meteor/meteor';

Meteor.methods({
  sum(n1, n2) {
    console.log('sum called; on server?', Meteor.isServer);
    check(n1, Number); // argument type validation
    check(n2, Number);
    return n1 + n2;
  }
});
```

Add the following near the top of `server/main.js` to invoke the code above:

```js
import '../imports/methods.js';
```

To call the `sum` Method from client code, add the following
in any client JavaScript file such as `client/main.js`:

```js
const number1 = 2;
const number2 = 3;
Meteor.call('sum', number1, number2, (err, result) => {
  if (err) {
    alert(err);
  } else {
    console.log('sum =', result); // 5
  }
});
```

If you prefer to use promises, implement a utility function
that wraps calls in a `Promise` as follows:

```js
export function call(name, ...args) {
  return new Promise((resolve, reject) => {
    Meteor.call(name, ...args, (err, result) => {
      if (err) {
        reject(err);
      } else {
        resolve(result);
      }
    });
  });
}
```

An advantage of calling Methods in this way is that
the result of one call can be used to form the arguments of another call
without resorting to nested callback functions.

Using this new `call` function, our `sum` Method
can be called from an `async` function as follows:

```js
try {
  const sum = await call('sum', 2, 3);
  console.log('App.svelte addTask: sum =', sum);
} catch (e) {
  alert(e);
}
```

A Method can return a `Promise`.
Meteor will wait for the `Promise` to resolve or reject
before returning a result or an error to the client.

### Method Retries

If a client calls a Method and their internet connection is lost
before a result is returned, Meteor will remember this
and will make the call again when connectivity is restored.
This presents an issue when Methods are not idempotent.
Being idempotent means that the when a call is repeated,
no additional changes are made in the database.

Consider typical CRUD operations.

- Creating a new document in a collection using the same data multiple times
  is typically problematic and it's up to you to guard against this.
  For example, in a Todo app we could reject attempts
  to add multiple tasks with the same text.

- Retrieving a document from a collection multiple times is not a problem.

- Updating a document multiple times is not a problem as long as
  the document hasn't been modified using different data in the interim.
  This can be addressed by adding the last update timestamp to each document
  and verifying that it matches what is in the data for an update.
  When the timestamp does not match, we can assume the document
  has been updated since we last retrieved it and reject the update.

- Deleting a document multiple times is not a problem as long as
  attempts to delete a non-existent document are not treated as errors.

### Viewing WebSocket Messages

Chrome DevTools can be used to view the WebSocket messages
that are sent by Meteor.
Like HTTP messages, they appear on the Network tab.
But unlike HTTP messages where there is
a separate entry for each request/response pair,
there is only a single WebSocket entry per connection.
Selecting a connection displays all the messages sent over that connection.

To view WebSocket messages:

1. Open the Chrome DevTools.
1. Select the "Network" tab.
1. Click "WS" to filter on WebSockets.
1. Refresh the browser to get a new WebSocket connection.
1. Click the WebSocket connection that is displayed.
1. Click the "Messages" tab to see all the WebSocket messages.
   This list is updated as new ones are received.
1. Click a message to see its details.

Each message is an array of JSON objects that have
a `msg` property and other properties such as
`collection`, `fields`, `id`, `method`, `methods`, `msg`, and `params`.
Typically each message array contains a single object.

Each message is preceded by an arrow.
Outgoing messages have a green arrow pointing up
and are sent from the client to the server.
Incoming messages have a red arrow pointing down
and are sent from the server to the client.

Periodically, about every 45 seconds, each side sends
a "ping" message to verify that the other side is still reachable.
The other side replies with a "pong" message.

### Meteor WebSocket Messages

When a client initially connects to the server,
a series of messages are sent from the server to the client.
While I can't describe the purpose of each of these messages,
I can describe the pattern observed.
These messages include:

- one matching

  ```json
  {
    "msg": "connect",
    "version": "{some-version}",
    "support": [?]
  }
  ```

- several matching

  ```json
  {
    "msg": "sub",
    "id": "{some-id}",
    "name": "{some-name}",
    "params": [?]
  }
  ```

- one matching `{"server_id": "{some-id}"}`

- one matching `{"msg": "connected", "session": "{session-id}"}`

- several matching `{"msg": "ready", "subs": [?]}`

- several matching

  ```json
  {
    "msg": "added",
    "collection": "meteor_autoupdate_clientversions",
    "id": "version",
    "fields": {"version": "outdated}
  }
  ```

If the Meteor accounts packages are used
to support account creation and sign in,
a sign in request triggers a message from the client to the server
containing the following JSON:

```json
{
  "msg": "method",
  "method": "login",
  "params": [
    {
      "user": "{username}",
      "password": "{hashed-password}",
      "algorithm": "sha-256"
    }
  ]
}
```

A successful sign in triggers a message from the server to the client
containing the following JSON:

```json
{
  "msg": "result",
  "id": "{message-id}",
  "result": {
    "id": "{result-id}",
    "token": "{token}",
    "token-expires": {"$date": "{timestamp}"},
    "type": "password"
  }
}
```

Updates to the MongoDB database trigger additional JSON messages
from the server to the client that describe the changes.
This allows Minimongo in the client to stay in sync.
Each message has a `msg` property that specifies the type of update,
a `collection` property set to the name of the collection,
and an `id` property set to the `_id` value of the affected document.

When a document is added to a collection, `msg` is set to "added"
and `fields` set to a JSON object containing
all the properties of the document.

When a document is updated, `msg` is set to "changed"
and `fields` set to a JSON object containing
only the properties of the document that were modified.

When a document is deleted from a collection, `msg` is set to "remove".

If a client subscribes to a collection, "added" messages described above
are sent from the server to initially populate Minimongo.

### Optimistic UI

Optimistic UI is a feature of Meteor Methods, and also of Apollo GraphQL,
that enables a UI to respond to user interactions
without waiting for server responses.
This is satisfied by the following properties.

1. The UI is rendered on the client rather than
   waiting for the server to return HTML or data.
   This requires predicting the result of Method calls
   based on data that has been cached on the client.

1. The client-side cache (implemented by Minimongo)
   is a global cache rather than
   being associated with specific components,
   so it is not possible for components to disagree on the state.
   All affected components can re-render using the same data.
   When a MongoDB query is executed using a Meteor Method,
   it is first run against Minimongo on the client
   and then against MongoDB on the server.

1. The client subscribes to data using DDP to keep
   Minimongo on the client in sync with MongoDB on the server.
   Updates are made in real time, not using polling.

1. When the server returns the actual result of a Method call,
   Meteor verifies that it matches what the client predicted.
   If they differ, Meteor rolls back
   all the changes made from that point forward and
   replays them with the correct results from the server.
   UI changes triggered by Method calls are tracked
   in order to support the ability to rollback changes.

All of this functionality is provided by default.
The only requirement is for the client to
use Meteor Methods to request data changes.

The following sequence diagram illustrates the order of these operations:

![Meteor Method flow](/blog/assets/meteor-method-flow.png)

To see rollback in action, we can modify a Meteor Method to
return a different result when run on the client versus the server.
This is not typical, but it is useful to demonstrate optimistic UI.

Here is a simplified example from the upcoming Todo app.
It inserts a document in the "tasks" collection.
When run on the client updates Minimongo using the text passed as an argument.
When run on the server uses a timeout to wait three seconds
and then uses the uppercase version of the text.
The Method first runs on the client and
the UI renders a new task with the entered text.
The Method then runs on the server.
After the timeout it sends a message to the client that
tells it to update Minimongo using the uppercase version of the text.
The UI then updates to show the uppercase text.

```js
Meteor.methods({
  addTask(text) {
    check(text, String);
    return new Promise(resolve => {
      if (Meteor.isServer) {
        // Meteor requires using this version of setTimeout.
        // See https://docs.meteor.com/api/timers.html.
        Meteor.setTimeout(() => {
          // Tasks is a collection.
          const id = Tasks.insert({text: text.toUpperCase()});
          resolve(id);
        }, 3000);
      } else {
        const id = Tasks.insert({text});
        resolve(id);
      }
    });
  }
});
```

It is possible to define a Method only on the server-side,
thereby opting out of optimistic UI.
The UI will not update until the Method finishes executing on the server
and sends back a result message.
This is because Minimongo will not be updated
until the result message is received.

Here is an example of defining a Method on the server
taken from the upcoming Todo app.
It deletes a task with a given id.

```js
if (Meteor.isServer) {
  Meteor.methods({
    deleteTask(taskId) {
      check(taskId, String);
      Tasks.remove(taskId);
    }
  });
}
```

### Tracker

Tracker is a dependency tracking system used by Meteor to
update UIs when reactive variables and data sources change.
From the Meteor
{% aTargetBlank 'https://docs.meteor.com/api/tracker.html', 'Tracker docs' %},
"When you call a function that supports reactive updates
(such as a database query),
it automatically saves the current `Computation` object, if any
(representing, for example, the current template being rendered).
Later, when the data changes, the function can "invalidate" the `Computation`,
causing it to rerun (re-rendering the template)."

There is a low-level API for using Tracker,
but there are easier ways to use it in React and Svelte.

For React there is hook called `useTracker` for responding to tracker changes.

For Svelte, the
{% aTargetBlank 'https://atmospherejs.com/rdb/svelte-meteor-data', 'rdb/svelte-meteor-data' %}
provides a `useTracker` function.

For example, to always get the current user in a Svelte component:

```js
$: user = useTracker(() => Meteor.user());
```

The rdb/svelte-meteor-data package
also turns MongoDB cursor objects into Svelte stores
that automatically update whenever the database is updated
in a way that affects the query results.

For example, to always get the latest tasks from a `Task` collection:

```js
// A query can limit the documents returned,
// instead of getting all documents in the collection.
// It is not typically desirable to
// copy a large amount of data into Minimongo.
const query = {};
// A projection can limit the document properties returned
// and/or specify a sort order.
const projection = {sort: {createdAt: -1}}; // newest first
$: tasks = Tasks.find(query, projection);
```

Note that both `user` and `tasks` are Svelte stores,
so references to them should have a `$` prefix.

For more information on rdb/svelte-meteor-data, see this
{% aTargetBlank 'https://github.com/rdb/svelte-meteor-data/issues/6', 'issue' %}.

### ReactiveVar

A `ReactiveVar` is a client-side object holds a single value.
They can be created and exported from `.js` files
so they can be imported by multiple components.
They can also be passed as a prop to components.

To use this feature, add the `reactive-var` package
by entering `meteor add reactive-var`.

To create a new `ReactiveVar` object:

```js
import {ReactiveVar} from 'meteor/reactive-var';

const myVar = new ReactiveVar(initialValue);
```

To change the value, call `myVar.set(newValue)`.

To get the value, call `myVar.get()`.

Here is a Svelte component that demonstrates using a `ReactiveVar`:

```js
<script>
  import {ReactiveVar} from 'meteor/reactive-var';

  const myRV = new ReactiveVar(0);
  let counter;
  Tracker.autorun(() => {
    counter = myRV.get();
  });
</script>

<div>
  counter = {counter}
  <button on:click={() => myRV.set(counter + 1)}>Increment</button>
</div>
```

### ReactiveDict

A `ReactiveDict` is a client-side reactive data store
that holds key/value pairs.
They are used in the same ways a `ReactiveVar` objects,
but they can hold multiple values instead of just one.
The values can be any kind of JavaScript value that can be converted to JSON.
Other values can be made compatible by calling `EJSON.addType(name, factory)`.

Multiple instances can be created to
maintain separate groups of related key/value pairs.

To use this feature, add the `reactive-dict` package
by entering `meteor add reactive-dict`.

To create a new `ReactiveDict` object:

```js
import {ReactiveDict} from 'meteor/reactive-dict';

const myDict = new ReactiveDict(name, initialKeyValuePairs);
```

Both constructor arguments are optional.
Providing a name allows the key/value pairs to survive hot code pushes,
but not browser refreshes.
Providing an initial value avoids starting with no key/value pairs.

To set the default value of a `ReactiveDict` key,
call `ReactiveDict.setDefault(key, value)`.
Only the first call is processed. Subsequent calls are ignored.

To set a new value of a `ReactiveDict` key, call `Session.set(key, value)`.

To get the value of a `ReactiveDict` key, call `Session.get(key)`.

To get all the current key/value pairs, call `myDict.all()`.
To remove all the key/value pairs, call `myDict.clear()`.
These methods can also be called on the `Session` object.

### Session

The `Session` is a client-side, global `ReactiveDict` object.
It is typically used to share data between components.
The `Session` object cannot be shared between users or between browser tabs.

To use this feature, add the `session` package
by entering `meteor add session`.

To use `Session` in a component, import it with
`import {Session} from 'meteor/session;`

To set the default value of a session key, call
`Session.setDefault(key, value)`.

To set a new value for a session key, call `Session.set(key, value)`.

To get the value of a session key, call `Session.get(key)`.

To make session reactive so code is executed whenever
the value for a given key changes, use `Tracker.autorun` as follows:

```js
let someValue;
Tracker.autorun(() => {
  someValue = Session.get(key);
});
```

Here is a Svelte component that demonstrates using a `Session`:

```html
<script>
  import {Session} from 'meteor/session';
  import {Tracker} from 'meteor/tracker';

  let counter;
  Tracker.autorun(() => {
    counter = Session.get('counter');
  });
  Session.setDefault('counter', 0);

  function increment() {
    Session.set('counter', counter + 1);
  }
</script>

<div class="container">
  {counter}
  <button on:click="{increment}">Increment</button>
</div>
```

The ESLint plugin eslint-plugin-meteor will complain about
the use of `Session` based on the {% aTargetBlank
'https://github.com/dferber90/eslint-plugin-meteor/blob/master/docs/rules/no-session.md',
'meteor/no-session' %} rule.
Disable this ESLint rule if you decide to use `Session`.

### Email

TODO: Document the Meteor API for sending email.

### Meteor Packages

Meteor can use packages from npm and
from its own package repository called "Atmosphere".
Atmosphere contains packages that are specific to Meteor.

To see the available packages in Atmosphere,
browse {% aTargetBlank 'https://atmospherejs.com/', 'atmosphere.com' %}.
Packages without an author prefix followed by a colon
are official Meteor packages.

This page lists trending, recent, and most used packages.

To install a package from Atmosphere in your current Meteor project,
enter `meteor add {package-name}`.
This writes information about the installed packages to `.meteor/packages`
to track dependencies similar to how npm uses the `package-lock.json` file.
It also installs any CSS and JS files in the app
and does a "hot code push" so the effect is seen immediately,
unlike installing npm packages.

There are five types of packages:

1. `meteor-base`: This is a specific package that contains core components.
2. first-party packages: These are bundled with Meteor.
   Some are included by default, but can be removed.
   Others are not, but can be added.
3. local-packages: These are specific to your app
   and reside in the `packages` directory.
4. Atmosphere packages: These follow the `author:package` naming convention
   and are listed in the file `.meteor/packages`.
5. npm packages: These are listed as dependencies in `package.json`.

Popular Atmosphere packages include:

- accounts-password - "login service that enables secure password-based login"
- accounts-ui - "turn-key user interface for Meteor Accounts" using Blaze
- meteor-base - "default set of packages that almost every app will have"
- mongo - "adaptor for using MongoDB and Minimongo over DDP"
- rdb:svelte-meteor-data - "reactively track Meteor data inside Svelte components"
- react-meteor-data - "React hook for reactively tracking Meteor data"
- static-html - "define static page content in .html files"; alternative to Blaze
- svelte:compiler - compiles `.svelte` files to JavaScript
- svelte:blaze-integration - "render Blaze templates
  inside your Svelte components and vice versa";
  useful when using the account-ui package in a Svelte app
- tracker - "dependency tracker to allow reactive callbacks"
- typescript - "compiler plugin that compiles TypeScript and ECMAScript
  in .ts and .tsx files"

### ESLint

The steps to install and configure the ESLint linting tool
for use in a Meteor project are listed below.
They assume using Svelte as the web framework.
When this is not the case, the Svelte-specific parts can be omitted.

- Enter `npm install eslint eslint-plugin-import eslint-plugin-meteor eslint-plugin-svelte3`

- Add the following script in `package.json`:

  ```json
  "lint": "eslint --fix --quiet '{client,imports,server}/**/*.{js,svelte}'",
  ```

- Create the file `.eslintrc.json` containing the following:

  ```json
  {
    "env": {
      "browser": true,
      "es6": true,
      "jest": true,
      "node": true
    },
    "extends": [
      "eslint:recommended",
      "plugin:import/recommended",
      "plugin:meteor/recommended"
    ],
    "overrides": [
      {
        "files": ["**/*.svelte"],
        "processor": "svelte3/svelte3"
      }
    ],
    "parserOptions": {
      "ecmaVersion": 2019,
      "sourceType": "module"
    },
    "plugins": ["import", "svelte3"]
  }
  ```

ESLint will give "Unable to resolve path to module" errors
on all imports that begin with "meteor/".
However, the Meteor build system is able to resolve these.
To suppress these errors, add the following to your `.eslintrc.json` file:

```json
"rules": {
  "import/no-unresolved": ["error", {"ignore": ["^meteor/"]}]
}
```

### Prettier

The steps to install and configure the Prettier code formatting tool
for use in a Meteor project are listed below.
They assume using Svelte as the web framework.
When this is not the case, the Svelte-specific parts can be omitted.

- Enter `npm install prettier prettier-plugin-svelte`

- Add the following script in `package.json`:

  ```json
  "format": "prettier --write '{client,imports,server}/**/*.{css,html,js,svelte}'",
  ```

- Create the file `.prettierrc` containing
  personal preferences like the following:

  ```json
  {
    "arrowParens": "avoid",
    "bracketSpacing": false,
    "singleQuote": true,
    "svelteSortOrder": "scripts-markup-styles",
    "trailingComma": "none"
  }
  ```

### Meteor DevTools Evolved

The Chrome extension Meteor DevTools Evolved displays
DDP messages, Minimongo contents, and subscriptions.

To install it, browse
{% aTargetBlank
  'https://chrome.google.com/webstore/category/extensions', 'here' %}
and search by name.

### Tutorials

The Meteor web site contains several tutorials
{% aTargetBlank 'https://www.meteor.com/tutorials', 'here' %}
that provide introductions to using Meteor.

### Todo App

Let's walk through the steps to build a Todo app using Meteor.
This is a modified version of the Todo app presented at the Meteor site
that demonstrates additional features.
Code for the final version of this app can be found in
{% aTargetBlank 'https://github.com/mvolkmann/meteor-svelte-todos', 'GitHub' %}.

1. Create the app starting point by entering `meteor create todos`.

   We are using the default app template here.
   To use a non-default app template,
   add one of the following options after `create`:
   `--bare`, `--minimal`, `--full`, `--react`, or `--typescript`.
   Interestingly none of these options corresponds to the default.
   The default option produces applications that are insecure
   and are therefore only for prototyping.
   Such applications allow all MongoDB updates to be initiated from clients.
   For details on the Meteor packages included by default and with each option see
   {% aTargetBlank
     'https://docs.meteor.com/commandline.html#meteorcreate', 'here' %}.

1. Modify the `start` script in `package.json` as follows
   if you are not targeting older browsers or mobile devices:

   ```json
   "start": "meteor run --exclude-archs 'web.browser.legacy, web.cordova'",
   ```

1. Start the server by entering `cd todos` and `npm start`.

1. Browse localhost:3000 to see the following page:

   ![default Meteor app](/blog/assets/meteor-default-page.png)

1. Press the "Click Me" button multiple times.

   The text "You've pressed the button n times."
   will update to display the number of times it was clicked.

1. Exercise "hot code push".

   The UI is defined by the following files in the `client` directory:
   `main.html`, `main.css`, and `main.js`.
   Try editing the file `client/main.html`.
   The browser will updated automatically when the changes are saved.

1. Configure the app to use the Svelte web framework,
   instead of the default Blaze framework.

   Svelte is a good choice due to its use of reactive statements.
   For more information about Svelte, see
   {% aTargetBlank 'https://objectcomputing.com/resources/publications/sett/july-2019-web-dev-simplified-with-svelte', 'my article' %}.

1. Install Svelte by entering `meteor npm install svelte`

   It is recommended to use the command `meteor npm` instead of `npm`
   when installing npm packages in a Meteor app.
   When packages have binary dependencies,
   this ensures that they are built using the same C libraries.

1. Add some Meteor packages by entering  
   `meteor add svelte:compiler rdb:svelte-meteor-data`

1. Remove a package that will no longer be used by entering  
   `meteor remove blaze-html-templates`

1. Add a replacement Meteor package by entering  
   `meteor add static-html`

1. Replace the content of `client/main.html` with the following:

   ```html
   <head>
     <title>Todo App</title>
   </head>
   <body>
     <div id="app"></div>
   </body>
   ```

1. Replace the content of `client/main.js` with the following:

   ```js
   import {Meteor} from 'meteor/meteor';
   import App from './App.svelte';

   Meteor.startup(() => {
     new App({target: document.getElementById('app')});
   });
   ```

1. Create the file `client/Task.svelte` containing the following:

   ```html
   <script>
     export let task;

     const formatDate = date =>
       date.toLocaleDateString('en-us', {
         month: 'short',
         day: 'numeric',
         year: 'numeric'
       });

     // These are Svelte "reactive declarations".
     $: ({createdAt, text} = task);
     $: item = text + (createdAt ? ` - added ${formatDate(createdAt)}` : '');
   </script>

   <li>{item}</li>

   <style>
     li {
       padding-left: 0;
     }
   </style>
   ```

1. Create the file `client/App.svelte` containing the following:

   {% raw %}

   ```html
   <script>
     import Task from './Task.svelte';

     function getTasks() {
       const createdAt = new Date();
       return [
         {_id: 1, text: 'This is task 1', createdAt},
         {_id: 2, text: 'This is task 2', createdAt},
         {_id: 3, text: 'This is task 3', createdAt}
       ];
     }
   </script>
   <div class="container">
     <header>
       <h1>Todo App</h1>
     </header>
     <section>
       <ul>
         {#each getTasks() as task}
         <Task {task} />
         {/each}
       </ul>
     </section>
   </div>
   ```

   {% endraw %}

   At this point the app should display the following basic task list:

   ![Todo App after step 14](/blog/assets/meteor-todo-1.png)

1. Replace the contents of `client/main.css` with what is found at
   {% aTargetBlank
    'https://github.com/meteor/simple-todos-svelte/blob/master/client/main.css',
    'here' %}.
   Now the task list is styled nicely.

   ![Todo App after step 15](/blog/assets/meteor-todo-2.png)

1. Create a top-level project directory named `imports`.

1. Create the file `imports/tasks.js` containing the following
   which will create a MongoDB collection named "tasks":

   ```js
   import {Mongo} from 'meteor/mongo';

   export const Tasks = new Mongo.Collection('tasks');
   ```

1. Add the following import near the top of `server/main.js`.

   ```js
   import '../imports/tasks.js';
   ```

1. Add the following imports near the top of `client/App.svelte`:

   ```js
   import {Tasks} from '../imports/tasks.js';
   ```

1. Replace the `getTasks` function in `client/App.svelte` with the following
   in order to get tasks from MongoDB:

   ```js
   const query = {};
   const projection = {sort: {createdAt: -1}}; // newest first
   // This is a MongoDB cursor which is also a Svelte store.
   $: tasks = Tasks.find(query, projection);
   ```

1. Replace the call to `getTasks()` in `client/App.svelte` with `$tasks`.

   This is a reference to a Svelte store that is kept in sync
   with the MongoDB "tasks" collection.
   The UI will no longer display any tasks because
   this collection does not currently contain any tasks.

1. Insert some tasks using the MongoDB console by entering the following:

   ```bash
   meteor mongo
   db.tasks.insert({ text: "buy milk" })
   db.tasks.insert({ text: "take out trash" })
   ```

   Note that the UI updates automatically to show these new tasks.

   ![Todo App after step 22](/blog/assets/meteor-todo-3.png)

1. Enable adding tasks in the UI.

   - Add the following inside the `script` element in `client/App.svelte`:

     ```js
     let text = '';

     function addTask() {
       Tasks.insert({text, createdAt: new Date()});
       text = '';
     }
     ```

   - Add the following in `client/App.svelte` as the first child of
     the `section` element to prepare for adding tasks in the UI:

     ```html
     <form on:submit|preventDefault="{addTask}">
       <input placeholder="todo text" bind:value="{text}" />
       <button>Add</button>
     </form>
     ```

   - Add the following after the HTML in `client/App.svelte`:

     ```css
     <style>
       form {
         margin-top: 0;
         padding-bottom: 1rem;
       }

       section {
         padding: 1rem;
       }
     </style>
     ```

   Now new tasks can be added by entering text in the input and
   either pressing the "Add" button or pressing the return key.

   ![Todo App after step 25](/blog/assets/meteor-todo-4.png)

1. Note how Meteor keeps all clients in sync.

   To see this,
   open a second web browser or another window in the same web browser
   and browse localhost:3000.
   Add a task in either browser window and notice that it appears in both.

1. Add the ability to mark tasks as done and delete them
   by changing `client/Task.svelte` to match the following:

   ```html
   <script>
     import {Tasks} from '../imports/tasks.js';

     export let task;

     function deleteTask() {
       Tasks.remove(task._id);
     }

     const formatDate = date =>
       date.toLocaleDateString('en-us', {
         month: 'short',
         day: 'numeric',
         year: 'numeric'
       });

     function toggleDone() {
       Tasks.update(task._id, {$set: {done: !task.done}});
     }

     $: ({createdAt, text} = task);
     $: item = text + (createdAt ? ` - added ${formatDate(createdAt)}` : '');
   </script>

   <li>
     <input type="checkbox" checked="{task.done}" on:click="{toggleDone}" />
     <span class:done="{task.done}">{item}</span>
     <!-- Using Unicode trash can -->
     <button on:click="{deleteTask}">&#x1f5d1;</button>
   </li>

   <style>
     button {
       background-color: transparent;
       border: none;
     }

     .done {
       opacity: 0.3;
       text-decoration: line-through;
     }

     li {
       padding-left: 0;
     }
   </style>
   ```

   Now tasks can be marked as done by clicking the checkbox in front of them
   and they can be deleted by clicking the trash can icon after them.

   ![Todo App after step 26](/blog/assets/meteor-todo-5.png)

1. Show the number of remaining tasks and total tasks in the heading
   by making the following changes in `client/App.svelte`:

   - Add the following in the `script` element:

     ```js
     $: remaining = $tasks.filter(t => !t.done).length;
     ```

   - Add the following as the new first child of the `section` element:

     ```html
     <p class="stats">{remaining} of {$tasks.length} remaining</p>
     ```

   - Add the following inside the `style` element:

     ```css
     .stats {
       margin-top: 0;
     }
     ```

   Now when tasks are added, toggled between done and not done, and
   deleted, the number of remaining tasks and total tasks is updated.

   ![Todo App after step 27](/blog/assets/meteor-todo-6.png)

1. Add the ability to only display tasks that are not done
   by making the following changes in `client/App.svelte`:

   - Add the following variable declaration in the `script` element:

     ```js
     let hideCompleted = false;
     ```

   - Add the following inside the `form` element after the "Add" button:

     ```html
     <label className="hide-completed">
       <input type="checkbox" bind:checked="{hideCompleted}" />
       Hide Completed Tasks
     </label>
     ```

   {% raw %}

   - Change the content of the `{#each}` block to the following:

     ```html
     {#if !hideCompleted || !task.done}
     <Task {task} />
     {/if}
     ```

     {% endraw %}

   Now when the "Hide Completed Tasks" checkbox is checked,
   only tasks that are not done are displayed.

   ![Todo App after step 28](/blog/assets/meteor-todo-7.png)

1. Add support for user accounts by doing the following:

   - Add some Meteor packages by entering  
     `meteor add accounts-ui accounts-password svelte:blaze-integration`

   - Create the file `client/accounts-config.js` with the following content:

     ```js
     import {Accounts} from 'meteor/accounts-base';

     // Email is needed for the next step.
     Accounts.ui.config({
       passwordSignupFields: 'USERNAME_AND_EMAIL'
     });
     ```

   - Add the following near the top of `client/main.js`:

     ```js
     import './accounts-config.js';
     ```

   - Modify the `client/App.svelte` file to match the following:

     {% raw %}

     ```html
     <script>
       import {Meteor} from 'meteor/meteor';
       import {useTracker} from 'meteor/rdb:svelte-meteor-data';
       import {BlazeTemplate} from 'meteor/svelte:blaze-integration';
       import {Tasks} from '../imports/tasks.js';
       import Task from './Task.svelte';

       let hideCompleted = false;
       let text = '';
       let user;

       // user is a store
       $: user = useTracker(() => Meteor.user());

       const query = {};
       const projection = {sort: {createdAt: -1}}; // newest first
       // This is a MongoDB cursor which is also a Svelte store.
       $: tasks = Tasks.find(query, projection);

       $: remaining = $tasks.filter(t => !t.done).length;

       function addTask() {
         Tasks.insert({
           text,
           createdAt: new Date(),
           owner: Meteor.userId(),
           username: $user.username
         });
         text = '';
       }
     </script>

     <div class="container">
       <BlazeTemplate template="loginButtons" />

       <header>
         <h1>Todo App</h1>
       </header>

       <section>
         {#if $user}
         <p class="stats">{remaining} of {$tasks.length} remaining</p>

         <form on:submit|preventDefault="{addTask}">
           <input placeholder="todo text" bind:value="{text}" />
           <button>Add</button>
           <label className="hide-completed">
             <input type="checkbox" bind:checked="{hideCompleted}" />
             Hide Completed Tasks
           </label>
         </form>

         <ul>
           {#each $tasks as task} {#if !hideCompleted || !task.done}
           <Task {task} />
           {/if} {/each}
         </ul>
         {:else}
         <p>Please sign in.</p>
         {/if}
       </section>
     </div>

     <style>
       form {
         margin-top: 0;
         padding-bottom: 1rem;
       }

       section {
         padding: 1rem;
       }

       .stats {
         margin-top: 0;
       }

       :global(#login-buttons) {
         padding: 0.5rem;
         text-align: right;
       }

       :global(#login-buttons a) {
         text-decoration: none;
       }

       :global(#login-dropdown-list) {
         position: fixed;
         top: 50%;
         left: 50%;
         transform: translate(-50%, -50%);
       }
     </style>
     ```

     {% endraw %}

   Now users must create an account and sign in before they can
   view, add, and modify tasks.
   Information about user accounts is stored in the
   MongoDB database "meteor" inside the collection "users".

   Before a user signs in, they see a "Sign in" link in the upper-right.

   ![Todo App before sign in](/blog/assets/meteor-todo-before-sign-in.png)

   Clicking "Sign in" displays a dialog that
   contains a link to create an account.

   ![Todo App before sign in](/blog/assets/meteor-todo-sign-in.png)

   Clicking "Create account" changes the dialog
   to prompt for account information.
   By default, passwords must contain at least six characters,
   but there are no restrictions on the characters.
   There is not currently an easy way to customize password validation
   in order to enforce different rules.
   For possible solutions, see
   {% aTargetBlank
     'https://stackoverflow.com/questions/32651151/personalize-password-validation-function-used-by-accounts-ui',
     'here' %}.

   ![Todo App before sign in](/blog/assets/meteor-todo-create-account.png)

   After creating an account, or
   entering username and password for an existing account,
   the user is signed in and
   the "Sign in" link in the upper-right is replaced by their username.

   ![Todo App before sign in](/blog/assets/meteor-todo-after-sign-in.png)

   Clicking the username link in the upper-right
   presents a dialog for signing out or changing the password.

   ![Todo App before sign in](/blog/assets/meteor-todo-click-username.png)

   Clicking "Change password" changes the dialog to
   prompt for the current and new passwords.

   ![Todo App before sign in](/blog/assets/meteor-todo-change-password.png)

   After successfully changing the password,
   the dialog changes to notify the user.

   ![Todo App before sign in](/blog/assets/meteor-todo-password-changed.png)

1. Add support for "forgot password" emails.

   This will allow users to click the "Forgot password" link
   in the "Sign in" dialog which changes the dialog content
   to prompt for an email address.
   If a user account exists for the email address,
   an email is sent containing a link that the user can click
   to reset their password.
   After doing so they are immediately signed in.

   - Add another Meteor packages by entering  
     `meteor add email`.

   - Create the file `secrets.json` in the project root directory
     with content like the following:

     ```json
     {
       "MAIL_NAME": "Your Name",
       "MAIL_USER": "some-user",
       "MAIL_PASSWORD": "some-password",
       "MAIL_USER_DOMAIN": "some-company.com",
       "MAIL_SERVER_DOMAIN": "smtp.gmail.com",
       "MAIL_SERVER_PORT": 587
     }
     ```

     Note that the last two values above are specific to using
     a Google G Suite account for sending email.
     The values will differ if you are using a different email service.
     Another service to consider is
     {% aTargetBlank 'https://mailchimp.com/', 'MailChimp' %}.

   - If you are saving the project in a Git repository,
     add `secrets.json` to the `.gitignore` file
     so it is not added to the repository for others to see.

   - Create the file `server/accounts-setup.js` with the following content:

     ```js
     import {Accounts} from 'meteor/accounts-base';
     import secrets from '../secrets.json';

     const siteName = 'Todos by Mark';
     Accounts.emailTemplates.siteName = siteName;

     const from = `${secrets.MAIL_NAME}<${secrets.MAIL_USER}@${secrets.MAIL_USER_DOMAIN}>`;
     Accounts.emailTemplates.from = from;
     ```

   - Modify the file `server/main.js` to match the following:

     ```js
     import {Meteor} from 'meteor/meteor';
     import '../imports/tasks.js';
     import './accounts-setup.js';
     import secrets from '../secrets.json';

     Meteor.startup(() => {
       // code to run on server at startup
       process.env.MAIL_URL =
         'smtp://' +
         secrets.MAIL_USER +
         '%40' +
         secrets.MAIL_USER_DOMAIN +
         ':' +
         secrets.MAIL_PASSWORD +
         '@' +
         secrets.MAIL_SERVER_DOMAIN +
         ':' +
         secrets.MAIL_SERVER_PORT;
     });
     ```

   Now when I use clicks the "Forgot password" link in the sign in dialog,
   they can enter their email address and press the "Reset password" button.

   ![Todo App forgot password](/blog/assets/meteor-todo-forgot-password.png)

   When the email is sent, the dialog changes to include the text "Email sent".

   ![Todo App forgot password email sent](/blog/assets/meteor-todo-forgot-password-email-sent.png)

   The user will receive an email like the following:

   ![Todo App password reset email](/blog/assets/meteor-todo-password-reset-email.png)

   Clicking the link opens the app in a new browser tab
   and displays a dialog for resetting the password.

   ![Todo App password reset](/blog/assets/meteor-todo-password-reset.png)

   After entering a new password, the user is notified that it has been reset
   and that they are now logged in.

   ![Todo App after password reset](/blog/assets/meteor-todo-after-password-reset.png)

1. Make the app more secure by moving database interactions to the server.

   This is accomplished by implementing Meteor Methods
   that are invoked by client-side code
   with calls to `Meteor.call(name, data, callback)`.

   - Remove the Meteor package that allows client code to
     interact with MongoDB by entering `meteor remove insecure`

   - Define Methods by modifying `imports/tasks.js` to match the following:

     ```js
     import {check} from 'meteor/check';
     import {Meteor} from 'meteor/meteor';
     import {Mongo} from 'meteor/mongo';

     export const Tasks = new Mongo.Collection('tasks');

     Meteor.methods({
       addTask(text) {
         check(text, String); // argument type validation

         // Make sure the user is logged in before inserting a task.
         if (!this.userId) throw new Meteor.Error('add-task', 'not-authorized');

         const {username} = Meteor.users.findOne(this.userId);
         const id = Tasks.insert({
           text,
           createdAt: new Date(),
           owner: this.userId,
           username
         });
         return id;
       },

       deleteTask(taskId) {
         check(taskId, String); // argument type validation
         Tasks.remove(taskId);
       },

       setDone(taskId, done) {
         check(taskId, String); // argument type validation
         check(done, Boolean); // argument type validation
         Tasks.update(taskId, {$set: {done}});
       }
     });
     ```

   - Create the file `client/util.js` containing the following error handling:

     ```js
     export function handleError(err) {
       // Replace this will better error handling later.
       if (err) alert(err.message);
     }
     ```

   - Add the following `import` in `client/App.svelte` and `client/Task.svelte`:

     ```js
     import {handleError} from './util.js';
     ```

   - Change the call to `Tasks.insert` in the `addTask` function
     of `client/App.svelte` to the following:

     ```js
     Meteor.call('addTask', text, handleError);
     ```

   - Remove the import of `Tasks` from `client/Task.svelte`
     since it is no longer used and add the following import:

     ```js
     import {Meteor} from 'meteor/meteor';
     ```

   - Change the call to `Tasks.remove` in the `deleteTask` function
     of `client/Task.svelte` to the following:

     ```js
     Meteor.call('deleteTask', task._id, handleError);
     ```

   - Change the call to `Tasks.update` in the `toggleDone` function
     of `client/Task.svelte` to the following:

     ```js
     Meteor.call('setDone', task._id, !task.done, handleError);
     ```

   Nothing will change in the UI. But client-side code
   can no longer directly modify the MongoDB database.

1. Explicitly specify what data the server sends to the client
   so we can separate tasks by user.

   Tasks will still be stored in the same MongoDB collection,
   but each user will only see and operate on the tasks they created.

   - Remove the ability for the server to
     send any MongoDB content requested by clients.
     Currently the app includes the `autopublish` package.
     This shares every collection and every document in these
     with every connected client.
     This is convenient for prototyping, but not desired for production.

     Enter `meteor remove autopublish`

   - Publish only tasks that belong to the logged in user
     by adding the following in `imports/tasks.js`
     after the line that starts with `export const Tasks`

     ```js
     if (Meteor.isServer) {
       // This is only run on the server.
       // An arrow function cannot be used here
       // because we need to use the "this" keyword.
       // If no query is provided, the entire collection is published.
       // A second argument can be passed to the find method which is
       // an object where the keys are names of properties.
       // The values are booleans that either specify which properties
       // to include (true) or which to exclude (false), but not both.
       Meteor.publish('tasks', function () {
         return Tasks.find({owner: this.userId});
       });
     }
     ```

   - Add the following `import` in `client/App.svelte`:

     ```js
     import {onMount} from 'svelte';
     ```

   - Add the following in the `script` element of `client/App.svelte`
     to subscribe to all tasks that are published:

     ```js
     // The name passed to subscribe must match a name passed to publish.
     onMount(() => Meteor.subscribe('tasks'));
     ```

     The `Meteor.subscribe` method returns an object
     that can be useful to capture in a variable.
     It has three properties.
     `stop` is a method that can be called to cancel the subscription.
     Typically the subscription is retained for the duration of the session.
     `ready` is a method that is called when the server
     has completed the initial publication of data.
     `subscriptionId` is a unique id for the subscription
     that is not typically needed.

     Additional arguments can be passed to `Meteor.subscribe`.
     These are passed the function passed to `Meteor.publish` in the server
     and it can uses them to decide which documents to publish.
     This is useful to limit the amount of data that is
     copied into Minimongo on the client.

   Now users only see and operate on their own tasks.

1. Require account email validation before users
   can view and operate on their tasks.

   - Add the following at the bottom of `server/accounts-setup.js`:

     ```js
     Accounts.onLogin(({user}) => {
       const [email] = user.emails;
       if (!email.verified) {
         Accounts.sendVerificationEmail(user._id, email.address);
       }
     });

     Accounts.emailTemplates.verifyEmail = {
       subject() {
         return `Activate your ${siteName} account.`;
       },
       text(user, url) {
         return (
           `Hey ${user.username}, click the following link ` +
           `to verify your email address: ${url}.`
         );
       }
     };
     ```

   - Determine if the logged in user has a verified email address by
     adding the following in `client/App.svelte` after the `$: user =` line:

     ```js
     $: emailVerified = $user && $user.emails[0].verified;
     ```

   {% raw %}

   - Change `{#if $user}` to `{#if $user && emailVerified}`

   - Add the following before the `{:else}` line:

     ```js
     {:else if $user && !emailVerified}
       <p>
         You have been sent an email containing a link to verify your account.
         Please click that link in order to start adding tasks.
       </p>
     ```

   {% endraw %}

   Now after a user creates a new account, they will see the following:

   ![Todo App verify email sent](/blog/assets/meteor-todo-verify-email-sent.png)

   The user must must verify their email address by clicking a link
   in an email they receive like the following
   before they can start adding tasks.

   ![Todo App verify email](/blog/assets/meteor-todo-verify-email.png)

   Clicking the link in the email opens a browser tab containing the following:

   ![Todo App email verified](/blog/assets/meteor-todo-email-verified.png)

   The user can dismiss the dialog and proceed with using the app.

   That's it! We now have a functioning Todo app
   that supports user accounts and tasks are persisted in a database.
   I can't imagine doing all of this in less code
   using anything other than Meteor!

### Routing

For apps that need client-side page routing there are two popular libraries:

- {% aTargetBlank 'https://github.com/kadirahq/flow-router', 'FlowRouter' %}
  (with flow-router-extra)
- {% aTargetBlank 'https://github.com/iron-meteor/iron-router', 'Iron.Router' %}

### Meteor Shell

The `meteor shell` command launches a REPL that is used to
examine and interact with the server side of a Meteor app.
TODO: How is it used?

### Building and Deploying

Now that we have our app running locally,
we are ready to deploy it to a server so real users can access it.

#### MongoDB Hosting

If your Meteor app uses MongoDB, and most do,
the first step is to select a hosting platform.
One option is
{% aTargetBlank 'https://www.mongodb.com/cloud/atlas', 'MongoDB Atlas' %}.
Another option is {% aTargetBlank 'https://mlab.com/', 'mLab' %}.

MongoDB Atlas has an endless free tier referred to as "Shared Clusters"
that does not provide backup.

To configure MongoDB Atlas:

- Browse
  {% aTargetBlank 'https://www.mongodb.com/cloud/atlas', 'here' %}.
- Choose between AWS, GCP, and Azure for the cloud provider.
- Create a cluster.
- In the left nav. under "SECURITY", click "Database Access"
  and press "Add New Database User".
- Optionally change the default username and password.
- Select "Database User Privileges" from the drop-down.
  You will likely want "Read and write to any database".
- Press "Add User".
- In the left nav. under "SECURITY", click "Network Access"
  and press "ADD IP ADDRESS".
- Enter IP addresses that should have access.
  You will likely want to add the IP address of your machine
  AND the IP address of the Galaxy server.

To view databases and collections:

- In the left nav. under "DATA STORAGE", click "Clusters".
- Press "COLLECTIONS".
- Select a database names to see the collections in it.
- Select a collection name to see the documents in it.
- Hover over a document and click the pencil icon to modify the document.
  Press the "UPDATE" button when finished.
  It can take about five seconds for Meteor apps that use the document
  to update.
- Hover over a document and click the trash can icon to delete the document.

To connect from the mongo shell:

- In the left nav. under "DATA STORAGE", click "Clusters".
- Press "CONNECT".
- Click "Connect with the mongo shell".
- If you do not already have the mongo shell installed,
  click "I do not have the mongo shell installed"
  and follow the platform-specific instructions to install it.
- Otherwise, click "I have the mongo shell installed".
- Press "Copy" after the supplied connection string.
- Open a terminal window.
- Paste the connection string.
- Replace "<dbname>" with the name of an existing database and press return.
- Enter the password.
- Enter "show collections" to list the collections within the database.

To connect from the MongoDB Compass app:

- In the left nav. under "DATA STORAGE", click "Clusters".
- Press "CONNECT".
- Click "Connect using MongoDB Compass".
- If you do not already have MongoDB Compass installed,
  click "I do not have MongoDB Compass"
  and follow the platform-specific instructions to install it.
- Otherwise, click "I have MongoDB Compass".
- Press "Copy" after the supplied connection string.
- Open the MongoDB Compass app.
- Paste the connection string.
- Replace "<password>" with the password and press "CONNECT".
- Click a database name to see the collections inside it.

#### Configuring a Cloud Server

There are many options to choose from including
AWS, Google Cloud Platform (GCP), and Azure.

An option that is fairly simple with a low cost is DigitalOcean.

You will want to associate a domain name with your DigitalOcean server
so you can enable SSL encryption later.
There are many domain registrars to choose from.
A popular, low cost option is
{% aTargetBlank 'https://hover.com', 'hover.com' %}.
You can also get a free domain name from
{% aTargetBlank 'https://www.freenom.com/', 'freenom.com' %}.

To associate a domain name with a DigitalOcean project, browse
{% aTargetBlank 'https://cloud.digitalocean.com', 'cloud.digitalocean.com' %},
sign in, click "Networking" in the left nav., enter a domain for a project,
and press "Add Domain".
Then under "Create new record", select "A", enter "@" for "HOSTNAME",
enter your DigitalOcean server IP address for "WILL DIRECT TO",
and press "Create Record".
Repeat this using "www" instead of "@" for "HOSTNAME".
You can also create A records from the domain registrar site such as Hover.

After ssh'ing to your instance for the first time,
it is helping to edit your `.bashrc` file.
Some things you may want to add include:

```bash
# Make it easier to clear the screen.
alias cls='clear'

# Ask for confirmation before overwriting or deleting files.
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

# Make the delete key work.
export TERM=xterm-256color

# Set environment variables that the Meteor server will need.
export MONGO_URL='{your-mongodb-url}'
export ROOT_URL=https://{ip-address-of-this-server}:{port-of-meteor-server}
```

To find processes that are listening on ports, enter `netstat -tulpn`.
My DigitalOcean server had nginx listening on port 80
and node listening on port 3000.
A different port can be used for the Meteor server.

To enable accessing you MongoDB Atlas server:

- Browse cloud.mongodb.com and log in.
- In the left nav. under "SECURITY", click "Network Access".
- Press "ADD IP ADDRESS".
- Add the IP address of your cloud server.

TODO: mup works now! Try to get it to use HTTPS!
TODO: Get Galaxy to work.
TODO: Get manual deploy to work.

#### Enabling HTTPS

To determine the version of Ubuntu you are using, enter `lsb_release -a`.
For me this is 20.04.

DigitalOcean provides an nginx proxy that is started by default.

To enable nginx to act as a proxy for a server that listens on a particular port:

- Enter `sudo touch /etc/nginx/conf.d/sysmon.conf`
- Enter `sudo vim /etc/nginx/conf.d/sysmon.conf`
- Add the following:

  {% raw %}

  ```text
  server {
    listen 1919; # node port
    server_name sysmon.{your-domain};

    location / {
      proxy_set_header   X-Forwarded-For $remote_addr;
      proxy_set_header   Host $http_host;
      proxy_pass         http://{node-ip}:{node-port};
    }
  }
  ```

  {% endraw %}

- Enter `sudo systemctl restart nginx`
- Now you should be able to browse https://{your-domain}:{node-port}
- TODO: THIS DID NOT WORK!
  There is a probably a conflict with
  `/etc/nginx/sites-available/{your-domain}` below.

Setup nginx "server blocks" by following the steps
{% aTargetBlank
  'https://www.digitalocean.com/community/tutorials/how-to-install-nginx-on-ubuntu-20-04#step-5-%E2%80%93-setting-up-server-blocks-(recommended)',
  'here' %} which are summaries below:

1. Enter `sudo mkdir -p /var/www/{your-domain}/html`
1. Enter `sudo chown -R $USER:$USER /var/www/{your-domain}/html`
1. Enter `sudo chmod -R 755 /var/www/{your-domain}`
1. Enter `vim /var/www/{your-domain}/html/index.html`
1. Add content like the following for testing your site:

   {% raw %}

   ```html
   <html>
     <head>
       <title>Welcome to {your-site}!</title>
     </head>
     <body>
       <h1>Success, the {your-site} server block is working!</h1>
     </body>
   </html>
   ```

   {% endraw %}

1. Enter `sudo vim /etc/nginx/sites-available/{your-domain}`
1. Add content like the following to serve your site:

   ```text
   server {
     listen 80;
     listen [::]:80;

     root /var/www/your_domain/html;
     index index.html index.htm index.nginx-debian.html;

     server_name your_domain www.your_domain;

     location / {
       try_files $uri $uri/ =404;
     }
   }
   ```

1. Enter `sudo ln -s /etc/nginx/sites-available/{your-domain} /etc/nginx/sites-enabled/`
1. Enter `sudo vim /etc/nginx/nginx.conf`
1. Uncomment the line `server_names_hash_bucket_size 64;`
1. Enter `sudo nginx -t` to verify that there are
   no syntax errors in the edited files.
1. Enter `sudo systemctl restart nginx` to restart nginx.
1. Browse http://{your-domain} to verify that nginx can server the earlier HTML.

Enable SSL by following the steps
{% aTargetBlank
  'https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-20-04',
  'here' %}.
These instructions specific to Ubuntu version 20.04.
It uses a "certbot" which will automatically renew the certificate
before it expires.

1. Enter `sudo apt install python3-certbot-nginx`
1. Enter `sudo ufw status` to see that network traffic is currently allowed.
1. If "Nginx FULL" is not set to "ALLOW" ...
   - Enter `sudo ufw allow 'Nginx Full'`
   - Enter `sudo ufw delete allow 'Nginx HTTP'`
     because it is implied by `Nginx Full`.
1. Enter `cd` to change to your home directory.
1. Enter `sudo certbot --nginx -d {your-site} -d www.{your-site}`
   to obtain an SSL certificate.
   You will be prompted to enter your email,
   agree to the LetsEncrypt terms of service,
   optionally receive email from the Electronic Freedom Foundation,
   and whether HTTP requests should be redirected to HTTPS.

Your certificate and chain are saved at
`/etc/letsencrypt/live/{your-site}/fullchain.pem`.
Your key file is saved at `/etc/letsencrypt/live/mvolkmann.site/privkey.pem`.
Normally your cert will expire in three months any you need to obtain a
new version by running the `certbot` command again with the `--certonly` option.
You can also non-interactively renew all of your certificates
by entering `certbot renew`.
But certbot makes this unnecessary and will automatically renew your cert.
Make a backup of the `/etc/letsencrypt` directory.

You can now browse `https://{your-domain}`.

#### Manual Deploy

To build a Meteor app for deployment:

- cd to the root project directory.
- You may need to enter `meteor remove-platform ios`
  before running the next command.
- Enter `meteor build {dir-name} --architecture {a-name}`
  where `{a-name}` is replaced by the target architecture
  such as `os.linux.x86_64`.

This creates the specified directory if it doesn't exist and creates
a TAR file named `{project-name}.tar.gz` inside it.
The TAR file contains a single directory named `bundle` which contains
all the files for the client-side and server-side of the application.

For help on the build command, enter `meteor help build`.

To deploy the app:

1. Copy the TAR file to a server, perhaps using the `scp` command.
   For example, `scp {project-name}.tar.gz root@{server-ip-address}:.`
1. Start a terminal session on the server using the `ssh` command.
   For example, `ssh root@{server-ip-address}`.
1. Verify that a recent version of Node.js is installed
   by entering `node -v`.
1. Create a directory to receive the contents of the tar file
   by entering `mkdir {project-name}`
1. Un-tar the file by entering `tar zxf {project-name}.tar.gz -C {project-name}`
1. Enter `cd {project-name}/bundle/programs/server`
1. Enter `npm install`
1. Enter `cd ../..`
1. Set the `MONGO_URL` environment variable to your MongoDB server URL.
   For example, `mongodb+srv://{username}:{password}@cluster0.tqq7g.mongodb.net/meteor`
1. Set the `ROOT_URL` environment variable to your HTTP server URL.
   For example, `http://{server-ip-address}:{server-port}`
   For example, `http://{server-ip-address}:4000`
1. Expose the port that will be used.
   With DigitalOcean this is done by entering `sudo ufw allow {port}/tcp`.
   Meteor uses port 3000 by default, but my DigitalOcean instance
   has an HTTP server that is always listening on that port,
   so choose another such as 4000.
1. Enter `node main.js` to start the server.
   TODO: Do you need to do something to tell it to use port 4000
   TODO: or does it get that from the ROOT_URL environment variable?

This gives the error MongoNetworkError: failed to connect to server
[cluster0-shard-00-00.tqq7g.mongodb.net:27017] on first connect
[MongoNetworkError: connection 5 to
cluster0-shard-00-00.tqq7g.mongodb.net:27017 closed

Other alternatives for deploying Meteor apps include
Galaxy ({% aTargetBlank 'https://www.meteor.com/hosting', 'here' %})
and Meteor Up ({% aTargetBlank 'http://meteor-up.com/', 'here' %}).
Meteor Up, a.k.a mup, can be used to deploy a Meteor app
to any server to which you can `ssh`.

#### Meteor Up (mup)

Meteor Up uses Docker to deploy Meteor apps.

The steps to deploy a Meteor app using Meteor Up are:

- Enter `npm install -g mup`
- cd to the root project directory.
- Enter `mup init` to create the files `mup.json` and `settings.json`
  in the `./deploy` directory.
- Add `.deploy/mup.js` and `.deploy/settings.json` to `.gitignore`.
- Edit `.deploy/mup.js`.
  - Set `servers.one.host` to the IP address of the server.
  - Set `servers.one.username` to an account username, possibly `'root'`.
  - Set `servers.one.password` to an account password.
  - Set `app.name` to the app name.
  - Set `app.path` to `'../'`
  - Set `app.env.ROOT_URL` to the app URL.
    For example, `'http://{server-ip-address}.{server-port}'`
  - Set `app.env.MONGO_URL` to the URL of your MongoDB server.
  - Set `app.env.PORT` to the value of `server-port`.
  - To enable SSL:
    - See {% aTargetBlank
        'https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-18-04',
        'How to Secure Nginx' %}
    - This requires a domain name, not just an ip address.
    - Set `proxy.domains` to `'https://{server-ip-address}:80'`, // nginx proxy
    - Set `proxy.ssl.forceSSL` to `true`.
    - Set `proxy.ssl.letsEncryptEmail` to `'email@domain.com'`.
  - Delete the top-level `mongo` property.
- Enter `cd .deploy`
- Enter `mup setup`
- If you get the error "endpoint mup-nginx-proxy not found",
  this is not your first time running `mup` for this project,
  and you have made certain changes to your `mup.js` file,
  you may need to enter `mup stop` and `mup reconfig` instead of `mup setup`.
- Enter `mup deploy`
  (This works in Node 12, but may not yet work in newer versions.)

THIS HANGS FOREVER!

#### Galaxy

Galaxy provides a free, one-month trial.

Galaxy does not provided MongoDB hosting.

To use Galaxy:

- Browse {% aTargetBlank 'https://galaxy.meteor.com', 'galaxy.meteor.com' %}
- Press "SIGN IN WITH METEOR" and authenticate.
- Create a free trial account
- Choose a region (ex. "US-East-1")
- Open a terminal window and cd to the root project directory.
- Create the file `settings.json` containing the following:

  {% raw %}

  ```json
  {
    "galaxy.meteor.com": {
      "env": {
        "MONGO_URL": "mongodb://{username}:{password}@{mongo-host}:27017/meteor?authSource=admin&ssl=true",
        "MAIL_URL": "smtp://volkmannm@objectcomputing.com:$&oBDvwWTTms9d6dxYDG@smtp.gmail.com:587"
      }
    }
  }
  ```

  {% endraw %}

- Add `settings.json` to `.gitignore`.
- Enter `DEPLOY_HOSTNAME=us-east-1.galaxy-deploy.meteor.com \`
  `meteor deploy {hostname} --settings settings.json`
  where `{hostname}` can be "{username}.meteorapp.com".

### Resources

- {% aTargetBlank 'https://meteor.com/', 'Meteor home page' %}
- {% aTargetBlank 'https://guide.meteor.com/', 'Meteor Guide' %}
- {% aTargetBlank 'https://docs.meteor.com/', 'Meteor API docs' %}
- {% aTargetBlank 'https://github.com/Urigo/awesome-meteor', 'Awesome Meteor' %}
  curated list of packages and libraries
- {% aTargetBlank 'https://atmospherejs.com/', 'Atmosphere Meteor package repository' %}
- {% aTargetBlank 'https://forums.meteor.com/', 'Meteor Forum' %}
- {% aTargetBlank 'https://stackoverflow.com/questions/tagged/meteor', 'Stack Overflow' %}
