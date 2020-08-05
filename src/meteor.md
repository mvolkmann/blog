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

- Client and server code can be developed in a single language,
  JavaScript or TypeScript.
- The UI can be implemented using any popular web framework.
- There is no need to implement user account management and authentication
  if using the Meteor account-ui and accounts-password packages
  is acceptable.
- There is no need to install and configure a database
  if using MongoDB is acceptable.
- Changes to data in MongoDB collections are published to all
  connected clients using WebSockets so all the UIs can stay in sync.
- There is no need to install and configure an HTTP server library.
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

All client-side files can be placed in the `client` directory
and all server-side files can be placed in the `server` directory.

In the past Meteor eagerly loaded files outside the `imports` directory
and lazily loaded files inside the `imports` directory.
As of Meteor 1.7 it no longer makes this distinction
and all code is lazily loaded.
So there is no longer a requirement to have an `imports` directory.
However, it is still a useful location for
files that are shared between client and server code.

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
creating a MongoDB collection named "Todos",
inserting a document into the collection, and finding it.
MongoDB prefers for the unique id of each document
to be held in property named `_id`.

```js
const Todos = new Mongo.Collection('todos');
Todos.insert({_id: 't1', text: 'buy milk'});
const todo = Todos.findOne({_id: 't1'});
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

### Methods

Meteor Methods are functions that typically
reside on both the client-side and server-side.
By convention, "Method" is written with a capital "M"
to distinguish it from normal JavaScript methods.
Methods are meant to be called from client-side code.
Server-side implementations are invoked using a
Remote Procedure Call (RPC) that utilizes WebSockets.

Meteor Methods are an alternative to REST calls implemented using HTTP.
An issue with using Meteor Methods instead of REST
is that they can only be called from Meteor apps.

Common uses for Methods include
inserting a document in a collection,
deleting a document from a collection,
and updating a document in a collection.
Retrieving documents from collections
is typically done by subscribing to them.

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
update UIs when session variables and data sources change.
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
$: tasks = Tasks.find(query, projection);
```

Note that both `user` and `tasks` are Svelte stores,
so references to them should have a `$` prefix.

### Meteor Packages

Meteor can use packages from npm and
from its own package repository called "Atmosphere".
Atmosphere contains packages that are specific to Meteor.

To see the available packages in Atmosphere,
browse {% aTargetBlank 'https://atmospherejs.com/', 'atmosphere.com' %}.

This page lists trending, recent, and most used packages.

To install a package from Atmosphere in your current Meteor project,
enter `meteor add {package-name}`.
This writes information about the installed packages to `.meteor/packages`
to track dependencies similar to how npm uses the `package-lock.json` file.

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

- Enter `npm install eslint eslint-plugin-import eslint-plugin-svelte3`

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
    "extends": ["eslint:recommended", "plugin:import/recommended"],
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

1. Start the server by entering `cd todos` and `meteor`.

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

   Before a user signs in, they see a "Sign in" link in the upper-right.

   ![Todo App before sign in](/blog/assets/meteor-todo-before-sign-in.png)

   Clicking "Sign in" displays a dialog that
   contains a link to create an account.

   ![Todo App before sign in](/blog/assets/meteor-todo-sign-in.png)

   Clicking "Create account" changes the dialog
   to prompt for account information.

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

   TODO: How can password rules be specified?

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

   - Remove the ability for the server to send any MongoDB content
     requested by clients.

     Enter `meteor remove autopublish`

   - Publish only tasks that belong to the logged in user
     by adding the following in `imports/tasks.js`
     after the line that starts with `export const Tasks`

     ```js
     if (Meteor.isServer) {
       // This is only run on the server.
       // An arrow function cannot be used here
       // because we need to use the "this" keyword.
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
     onMount(() => Meteor.subscribe('tasks'));
     ```

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

### Building and Deploying

Now that we have our app running locally,
we are ready to deploy it to a server for real users can access it.

To build a Meteor app for deployment,
cd to the root project directory and enter `meteor build {dir-name}`.
This creates the specified directory if it doesn't exist
and creates a TAR file inside the directory named `{project-name}.tar.gz`.
The TAR file contains a single directory named `bundle` which contains
all the files for the client-side and server-side of the application.
Include the `--architecture` option to build for a specific architecture.
For help on the build command, enter `meteor help build`.

To deploy the app:

1. Copy the TAR file to a server, perhaps using the `scp` command.
1. Start a terminal session on the server using the `ssh` command.
1. Verify that a recent version of Node.js is installed.
1. Un-tar the file by entering `tar zxf {project-name}.tar.gz`
1. Enter `cd {project-name}/programs/server`
1. Enter `npm install`
1. Set the `MONGO_URL` environment variable to `mongodb://localhost:27017/myapp`
1. Set the `ROOT_URL` environment variable to `http://myapp.com`
1. Enter `node main.js` to start the server.

TODO: What should the value of "myapp" be in the environment variables.

Other alternatives for deploying Meteor apps include
Galaxy ({% aTargetBlank 'https://www.meteor.com/hosting', 'here' %})
and Meteor Up ({% aTargetBlank 'http://meteor-up.com/', 'here' %}).
Meteor Up, a.k.a mup, can be used to deploy a Meteor app
to any server to which you can `ssh`.

### Resources

- ({% aTargetBlank 'https://meteor.com/', 'Meteor home page' %})
- ({% aTargetBlank 'https://docs.meteor.com/', 'Meteor API docs' %})
- {% aTargetBlank 'https://github.com/Urigo/awesome-meteor', 'Awesome Meteor' %}
  curated list of packages and libraries
- {% aTargetBlank 'https://atmospherejs.com/', 'Atmosphere Meteor package repository' %}
- {% aTargetBlank 'https://forums.meteor.com/', 'Meteor Forum' %}
- {% aTargetBlank 'https://stackoverflow.com/questions/tagged/meteor', 'Stack Overflow' %}
