---
eleventyNavigation:
  key: Meteor
layout: topic-layout.njk
---

### Overview

Meteor (<https://www.meteor.com/>) is an open-source, full-stack, JavaScript
platform for building web and mobile (Android and iOS) applications.
It is referred to as a platform instead of a framework
because it works with other frameworks.
It is used to host both client-side and server-side code.

Meteor is built on Node.js.
It has good integration with the MongoDB NoSQL database.
It supports a publish and subscribe mechanism to synchronize
user interfaces with backend data, providing real-time updates.

Meteor uses its own build systems and JavaScript bundler.
It does not use Webpack, Rollup, or Parcel.

A big benefit of Meteor is the ability to implement
significant functionality in a small amount of code.
One example of this is user sign up and authentication.
This is demonstrated in the sample app below.

Meteor has its own frontend framework called Blaze.
But it also supports other popular options that can be used in its place
such as Angular, React, Svelte, Vue, and Cordova.

Meteor was initially released in 2012.
It gained immediate attention for its novel use of WebSockets
as an alternative to HTTP to achieve real-time updates.
But the attention faded quickly as other frameworks garnered more notice.
It is seeing some resurgence in 2020.

Tiny (<https://www.tinycapital.com/>), a Canadian technology holding company,
acquired Meteor from the Meteor Development Group in October, 2019.
This occurred after most of the Meteor Development Group team
had transitioned to working on Apollo GraphQL.
The new Meteor company supports both Meteor and Galaxy.
Galaxy is a commercial cloud hosting platform for Meteor applications.

Companies that use Meteor include
Qualcomm, Mazda, IKEA, and Honeywell.

Enterprise support is available in order to have
guaranteed support response times and SLAs.

Meteor provides many command-line tools.
These are summarized at <https://docs.meteor.com/commandline.html>.
Enter `meteor help` for help on using these.

### Installing Meteor

See <https://www.meteor.com/install> for
platform-specific instructions on installing Meteor.
In Linux or macOS, enter `curl https://install.meteor.com | sh`.
In Windows, install Chocolatey and enter `choco install meteor`.

This installs several tools used by Meteor including MongoDB and TypeScript.

## Meteor Application Directory Structure

The recommended directory structure for Meteor applications
is described below.

- `client`
  - `main.html`
  - `main.css`
  - `main.js`
- `server`
  - `main.js`
- `imports`
  - `client`
    - `\*.js`
  - `server`
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

### Blaze Web Framework

TODO: Learn something about this

### Collections

Meteor stores data in "collections" that can be accessed from both
client-side and server-side code.
When data is added to a collection or existing data is modified,
client code that uses it updates automatically.

Collections are associated with a MongoDB collection.
Meteor apps can access other databases (such as PostgreSQL)
through calls to REST services,
but real-time updates only occur through collections
and those only use MongoDB.

The following code demonstrates
creating a MongoDB collection named "Todos",
inserting a document into the collection, and finding it.

```js
const Todos = new Mongo.Collection('todos');
Todos.insert({_id: 't1', text: 'buy milk'});
const todo = Todos.findOne({_id: 't1'});
```

The same code can be used in server-side or client-side code.
When run server-side, it actually creates a collection and populates it.
When run client-side, it creates a client-side cache
using the "Minimonogo" library which provides an
in-memory JavaScript implementation of the MongoDB API.

Client code must subscribe to a publication
in order to receive updates from the server.
This keeps the client-side cache in sync
with a subset of the data on the server.
It uses the Meteor Distributed Data Protocol (DDP)
to send data in both directions.

### MongoDB

To see a list of all the databases, enter `show dbs`.

To use the MongoDB console, enter `meteor mongo`.

To use the Meteor database, enter `use meteor`.

To see a list of collections in this database, enter `show collections`.

To see the first 20 documents in a given collection,
enter `db.{coll-name}.find()`.

To delete all the documents in a collection, enter `db.{coll-name}.drop()`.

### Methods

Meteor Methods are functions that typically
reside on both the client-side and server-side.
They are meant to be called from client-side code.
Server-side implementations are invoked using a
Remote Procedure Call (RPC) that uses WebSockets.
This is alternative to REST calls implemented using HTTP.

Common uses for methods include
inserting documents in collections,
deleting documents from collections,
and updating documents in collection.
Retrieving documents from collections
is typically done by subscribing to them.

To implement a method in server-side code,
pass an object literal to `Meteor.methods`
where the properties are method definitions.
This registers the methods with Meteor's DDP system.
It can be called any number of times to register additional methods.

The methods can have any number of parameters with any JavaScript types.

The `check` function in the `meteor/check` package
can be used to check the types of parameters and runtime.
Note that TypeScript types are only checked at compile-time.
It throws a `Match.Error` if unexpected types are passed.
For details on the `check` function,
see <https://docs.meteor.com/api/check.html>.
To report other parameter validation errors,
use `throw new ValidationError(message)`.

For most other errors, use
`throw new Meteor.Error(errorId, message[, details])`.
The error id is typically the method name
followed by a period and an error name.
If instead a generic JavaScript error is thrown using
`throw new Error(message)`, the details will only appear on the server
and clients will only be notified that an internal server error occurred.

To call a method from client code,
import `Meteor` from the `meteor/meteor` package
and call `Meteor.call` passing it the name of a Method,
arguments, and a callback function.
This triggers the method call lifecycle which has six parts.

1. If the method is defined on the client, it is executed there,
   typically updating Minimongo,
   and corresponding UI updates are made.
1. A JSON-based DDP message is constructed and sent to the server.
   This includes the method name, arguments, and a generated method id.
1. The method executes on the server, possibly updating MongoDB.
1. A return value is sent to the client in a "result" message
   that includes the previously generated method id and the result.
1. Any affected data in Minimongo is updated, but the UI is not yet updated.
1. The callback function passed to `Meteor.call` is passed
   an error description (if there was an error) and the result.
   Now the UI is updated using data from Minimongo.

Here is an example of a trivial method that simply adds two numbers.
This is not a typical method because it does not update a collection.
To verify that it is called on both the client and server side,
a `console.log` call is added.
It will be output in the DevTools console AND
in the terminal window where the server is running.

Create the file `imports/methods.js` containing the following:

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
import '../imports/methods';
```

To call the `sum` method from client code, add the following:

```js
const number1 = 2;
const number2 = 3;
Meteor.call('sum', number1, number2, (err, result) => {
  if (err) {
    // Handle the error.
  } else {
    console.log('sum =', result); // 5
  }
});
```

If you prefer to use promises, create a utility function
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

An advantage of calling methods in this way is that
the result of one call can be used to form the arguments of another call
without resorting to nested callback functions.

Using this new `call` function, our `sum` Meteor method
can be called from an `async` function as follows:

```js
try {
  const sum = await call('sum', 2, 3);
  console.log('App.svelte addTask: sum =', sum);
} catch (e) {
  // Handle the error.
}
```

A Meteor Method can return a `Promise`.
It will wait for the `Promise` to resolve or reject
before returning a result or error to the client.

### Viewing WebSocket Messages

Chrome DevTools can be used to view the WebSocket messages
that are sent by Meteor.
Like HTTP messages, they appear on the Network tab.
But unlike HTTP messages where there is a separate entry for each
request/response pair,
there is only a single WebSocket entry per connection.
Selecting a connection displays all the messages sent over that connection.

To view WebSocket messages:

1. Open the Chrome DevTools.
1. Select the "Network" tab.
1. Click "WS" to filter on WebSockets.
1. Refresh the browser.
1. Click the WebSocket connection that is displayed.
1. Click the "Messages" tab to see all the WebSocket messages,
   updated as new ones are received.
1. Click a message to see its details.

Each message is an array of JSON objects that have
a `msg` property and other properties that may include
`collection`, `fields`, `id`, `method`, `methods`, `msg`, and `params`.

### Optimistic UI

Optimistic UI is a feature of Meteor, and also of Apollo GraphQL,
that enables a UI to respond to user interactions
without waiting for server responses.
This is satisfied by the following sequence.

1. The UI is rendered on the client rather than
   waiting for the server to return HTML or data.
   This requires predicting the result of operations
   based on data that has been cached on the client.
1. This cached data is in a global cache rather than
   being associated with specific components,
   so it is not possible for them to disagree on the state
   and all affected components can re-render.
   Meteor uses Minimongo for this.
   When a MongoDB query is executed using a Meteor Method,
   it is first run against Minimongo on the client
   and then against MongoDB on the server.
1. The client subscribes to data using DDP to keep
   Minimongo on the client in sync with MongoDB on the server.
   Updates are made in real time, not using polling.
1. When the server returns the actual result of an operation,
   Meteor verifies that it matches what the client predicted.
   If they differ, Meteor rolls back
   all the changes made from that point forward and
   replays them with the correct results from the server.
   UI changes triggered by Meteor Method calls are tracked
   and this supports the ability to rollback.

All of this functionality is provided by default.
The only requirement is for the client to
use Meteor Methods to request data changes.

To see this in action, we can modify a Meteor Method to
return a different result when run on the client versus the server.
This is not typical, but it is useful to demonstrate optimistic UI.

Here is a simplified example from the upcoming Todo app.
It inserts a document in the "tasks" collection.
When run on the client it uses the text passed as an argument.
When run on the server it waits three seconds
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

Here is an example from the upcoming Todo app.
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
update user interfaces when session variables and data sources change.
From <https://docs.meteor.com/api/tracker.html>,
"When you call a function that supports reactive updates
(such as a database query),
it automatically saves the current Computation object, if any
(representing, for example, the current template being rendered).
Later, when the data changes, the function can "invalidate" the Computation,
causing it to rerun (re-rendering the template)."

There is a low-level API for using Tracker,
but there are easier ways to use it in React and Svelte.

For React there is hook called `useTracker` for responding to tracker changes.

For Svelte there is a corresponding `useTracker` function
at <https://atmospherejs.com/rdb/svelte-meteor-data>.
This takes a function that returns the result of a MongoDB query.
It returns a Svelte store that is updated whenever
the database is updated in a way that affects the query results.

To always get the current user in a Svelte component:

```js
$: user = useTracker(() => Meteor.user());
```

To always get the latest tasks from a Task collection:

```js
$: tasks = useTracker(() => Tasks.find(query, projection).fetch());
```

Note that both `user` and `tasks` are stores,
so references to them should have a `$` prefix.

### Meteor Packages

Meteor can use packages from npm and
from its own package repository called "Atmosphere".
To see the available packages in Atmosphere, browse <https://atmospherejs.com/>.
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
  inside your Svelte components and vice versa"
- tracker - "dependency tracker to allow reactive callbacks"
- typescript - "compiler plugin that compiles TypeScript and ECMAScript
  in .ts and .tsx files"

### ESLint

To use ESLint in a Meteor project that uses Svelte:

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
    "plugins": ["import", "svelte3"],
    "rules": {
      "import/no-unresolved": ["error", {"ignore": ["^meteor/"]}]
    }
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

To use Prettier in a Meteor project that uses Svelte:

- Enter `npm install prettier prettier-plugin-svelte`

- Add the following script in `package.json`:

  ```json
  "format": "prettier --write '{client,imports,server}/**/*.{css,html,js,svelte}'",
  ```

- Create the file `.prettierrc` containing the following:

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
that provide an introduction to using Meteor.
See <https://www.meteor.com/tutorials>.

Let's walk through the steps to build a Todo app using Meteor.

Create the app starting point by entering
`meteor create todos`.
To use a non-default app templates, add one of the following options
after `create`:
`--bare`, `--minimal`, `--full`, `--react`, or `--typescript`
Interestingly none of these options corresponds to the default.
The default option produces applications that are insecure
and are therefore only for prototyping.
They allow all MongoDB updates to be initiated from clients.
For details on the features included by default and with each option
see <https://docs.meteor.com/commandline.html#meteorcreate>.

Enter `cd todos`.
Run the app by entering `meteor`.
Browse localhost:3000.
The following page will be rendered.

![cover](/blog/assets/meteor-default-page.png)

Press the "Click Me" button.
The text "You've pressed the button n times."
will update to display the number of times it was clicked.

The UI is defined by the following files in the `client` directory:
`main.html`, `main.css`, and `main.js`.

Try editing the file `client/main.html`.
The browser will updated automatically.
Meteor refers to this as "hot code push".

Now let's configure the app to use Svelte as its web framework,
instead of the default Blaze framework.
Svelte is a good choice due to its use of reactive statements.

It is recommended to use the command `meteor npm` instead of `npm`
when installing npm packages in a Meteor app.
When packages have binary dependencies,
this ensures that they are built using the same C libraries.

1. Enter `meteor npm install svelte`
1. Add some Meteor packages by entering
   `meteor add svelte:compiler rdb:svelte-meteor-data`
1. Remove a package that will not be used by entering
   `meteor remove blaze-html-templates`
1. Add a replacement Meteor package by entering `meteor add static-html`
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
   import App from '../imports/ui/App.svelte';

   console.log('meteor.md x: Meteor =', Meteor);

   Meteor.startup(() => {
     new App({target: document.getElementById('app')});
   });
   ```

1. Create a top-level project directory named `imports`.
1. Create a directory inside `imports` named `ui`.
1. Create the file `imports/ui/Task.svelte` containing the following:

   ```html
   <script>
     export let task;

     const formatDate = date =>
       date.toLocaleDateString('en-us', {
         month: 'short',
         day: 'numeric',
         year: 'numeric'
       });
   </script>

   <li>{task.text} - added {formatDate(task.createdAt)}</li>
   ```

1. Create the file `imports/ui/App.svelte` containing the following:

   {% raw %}

   ```html
   <script>
     import Task from './Task.svelte';

     function getTasks() {
       return [
         {_id: 1, text: 'This is task 1'},
         {_id: 2, text: 'This is task 2'},
         {_id: 3, text: 'This is task 3'}
       ];
     }
   </script>
   <div class="container">
     <header>
       <h1>Todo App</h1>
     </header>
     <ul>
       {#each getTasks() as task}
       <Task task="{task}" />
       {/each}
     </ul>
   </div>
   ```

   {% endraw %}

1. Copy the CSS from
   [here](https://github.com/meteor/simple-todos-svelte/blob/master/client/main.css)
   into `client/main.css`.

1. Create `imports/tasks.js` containing the following:

   ```js
   import {Mongo} from 'meteor/mongo';

   export const Tasks = new Mongo.Collection('tasks');
   ```

1. Add the following near the top of `server/main.js`
   to create the MongoDB collection:

   ```js
   import '../imports/tasks.js';
   ```

1. Add the following imports near the top of `client/App.svelte`:

   ```js
   import {useTracker} from 'meteor/rdb:svelte-meteor-data';
   import {Tasks} from '../imports/tasks.js'; // creates local collection cache
   ```

1. Replace the `getTasks` function in `client/App.svelte` with the following:

   ```js
   const query = {};
   const projection = {sort: {createdAt: -1}}; // newest first
   $: tasks = useTracker(() => Tasks.find(query, projection).fetch());
   ```

1. Replace the call to `getTasks()` in `client/App.svelte` with `$tasks`.

1. Insert some tasks using the MongoDB console by entering the following:

   ```bash
   meteor mongo
   db.tasks.insert({ text: "buy milk" })
   ```

   Note that the UI updates automatically to show this todo item.

1. Add the following in `client/App.svelte` after the `header` element:

   ```html
   <form on:submit|preventDefault="{addTask}">
     <input placeholder="todo text" bind:value="{text}" />
     <button>Add</button>
   </form>
   ```

1. Add the following inside the `script` tag in `client/App.svelte`:

   ```js
   let text = '';

   function addTask() {
     Tasks.insert({text, createdAt: new Date()});
     text = '';
   }
   ```

1. Add the following after the HTML in `client/App.svelte`:

   ```css
   <style>
     form {
       margin-top: 0;
       padding-bottom: 1rem;
     }
   </style>
   ```

   Now new tasks can be added by entering text in the input and
   either pressing the "Add" button or pressing the return key.
   Meteor keeps all clients in sync. To see this,
   open a second web browser or another window in the same web browser
   and browse localhost:3000.
   Add a todo in either browser window and notice that it appears in both.

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
   </script>

   <li>
     <input type="checkbox" checked="{task.done}" on:click="{toggleDone}" />
     <span class:done="{task.done}">
       {task.text} - added {formatDate(task.createdAt)}
     </span>
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
   </style>
   ```

1. Show the number of remaining tasks and total tasks in the heading
   by making the following changes in `client/App.svelte`:

   - Add the following in the `script` tag:

     ```js
     $: remaining = $tasks.filter(t => !t.done).length;
     ```

   - Add the following after the `header` tag:

     ```html
     <p class="stats">{remaining} of {$tasks.length} remaining</p>
     ```

   - Add the following inside the `style` tag:

     ```css
     .stats {
       margin-top: 0;
     }
     ```

1. Add the ability to only display tasks that are not done
   by making the following changes in `client/App.svelte`:

   - Add the following variable declaration in the `script` tag:

     ```js
     let hideCompleted = false;
     ```

   - Add the following in side the `form` element:

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

1. Add support for user accounts by doing the following:

   - Enter `meteor add accounts-ui accounts-password`

   - Enter `meteor add svelte:blaze-integration`

   - Create the file `imports/account-config.js` with the following content:

     ```js
     import {Accounts} from 'meteor/accounts-base';

     // Email is needed for the next step.
     Accounts.ui.config({
       passwordSignupFields: 'USERNAME_AND_EMAIL'
     });
     ```

   - Add the following near the top of `client/main.js`:

     ```js
     import '../imports/accounts-config.js';
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
       const projection = {sort: {createdAt: -1}};
       // tasks is a store
       $: tasks = useTracker(() => Tasks.find(query, projection).fetch());

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
         <p class="stats>{remaining} of {$tasks.length} remaining</p>

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

     We now have the ability to create accounts, sign in,
     change the password of the signed in user, and sign out.

1. Add support for "forgot password" emails.
   This will allow users to click a "Forgot password" link
   in the "Sign in" dialog which changes the dialog content
   to prompt for an email address.
   If a user account exists for the email address,
   it is sent an email containing a link that the user can click
   to reset their password.
   After doing so they are immediately signed in.

   - Enter `meteor add email`.
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

   - If using Git, add `secrets.json` to the `.gitignore` file
     so it is not added to the repository for others to see.

   - Create the file `server/accounts-setup.js` with the following content:

     ```js
     import {Accounts} from 'meteor/accounts-base';
     import secrets from '../secrets.json';

     const siteName = 'Todos by Mark V.';
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

1. Make the app more secure by moving database interactions to the server.
   This is accomplished by implemented "methods"
   that are invoked by client-side code.
   Client code invokes them using `Meteor.call(name, data, callback)`
   which makes a Remote Procedure Call (RPC) using WebSockets,
   not by sending an HTTP request.
   The callback function is passed an error description and a result.
   If the call succeeds, the error description is undefined
   and the result is set to whatever value the method returns.
   If the call fails, the error description is set to an object
   that contains the properties
   `error` (an HTTP status code even though HTTP isn't used),
   `reason` (concise message), `message` (long message), and more.
   It does not throw a JavaScript Error, so try/catch cannot be used.
   An issue with using Meteor Methods instead of REST services
   is that they really can only be called from Meteor apps.
   Methods that throw should do so using
   `throw new Meteor.error(methodName, message)`.
   Throwing a normal JavaScript Error will not return the message to the client.
   Add information about "optimistic UI" and "method retries" in Meteor!
   See <https://blog.meteor.com/optimistic-ui-with-meteor-67b5a78c3fcf>.

   - Enter `meteor remove insecure`

   - Define server-side methods by modifying `imports/task.js`
     to match the following:

     ```js
     import {check} from 'meteor/check';
     import {Meteor} from 'meteor/meteor';
     import {Tasks} from '../imports/tasks';

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

   - Add the following near the top of `server/main.js`
     to invoke the code above:

     ```js
     import './methods';
     ```

   - Create the file `client/util.js` containing the following:

     ```js
     export function handleError(err) {
       // Replace this will better error handling.
       if (err) alert(err.message);
     }
     ```

   - Add the following `imports` in `client/App.svelte` and `client/Task.svelte`:

     ```js
     import {handleError} from './util';
     ```

   - Change the call to `Tasks.insert` in the `addTask` function
     of `client/App.svelte` to the following:

     ```js
     Meteor.call('addTask', text, handleError);
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

   - Remove the import of `Tasks` from `client/Task.svelte`
     since it is no longer used.

1. Explicitly specify what data the server sends to the client
   so we can separate todos by user.
   They will still be stored in the same MongoDB collection,
   but each user will only see and operate on the todos they created.

   - Enter `meteor remove autopublish`

   - Add the following after the imports in `server/tasks.js`
     to publish only tasks that belong to the logged in user:

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

   - Add the following `import` in `client/App.js`:

     ```js
     import {onMount} from 'svelte';
     ```

   - Add the following in the `script` tag of `client/App.js`:

     ```js
     onMount(() => Meteor.subscribe('tasks'));
     ```

1. Require account email validation before tasks can be entered.

   - Add the following at the bottom of `server/account-setup.js`:

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

   - Add the following in `client/App.svelte` after the `$: user =` line
     to determine if the logged in user has a verified email address:

     ```js
     $: emailVerified = $user && $user.emails[0].verified;
     ```

   {% raw %}

   - Change `{#if user}` to `{#if $user && emailVerified}`
   - Add the following before `{:else}`:

     ```js
     {:else if $user && !emailVerified}
       <p>
         You have been sent an email containing a link to verify your account.
         Please click that link in order to start adding tasks.
       </p>
     ```

   {% endraw %}

### Building and Deploying

To build the app for deployment, enter `meteor build {dir-name}`.
This creates the specified directory if it doesn't exist
and creates a TAR file inside it named `{project-name}.tar.gz`.
This contains a single directory named `bundle` which contains
all the files for the client-side and server-side of the application.

Include the `--architecture` option to build for a specific architecture.

For help on the build command, enter `meteor help build`.

To deploy the app:

1. Copy the TAR file to a server.
1. SSH to the server.
1. Verify that a recent version of Node.js is installed.
1. Un-tar the file by entering `tar zxf {project-name}.tar.gz`
1. Enter `cd {project-name}/programs/server`
1. Enter `npm install`
1. Set the `MONGO_URL` environment variable to `mongodb://localhost:27017/myapp`
1. Set the `ROOT_URL` environment variable to `http://my-app.com`
1. Enter `node main.js` to start the server.

Other alternatives for deploying Meteor apps include Galaxy and Meteor Up.
Meteor Up (<http://meteor-up.com/>), a.k.a mup, can be used to
deploy a Meteor app to any server to which you can ssh.
