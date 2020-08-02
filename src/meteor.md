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
It is used to host client-side and server-side code.

Meteor is built on Node.js.
It has good integration with the MongoDB NoSQL database.
It supports a publish and subscribe mechanism to synchronize
user interfaces with backend data, providing real-time updates.

A significant benefit of Meteor is the ability to implement
significant functionality in a small amount of code.
One example of this is user sign up and authentication.

Meteor has its own frontend framework called Blaze.
But it also supports popular options such as
Angular, React, Svelte, Vue, and Cordova.

Meteor was initially released in 2012.
It gained immediate attention for its novel use of WebSockets
as an alternative to HTTP.
But the attention faded quickly as other frameworks garnered more notice.
It is seeing some resurgence in 2020.

Galaxy is a cloud hosting platform for Meteor applications.

The use of WebSockets to support the Meteor pub/sub model
has been replaced by the use of the Apollo framework and GraphQL.
TODO: Has it been completely replaced?

Companies that use Meteor include
Qualcomm, Mazda, IKEA, and Honeywell.

Enterprise support is available in order to have
guaranteed support response times and SLAs.

Meteor provides many command-line tools.
These are summarized at <https://docs.meteor.com/commandline.html>.
Enter `meteor help` for help on using these.

### Installing Meteor

See <https://www.meteor.com/install>.
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
- `imports` (legacy)
  - `client`
    - `\*.js`
  - `server`
    - `\*.js`

In the past Meteor eagerly loaded files outside the `imports` directory
and lazily loaded files inside the `imports` directory.
As of Meteor 1.7 it no longer makes this distinction
and all code is lazily loaded.
So there is no longer a need to have an `imports` directory.
All client-side files can be placed in the `client` directory
and all server-side files can be placed in the `server` directory.

### Blaze Web Framework

TODO: Learn something about this

### Collections

Meteor stores data in "collections" that can be accessed from both
client-side and server-side code.
When data is added to a collection or existing data is modified,
client code that uses it updates automatically.

Typically collections are associated with a MongoDB collection,
but it's also possible to use other databases such as
PostgreSQL and RethinkDB.

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

Client code can update a collection on the server using a
"method" which uses Meteor remote procedure call (RPC).

### ESLint

ESLint will give "Unable to resolve path to module" errors
on all imports that begin with "meteor/".
However, the Meteor build system is able to resolve these.
To suppress these errors, add the following to your `.eslintrc.json` file:

```json
  "rules": {
    "import/no-unresolved": ["error", {"ignore": ["^meteor/"]}]
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
1. Add some Meteor packages by entering `meteor add svelte:compiler rdb:svelte-meteor-data`
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

1. Copy the CSS from [here](https://github.com/meteor/simple-todos-svelte/blob/master/client/main.css) into `client/main.css`.

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
       padding: 1rem;
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
     <p>{remaining} of {$tasks.length} remaining</p>
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

       {#if $user}
       <p>{remaining} of {$tasks.length} remaining</p>

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
       {/if}
     </div>

     <style>
       form {
         margin-top: 0;
         padding: 1rem;
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

     Accounts.emailTemplates.siteName = 'Todos by Mark V.';
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
   that contains the properties `error` (HTTP status code),
   `reason` (concise message), `message` (long message), and more.
   It does not throw a JavaScript Error, so try/catch cannot be used.
   An issue with using Meteor Methods instead of REST services
   is that they really can only be called from Meteor apps.
   Methods that throw should do so using
   `throw new Meteor.error(methodName, message)`.
   Throwing a normal JavaScript Error will not return the message to the client.

   - Enter `meteor remove insecure`

   - Define server-side methods by modifying `server/tasks.js`
     to match the following:

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
