---
eleventyNavigation:
  key: Firebase
layout: topic-layout.njk
---

{% aTargetBlank "https://firebase.google.com", "Firebase" %}
is a suite of products from Google,
primarily known for providing a cloud-based database.

Services provided include (alphabetical):

- A/B Testing
- App Distribution
- Authentication
- Cloud Firestore (newest type of database)
- Cloud Functions
- Cloud Messaging
- Cloud Storage
- Crashlytics
- Dynamic Links
- Google Analytics
- Hosting
- In-App Messaging
- Machine Learning
- Performance Monitoring
- Realtime Database (original type of database)
- Remote Config
- Test Lab

Features that the Realtime Database and Cloud Firestore have in common include:

- NoSQL database that stores JSON
- hosted by Google in the cloud
- data syncs across multiple clients in near realtime
- data remains available when clients go offline

For details on the differences and cost of each option, see {% aTargetBlank
"https://firebase.google.com/docs/firestore/rtdb-vs-firestore",
"Choose a databsase" %}.

TODO: Can data be modified when offline and re-synced when back online?

## Pricing

For details, see the {% aTargetBlank "https://firebase.google.com/pricing",
"Pricing" %} page.
There are two pricing plans, "Spark" and "Blaze".

The Realtime Database only charges for bandwidth and storage
and does so at a higher rate thatn Cloud Firestore.
Is there are minimum cost of $5/month?

The Cloud Firestore database charges for
read, write, and delete operations performed.
It is free for small usages.
Daily spending limits can be enforced.

## Creating a New Database

1. browse firebase.google.com
1. click the "Get Started" button
1. login with your Google account
1. click "Add project"
1. enter a project name
1. check "I accept the Firebase terms"
1. click "Continue"
1. optionally disable "Enable Google Analytics for this project"
1. if enabled, click "Continue"
1. if enabled, check "I accept the Google Analytics terms"
1. click "Create project"
1. wait about 30 seconds for the project to be created
1. click "Continue"

There are two types of databases to choose between,
"Cloud Firestore" (new) and "Realtime Database" (original).

## Firestore Database

1. in the left nav, click "Firestore Database"
1. click "Create Database"
1. select a mode radio button (production or test)
1. click "Next"
1. select a location (ex. "us-central") and click "Enable"
1. wait for provisioning to complete

## Realtime Database

1. in the left nav, click "Realtime Database"
1. click "Create Database"
1. select a location (ex. "United States") and click "Next"
1. select the "Start in locked mode" radio button for secured data access
1. click "Enable"
1. optionally add data to the database tree in the web UI, up to 32 levels deep

Retrieving a node also retrieves all the nodes below it.
Read/write restrictions on a node also apply to all nodes below it.

## Using in a Web App

1. Open a terminal
1. cd to the top project directory
1. Enter `npm install firebase`
1. In a web browser, navigate to the Firebase project.
1. In the left nav., click the gear icon after "Project Overview".
1. Click "Project settings".
1. Click the "General" tab.
1. Scroll down to the "Your apps" section
1. Click an app type: iOS, Android, web, or Unity.
1. If web is selected
   - Enter an "App nickname".
   - Click the "Register app" button.
   - Select the "Use npm" radio button.
   - Follow the instructions that are displayed.
   - Move the line that sets `firebaseConfig` to `src/secrets.js`
     and add `export` in front of it.
   - Add `src/secrets.js` to `.gitignore`
   - Import `firebaseConfig` from `secrets.js`
     in the file where `initializeApp` is called.
   - Back in the web browser, click "Continue to console".

- enter an app nickname
- check the "Also set up Firebase Hosting for this app" checkbox
- click "Register app"
- copy the JavaScript code needed to configure the use of Firebase and paste it into the web app
- import {initializeApp} from ‘firebase/app’;
- import {getDatabase, onValue, ref, set} from ‘firebase/database’;
- const firebaseConfig = {…};
- const app = initializeApp(firebaseConfig);
- const db = getDatabase(); // can call multiple times; returns same object
- const userRef = ref(db, ‘/users/ + userId);
- set(userRef, userObject); // replaces node if one already exists at the path
- const emailRef = ref(db, `/users/${userId}/email`);
- // The onValue callback is called initially and again every time the value changes.
- // Does this use a WebSocket?
- onValue(emailRef, snapshot => {
- const email = snapshot.val();
- // Display the email.
- });

How can you delete a node and all its descendants?

To watch a list of nodes with a common parent node …
const parentRef = ref(db, parentPath);
onValue(
parentRef,
snapshot => {
snapshot.forEach(childSnapshot => {
const childKey = childSnapshot.key;
const childValue = childSnapshot.val();
// Do something with child value.
});
},
{onlyOnce: true} // Does this mean we aren’t watching for changes?
);

Rather than watching for any changes to the children of a parent node,
we can listen for specific events.

onChildAdded(parentRef, data => { … });

- triggered once for each existing child and again every time a new child is added

onChildChanged(parentRef, data => { … });

onChildRemoved(parentRef, data => { … });

There are web framework specific libraries for Firebase including
AngularFire, ReactFire, SvelteFire, and VueFire.

Swift Details

- create a Swift project in Xcode
- when creating a Firebase project, paste in the bundle id from the Xcode project
- download the Firebase configuration GoogleService-Info.plist
- to install the Firebase SDK in the Swift project
  - cd to the top project directory
  - if not already installed, install Cocoapods - sudo gem install cocoapods
  - enter "pod init" to create the file "Podfile" (similar to the Node.js package.json file)
  - see available pods at https://firebase.google.com/docs/ios/setup#available-pods
  - edit "Podfile" and add "‘pod ‘Firebase/Database’" or "pod ‘Firebase/‘"
  - if running on an M1 Mac
  - … in Finder, locate Terminal under Applications/Utilities
  - … right-click and select "Get Info"
  - … check the checkbox for "Open using Rosetta"
  - … if Terminal is running, quit out of it
  - … open Terminal
  - … enter "sudo gem install ffi"
  - … enter "sudo gem install cocoapods"
  - … quit out of Terminal
  - … undo the change to open using Rosetta
  - enter "pod install" which creates a "{project-name}.xcworkspace" file
  - open the project in Xcode by double-clicking the .xcworkspace file instead of the .xcodeproj file in order to have access to all the installed pods
  - edit {project-name}App.swift
  - add "import Firebase"
  - inside the App class, add "init() { Firebase.configure() }"

Firestore Database

two main record types, collections and documents

## Svelte Demo

The following code is from a Svelte component.
It listens for documents in a collection named "Residents".
It supports adding, updating, and deleting the documents in this collection.

<img alt="Firestore in Svelte" style="width: 50%"
  src="/blog/assets/firestore-svelte.png?v={{pkg.version}}"
  title="Firestore in Svelte">

```html
<script>
  import {initializeApp} from 'firebase/app';
  import {
    addDoc,
    collection,
    deleteDoc,
    doc,
    getDocs,
    getFirestore,
    onSnapshot,
    query,
    updateDoc,
    where
  } from 'firebase/firestore';
  import {onDestroy, onMount} from 'svelte';
  import {firebaseConfig} from '../secrets';

  let editing = false;
  let residents = [];

  const app = initializeApp(firebaseConfig);
  const db = getFirestore();

  let firstName = '';
  let lastName = '';
  let residentId;
  let unsubscribeOne;
  let unsubscribeAll;

  $: canAdd = firstName && lastName;

  onMount(() => {
    // Listen for changes to a specific document.
    const markId = 'b9uoh8LOTN5wPFiPuth9';
    unsubscribeOne = onSnapshot(doc(db, 'residents', markId), doc => {
      console.log('resident data: ', doc.data());
    });

    // Listen for changes to any documents in a collection.
    const q = query(collection(db, 'residents')); //, where('state', '==', 'CA'));
    unsubscribeAll = onSnapshot(q, snapshot => {
      residents = [];
      snapshot.forEach(doc => {
        residents.push({id: doc.id, ...doc.data()});
      });
    });
  });

  onDestroy(() => {
    if (unsubscribeOne) unsubscribeOne();
    if (unsubscribeAll) unsubscribeAll();
  });

  async function deleteResident(resident) {
    const docRef = doc(db, 'residents', resident.id);
    await deleteDoc(docRef);
  }

  function editResident(resident) {
    residentId = resident.id;
    firstName = resident.firstName;
    lastName = resident.lastName;
    editing = true;
  }

  async function saveResident() {
    if (editing) {
      const docRef = doc(db, 'residents', residentId);
      await updateDoc(docRef, {firstName, lastName});
    } else {
      const colRef = collection(db, 'residents');
      await addDoc(colRef, {firstName, lastName});
    }

    firstName = lastName = '';
    editing = false;
  }

  // One-time fetch instead of listening.
  async function getResidents() {
    const snapshot = await getDocs(collection(db, 'residents'));
    snapshot.forEach(doc => residents.push(doc.data()));
    residents = residents; // trigger reactivity
  }
</script>

<h1>Health Web App</h1>

<form on:submit|preventDefault={saveResident}>
  <label>
    First Name <input bind:value={firstName} />
  </label>
  <label>
    Last Name <input bind:value={lastName} />
  </label>
  <button disabled={!canAdd}>
    {editing ? 'Update' : 'Add'} Resident
  </button>
</form>

<table>
  <thead>
    <tr>
      <th>First Name</th>
      <th>Last Name</th>
      <th>Actions</th>
    </tr>
  </thead>
  <tbody>
    {#each residents as resident}
      <tr>
        <td>{resident.firstName}</td>
        <td>{resident.lastName}</td>
        <td>
          <button on:click={() => editResident(resident)} title="Edit">
            ✎
          </button>
          <button on:click={() => deleteResident(resident)} title="Delete">
            🗑
          </button>
        </td>
      </tr>
    {/each}
  </tbody>
</table>

<style>
  form > * {
    margin-bottom: 0.5rem;
  }

  input {
    border: 1px solid lightgray;
    border-radius: 0.5rem;
    padding: 0.5rem;
  }

  label {
    display: block;
  }

  table {
    border-collapse: collapse;
  }

  td,
  th {
    border: 1px solid lightgray;
    padding: 0.5rem;
  }

  td > button {
    background-color: transparent;
    border: none;
  }
</style>
```
