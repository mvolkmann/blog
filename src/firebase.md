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

TODO: Can data be modified when offline and re-synced when back online?

TODO: Summarize the cost

browse firebase.google.com
click the “Get Started” button
login with you Google account
click “Add project”
enter a project name
check “I accept the Firebase terms”
click “Continue”
optionally “enable Google Analytics for this project”
click “Continue”
if enabled, check “I accept the Google Analytics terms”
click “Create project”
wait about 30 seconds for the project to be created
click “Continue”

There are two types of databases to choose between,
“Cloud Firestore” (new) and “Realtime Database” (original).

Realtime Database
in the left nav, click “Realtime Database”
click “Create Database”
select a location (ex. “United States”) and click “Next”
select the “Start in locked mode” radio button for secured data access
click “Enable”
optionally add data to the database tree in the web UI, up to 32 levels deep
retrieving a node also retrieves all the nodes below it
read/write restrictions on a node also apply to all nodes below it

npm install firebase
click the gear icon after “Project Overview” in the left nav
click “Project settings”
click the “General” tab
scroll down to the “Your apps” section
click an app type: iOS, Android, web, or Unity
if web is selected

- enter an app nickname
- check the “Also set up Firebase Hosting for this app” checkbox
- click “Register app”
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
  - enter “pod init” to create the file “Podfile” (similar to the Node.js package.json file)
  - see available pods at https://firebase.google.com/docs/ios/setup#available-pods
  - edit “Podfile” and add “‘pod ‘Firebase/Database’” or “pod ‘Firebase/‘”
  - if running on an M1 Mac
  - … in Finder, locate Terminal under Applications/Utilities
  - … right-click and select “Get Info”
  - … check the checkbox for “Open using Rosetta”
  - … if Terminal is running, quit out of it
  - … open Terminal
  - … enter “sudo gem install ffi”
  - … enter “sudo gem install cocoapods”
  - … quit out of Terminal
  - … undo the change to open using Rosetta
  - enter “pod install” which creates a “{project-name}.xcworkspace” file
  - open the project in Xcode by double-clicking the .xcworkspace file instead of the .xcodeproj file in order to have access to all the installed pods
  - edit {project-name}App.swift
  - add “import Firebase”
  - inside the App class, add “init() { Firebase.configure() }”

Firestore Database

two main record types, collections and documents
