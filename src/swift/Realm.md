---
eleventyNavigation:
  key: Realm
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://realm.io/", "Realm" %} is an object database that
can be used by web applications and mobile applications (Android and iOS).
The data can remain local on each device or it can be synced with the cloud.
It provides an alternative to SQLite.

Realm is a popular alternative to using Core Data and CloudKit
which are specific to Apple platforms.

Changes in a local database are synced with
a MongoDB Atlas database in the cloud.
This enables all users of an app to have a consistent view of the data.
Realm has built-in conflict resolution to handle concurrent updates.

Partitioning can be used to limit the data that is synced in each app.
This has the following benefits:

- avoid copying sensitive data to devices the should not have access
- reduce the amount of data copied to each device
- reduce changes related to bandwidth usage.

Realm was purchased by MongoDB in 2019 for 39 million dollars.

Realm supports multiple authentication approaches including
username/password, JWT, Google, and Apple authentication.

Realm supports serverless functions implemented in JavaScript
that can use NPM modules.

Realm supports database triggers for:

- authentication: user registration, login, and user delete
- data: insert, update, replace, and delete operations
- scheduled operations similar to CRON

{% aTargetBlank "https://www.mongodb.com/docs/realm/studio/", "Realm Studio" %}
is a visual tool for designing Realm databases and
adding, viewing, and editing data.
It is similar to {% aTargetBlank "https://www.mongodb.com/products/compass",
"Compass" %} for MongoDB databases.

Realm Studio provides two-way live updates.
Changes made in the Realm Studio appear in running apps
and changes made in running apps appear in Realm Studio.

## Setup

1. Browse {% aTargetBlank "https://www.mongodb.com/cloud",
   "MongoDB Cloud Services" %}.
1. Click "Try Free".
1. Enter the required information to create an Atlas account.
1. Click the "Build a Database" button.
1. Select the "M0" button which is free and is
   "for learning and exploring MongoDB in a cloud environment".
1. Click the "Create" button.
1. Enter a username and password to associate with the new database.
1. Click the "Create User" button.
1. Click the "Finish and Close" button.
1. In the dialog that appears click the "Go to Databases" button.
1. Click the "App Services" tab.
1. Select "Real-time Sync".
1. Click the "Next" button.
1. Under "Name your Application" enter a name and click the "Save" button.
   I entered "RealmDemo".
1. Click the "Create App Service" button.
1. Select "Swift (iOS + SwiftUI)".
1. Click the "Download front-end code" button.
1. Click the "Close" button.
1. In the Finder, navigate to the "Downloads" directory.
1. Double-click the zip file that was downloaded whose name
   begins with the application name entered above to unzip it.
1. In the directory created by unzipping,
   double-click the `.xcodeproj` file to open it in Xcode.
1. Return to the web browser.
1. In the left nav, click "Authentication".
1. Verify that "Email/Password" authentication is enabled ("On" by default).
1. Click the "Edit" button on the row for "Email/Password".
1. Choose a "User Confirmation Method".
1. Choose a "Password Reset Method".
1. In the left nav, click "Device Sync" to enable syncing.
   No sync configuration changes are needed.

## Creating an iOS App

1. Launch Xcode.
1. Create a new project for an iOS app.
1. Select File ... Add Packages...
1. In the search input paste "https://github.com/realm/realm-swift.git".
   This will find a package named "realm-swift".
1. Add this package to obtain both "Realm" and "RealmDatabase".
1. Create model structs. 23:00 in video

TODO: Add more detail to this page!
