---
eleventyNavigation:
  key: CloudKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

CloudKit is an Apple cloud-based database solution that is similar to Firebase.
It provides a generous amount of free storage.
Using CloudKit requires an Apple Developer account.

CloudKit supports three kinds of databases.

- Public is for databases that are shared between all users of an application.
- Private is for databases that are only for the current user of an application.
  The space used counts against the iCloud limit of each user.
- Shared is for databases that are shared between multiple ???.

A database has a collection of "record types"
which are like tables in a relational database.
Each record type has a collection of fields.

"Zones" are used to segregate the data in databases.
A default zone named "\_defaultZone" is provided.

"Subscriptions" all apps to subscribe to a database
in order to be notified about changes.
This enables synchronizing changes across devices.
For example, a user with an iPhone and an iPad can
enter data on one device and have it automatically appear on the other.

"Indexes" improve query performance,
removing the need to search through records sequentially.

Applications that use CloudKit do not need to include a login screen
because users are authenticated based on their Apple ID.

"Security Roles" restrict database access.

When testing in the Simulator, sign in to your iCloud
by opening the Settings app, clicking "Sign in to your iPhone",
and entering your Apple ID and password.
When prompted about merging contacts, click "Don't Merge".

The record type "Users" is provided by default
and is a collection of iCloud user accounts.

A container is a space in the cloud that stores all of your saved data.
Each application can have its own container or
a container can be shared between multiple apps.
Each container has development and production databases.

The web-based CloudKit Console supports
querying, creating, updating, and deleting records.

To add the use of CloudKit to a project:

1. Click the top item in the Navigator.
1. Click the main target.
1. Click the "Signing and Capabilities" tab.
1. Click the "+" in the upper-right to add a capability.
1. Double-click "iCloud".
1. Under "Services", check the "CloudKit" checkbox.
   Also check "Key-value storage" and/or "iCloud Documents"
1. Click the "CloudKit Console" button to open a web page at
   https://icloud.developer.apple.com/dashboard/home/teams/{your-team-id}.
1. Click the "+" under "Containers" and enter a name for the new container.
   This needs to be unique among all CloudKit containers,
   so consider using the bundle id of the app.
1. Periodically click the refresh button below the list of containers
   until the new container name changes color from red to white,
   indicating that the container has been created.
   This can take a couple of minutes.
1. Click the "CloudKit Console" button.
1. Sign in using your Apple Developer account
1. Click the big "CloudKit Database" button.

After the first record is saved in the container,
make the records of that type queryable.

1. Click the top item in the Navigator.
1. Click the first target.
1. Click the "Signing and Capabilities" tab.
1. Click the "CloudKit Console" button
1. In the left nav under "Schemas", click "Indexes".
1. Click the name of the new record type.
1. Click "Add Basic Index".
1. In the "Select an option" dropdown, select "recordName".
1. It should be marked as "Queryable".
1. Click the "Save Changes" button.
1. In the left nav under "Data", click "Records".
1. In the "Record Types" dropdown, select the new record name.
1. Click the "Query Records" button to display all the records of that type.

For a good example of using CloudKit including performing CRUD operations
and subscribing to be notified of changes see
{% aTargetBlank "https://github.com/mvolkmann/CloudKitDemo", "CloudKitDemo" %}.

## Sample Code

See the app {% aTargetBlank "https://github.com/mvolkmann/CloudKitDemo",
"CloudKitDemo" %}.

## Querying

When querying for records, to limit the fields included in the returned data,
set the `desiredKeys` property on the `CKQueryOperation` object
to an array of property name strings.

## Dashboard

To view the web-based CloudKit Dashboard:

- Browse `developer.apple.com`.
- Click "Account".
- Sign in.
- In the left nav, click "CloudKit Console".
- Click the "CloudKit Database" button.
- Select a container from the dropdown at the top.

To view all the records of a given type:

- Click the "RECORD TYPE" dropdown and select one.
- Click the "Query Records" button.

To define a new record type:

- TODO

To create a new record:

- TODO

To delete a record:

- TODO
