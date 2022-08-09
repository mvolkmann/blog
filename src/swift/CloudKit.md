---
eleventyNavigation:
  key: CloudKit
  parent: Swift
layout: topic-layout.njk
---

Using CloudKit requires an Apple Developer account.

CloudKit supports three kinds of databases.

- Public is for databases that are shared between all users of an application.
- Private is for databases that are only for the current user of an application.
- Shared is for databases that are shared between multiple applications.

A database has a collection of "record types"
which are like tables in a relational database.
Each record type has a collection of fields.

"Zones" are used to segregate the data in databases.
A default zone named "\_defaultZone" is provided.

"Subscriptions" all apps to subscribe to a database
in order to be notified about changes.

"Indexes" improve query performance,
removing the need to search through records sequentially.

"Security Roles" restrict database access.

When testing in the Simulator, sign in to your iCloud
by opening the Settings app, clicking "Sign in to your iPhone",
and entering your Apple ID and password.
When prompted about merging contacts, click "Don't Merge".

The record type "Users" is provided by default
and is a collection of iCloud user accounts.

Each container has a development and production databases.

The web-based CloudKit Console supports
querying, creating, updating, and deleting records.

1. Click the top item in the Navigator.
1. Click the first target.
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

1. In the first dropdown under "Records", select "Private Database"
   to store data that is not shared with other users.

1. . Here we can create a default container for the app. A container is a space in the cloud that stores all of your saved data. You can use a container per application or a single container to share data between multiple apps.

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
