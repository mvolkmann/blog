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

- Public

  These are for databases that are shared between all users of an application.
  The space used counts against the iCloud limit of the app.

- Private

  These are for databases that are only accessible
  by the current user of an application.
  The space used counts against the iCloud limit of each user.

- Shared

  These are for databases that are shared between multiple ???.
  Is the space used counted against the iCloud limit of the app?

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
1. Under "Services", check all the checkboxes.
   These include "Key-value storage", "iCloud Documents", and "CloudKit".
1. Click the "+" under "Containers" and enter a name for the new container.
   This needs to be unique among all CloudKit containers,
   so consider using the bundle id of the app prefixes by "iCloud.".
1. Periodically click the refresh button below the list of containers
   until the new container name changes color from red to white,
   indicating that the container has been created.
   This can take a couple of minutes.
1. Click the "CloudKit Console" button to open the website at
   https://icloud.developer.apple.com/dashboard/home/teams/{your-team-id}.
1. Sign in using your Apple Developer account
1. Click the big "CloudKit Database" button.
1. Click the container dropdown at the top and
   select the container name created above.

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

To enable use of subscriptions:

1. Click the top item in the Navigator.
1. Click the first target.
1. Click the "Signing and Capabilities" tab.
1. Click the "+" in the upper-right to add a capability.
1. Double-click "Background Modes".
1. Under "Modes", check the "Background fetch"
   and "Remote notifications" checkboxes.

For a good example of using CloudKit including performing CRUD operations
and subscribing to be notified of changes see
{% aTargetBlank "https://github.com/mvolkmann/CloudKitDemo", "CloudKitDemo" %}.

## Terminology

- Container: TODO
- Database: TODO
- Record: TODO
- Reference: TODO
- Operation: TODO

  Use convenience APIs or `CKOperation` for full power.

## Sample Code

See the app {% aTargetBlank "https://github.com/mvolkmann/CloudKitDemo",
"CloudKitDemo" %} which demonstrates performing all the CRUD operations
on records in a CloudKit database.

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
- Select a database type: Public, Private, or Shared

## Schemas

In the left nav under "Schema", click "Record Types".

To define a new record type:

- Click the "+" after "Record Types".
- Enter a name.
- For each field

  - Click the "+" after "Fields".
  - Enter a name.
  - Select a type from the "Type" dropdown.

    The supported types are limited to those that
    conform to `CKRecordValueProtocol`. These include:

    - Asset
    - Bytes
    - Location
    - Double
    - Int(64)
    - Reference
    - String
    - Date/Time
    - Encrypted Bytes
    - Encrypted String

    Most of these also have a "(List)" option for a collection of values.

  - Click the "Done" button.

- Click the "Save" button.

To view and edit the schema for an existing record type,
click the name of a record type.

To add a field to a selected record type:

- Click the "+" after "Record Fields".
- Enter a name.
- Select a type from the "Type" dropdown.

To edit or delete a field in the selected record type,
click the ellipsis at the end of the row for the field
and select "Edit" or "Delete".

To delete a record type and all records of that type:

- Click the name of a record type.
- Click the ellipsis in the upper right.
- Select "Delete Record Type...".
- Click the "Delete" button.

## Data

To view (query) all the records of a given type:

- In the left nav under "Data", click "Records".
- Select a record type from the "RECORD TYPE" dropdown.
- Click the "Query Records" button.

Values of fields with a type of "Asset" can be downloaded from here.

To create a new record of the selected record type:

- Click the "+" after "Records".
- Select "Create New Record".
- In the area that appears on the right side,
  enter values for each of the fields.
  Typically the fields under "Metadata" should not be modified.
- Click the "Save" button.

To update an existing record:

- TODO

To delete an existing record:

- TODO

## Assets

Records can have fields with a type of "Asset".
The data for these is stored outside of the record data
and is referenced from records by URLs.

TODO: Try this.

## Push Notifications

It is possible receive a push notification every time
a record of a specific record type is created, updated, or deleted.
This can be used to keep multiple devices in sync.

Push notifications are only sent to apps running in real devices,
not to apps running in the Simulator.

## Production Databases

Once a database has been switched from "Development" to "Production",
it is no longer possible to delete record types.
New record types can be added and existing record types can be modified.
TODO: Can fields in records be deleted?
