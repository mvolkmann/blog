---
eleventyNavigation:
  key: CloudKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

CloudKit is an Apple-provided cloud-based database solution
that is similar to Firebase.
It provides a generous amount of free storage.
Developers need an Apple Developer account in order to use CloudKit in an app.
Users of the app need an Apple ID and must configure the use of iCloud
in the Settings app of their devices.

CloudKit supports three kinds of databases.

- Public

  These are for databases that are shared between all users of an application.
  The space used counts against the iCloud limit of the app.

- Private

  These are for databases that are only accessible
  by the current user of an application.
  The space used counts against the iCloud limit of each user.

- Shared

  These are for sharing specific records in a private database
  with other users.

Applications that use CloudKit do not need to include a login screen
because users are authenticated based on their Apple ID.

The record type "Users" is provided by default
and is a collection of iCloud user accounts.

The web-based CloudKit Console supports
querying, creating, updating, and deleting records.

## Terminology

- Container

  A CloudKit container is a collection of one, two, or three databases
  where one is public, one is private, and one shared.
  Each of these can have a development and production version.

  An app can have its own container or it can share a container with other apps.
  An app can access multiple containers.

- Database

  A CloudKit database is a collection of records
  where each has a specific record type.
  Record types are similar to relational database tables
  and NoSQL database collections.

- Record Type

  Each record type is defined by a collection of fields
  that have a name and a data type.

- Record

  A CloudKit record is a collection of field values
  whose types are defined by a record type.

- Index

  Indexes improve query performance,
  removing the need to search through records sequentially.

- Reference

  A CloudKit reference is a field type that is
  used to refer to one record from another.
  The target record can have a different record type
  or the same record type.

- Security Roles

  Security Roles restrict database access.

- Subscriptions

  Subscriptions allow apps to subscribe to a database
  in order to be notified about changes.
  This enables synchronizing changes across devices.
  For example, a user with an iPhone and an iPad can
  enter data on one device and have it automatically appear on the other.

- Operation

  A CloudKit operation describes a specific operation on a database.
  To perform multiple operations as a group (a.k.a. batch), see {% aTargetBlank
  "https://developer.apple.com/documentation/cloudkit/ckoperationgroup",
  "CKOperationGroup" %}.

  CloudKit provides convenience APIs to simplify code,
  but for full power subclasses of {% aTargetBlank
  "https://developer.apple.com/documentation/cloudkit/ckoperation",
  "CKOperation" %} can be used.
  These include the following:

  - {% aTargetBlank "https://developer.apple.com/documentation/cloudkit/ckfetchrecordsoperation",
    "CKFetchRecordsOperation" %}

    This is used to fetch complete records or
    a subset of their fields (`desiredKeys`)
    by the unique ids of the records.

  - {% aTargetBlank "https://developer.apple.com/documentation/cloudkit/ckmodifyrecordsoperation",
    "CKModifyRecordsOperation" %}

    This is used to create, modify, and delete records.

  - {% aTargetBlank "https://developer.apple.com/documentation/cloudkit/ckqueryoperation",
    "CKQueryOperation" %}

    This is used to fetch complete records or
    a subset of their fields (`desiredKeys`)
    using query predicates specified with a {% aTargetBlank
    "https://developer.apple.com/documentation/cloudkit/ckquery", "CKQuery" %}
    object.

- Zones

  Zones are used to segregate the data in private databases,
  not public or shared.
  They support operating on related records in batches.
  A default zone named "\_defaultZone" is provided.
  There is no requirement to create additional zones.

## Sample Code

See the app {% aTargetBlank "https://github.com/mvolkmann/CloudKitDemo",
"CloudKitDemo" %} which demonstrates performing all the CRUD operations
on records in a CloudKit database.

## Adding CloudKit to a Project

To add the use of CloudKit to a project:

1. Click the top item in the Navigator.
1. Click the main target.
1. Click the "Signing and Capabilities" tab.
1. Click the "+" in the upper-right to add a capability.
1. Double-click "iCloud".
1. Under "Services", check the checkboxes for the desired services.
   Typically only "CloudKit" is checked. The options are:

   - "Key-value storage" holds up to 1024 key/value pairs
     and is sometimes use to store user preferences.
   - "iCloud Documents" stores data as files that are accessed using
     `UIDocument` (for iOS) or `NSDocument` (for macOS).
     This is similar to a file system with a directory hierarchy.
   - "CloudKit" stores records defined by a record types.
     These records can contain references to other records.
     This is similar to a relational database.
     These records can be used directly
     or they can mirror the use of Core Data.

1. Click the "+" under "Containers" and enter a name for the new container.
   This needs to be unique among all CloudKit containers,
   so consider using using the app bundle ID.
   The prefix "iCloud-" will be automatically added to the container name.
   Sadly containers cannot be deleted,
   so if you create one with a name you don't like,
   just create another one and don't use the previous container.
   You can hide containers from appearing in the "CloudKit Console" dropdown.
   To do this, click the dropdown, click "Manage Containers",
   and toggle off any containers you wish to hide.
1. Periodically click the refresh button below the list of containers
   until the new container name changes color from red to white,
   indicating that the container has been created.
   This can take a couple of minutes.
1. Click the "CloudKit Console" button to open the website at
   https://icloud.developer.apple.com/dashboard/home/teams/{your-team-id}.
1. Sign in using your Apple Developer account
1. Click the big "CloudKit Database" button.

## CloudKit Console

To view the web-based CloudKit Console, click the "CloudKit Console" button
in Xcode as described above OR follow these steps:

1. Browse `developer.apple.com`.
1. Click "Account".
1. Sign in.
1. In the left nav, click "CloudKit Console".
1. Click the "CloudKit Database" button.

Now that you are browsing the console:

1. Click the container dropdown at the top and
   select a container name being used by the app.
1. Select a database type: Public, Private, or Shared

### Schemas

In the left nav under "Schema", click "Record Types".

To define a new record type:

1. Click the "+" after "Record Types".
1. Enter a camel case name that begins uppercase and is plural
   (ex. "People" or "Pets").
1. Click the "Save" button or press the return key.
1. For each field in the record type:

   1. Click the "+" after " Record Fields".
   1. Enter a camel case name that begins lowercase.
   1. Select a type from the "Type" dropdown.

      The supported types are limited to those that
      conform to `CKRecordValueProtocol`. These include:

      - Asset (for data such as images, audio, and video)
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

      There does not seem to be a way to require a field
      to have a value that is unique across all records.

   1. Click the "Done" button.

1. Click the "Save" button.

To view and edit the schema for an existing record type,
click the name of a record type.

To add a field to a selected record type:

1. Click the "+" after "Record Fields".
1. Enter a name.
1. Select a type from the "Type" dropdown.

To edit or delete a field in the selected record type,
click the ellipsis at the end of the row for the field
and select "Edit" or "Delete".

To delete a record type and all records of that type:

1. Click the name of a record type.
1. Click the ellipsis in the upper right.
1. Select "Delete Record Type...".
1. Click the "Delete" button.

### Adding Indexes

To make records of a particular record type queryable:

1. Open the "CloudKit Console".
1. In the left nav under "Schemas", click "Indexes".
1. Click the name of the new record type.
1. Click "Add Basic Index".
1. In the "Select an option" dropdown, select the field to index.
1. In the dropdown under the "INDEX TYPE" column, select the
   index type which can be "Queryable", "Searchable", or "Sortable".
   TODO: What is the difference between Queryable and Searchable?
1. When finished adding indexes, click the "Save Changes" button.

To make a record type queryable, its "recordName" field,
which holds the unique id of each record,
must have an index with the type "Queryable".

### Querying

To view (query) all the records of a given record type:

1. In the left nav under "Data", click "Records".
1. In the database dropdown, select
   "Public Database", "Private Database", or "Shared Database".
   This always resets to "Public" rather than remembering the last selection.
1. In the "RECORD TYPE" dropdown, select a record type.
1. Click the "Query Records" button.

Values in the "NAME" column are returned in a property named "recordID".
There is no conflict if a record type has a field named "name".

Values of fields with a type of "Asset" can be downloaded from here.

### Records

To create a new record of a given currently selected record type:

1. In the left nav under "Data", click "Records".
1. Click the "+" after "Records".
1. Select "Create New Record".
1. In the right pane, enter values for each of the fields.
   Typically the fields under "Metadata" should not be modified.
1. Click the "Save" button.

The new record will not automatically appear in the displayed list of records.
To see it, click the "Query Records" button again.

To update an existing record:

1. Click the unique id of the record to update
   which is in the first column labelled "NAME".
1. Modify any of the field values that appear in the right pane.
1. Click the blue "Save" button at the bottom of the right pane.

To delete an existing record:

1. Click the unique id of the record to delete
   which is in the first column labelled "NAME".
1. Click the red "Delete" button at the bottom of the right pane.
1. In the confirmation dialog that appears, click the blue "Delete" button.

### Assets

Records can have fields with a type of "Asset".
This is useful for data such as images, audio, and video.
The data for asset fields is stored outside of the record data
and is referenced from records by URLs.

TODO: Try this.

### Zones

To create a new zone:

1. In the left nav under "Data", click "Records".
1. Click the "+" after "Zones".
1. In the right pane, select a database type such as "Private Database".
1. Enter a zone name.
1. Click the blue "Save" button.

TODO: Try this.

## Subscriptions

To enable use of subscriptions from Xcode:

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

## Push Notifications

It is possible receive a push notification every time
a record of a specific record type is created, updated, or deleted.
This can be used to keep multiple devices in sync.

Push notifications are only sent to apps running in real devices,
not to apps running in the Simulator.

## Simulator Testing

In order to test apps that use CloudKit in the Simulator,
it is necessary to sign in to your iCloud account. To do this:

- Open the Settings app within the Simulator.
- Click "Sign in to your iPhone".
- Enter your Apple ID and password.
- When prompted about merging contacts, click "Don't Merge".

## Production Databases

When an app that uses CloudKit is ready for production use,
perhaps being released to the App Store,
switch the database from "Development" to "Production" mode.
To do this, change the dropdown in the upper-left
that reads "Development" to "Production".

Once a container is in production mode,
it is no longer possible to delete record types.
New record types can still be added and
existing record types still can be modified.

TODO: Can fields in records be deleted?

## CloudKit in Code

The following code is a heavily modified version of {% aTargetBlank
"https://github.com/SwiftfulThinking/SwiftUI-Advanced-Learning/blob/main/SwiftfulThinkingAdvancedLearning/CloudKitBootcamps/CloudKitUtility.swift",
"CloudKitUtility.swift" %} from Nick Sarno of Swiftful Thinking.

This requires defining a class for each record type
that conforms to the `CloudKitable` protocol.
The name should be singular.
For example, the class for the `People` record type should be `Person`.
An example of such a class follows:

```swift
import CloudKit

final class Person: CloudKitable, Hashable, Identifiable {
    init(record: CKRecord) {
        self.record = record
    }

    var id: String { record.recordID.recordName }

    var record: CKRecord

    var firstName: String { record["firstName"] as? String ?? "" }
    var lastName: String { record["lastName"] as? String ?? "" }

    // The Hashable protocol conforms to the Equatable protocol.
    // This is required by the Equatable protocol.
    static func == (lhs: Person, rhs: Person) -> Bool {
        lhs.record == rhs.record
    }

    // When present, this is used by the Hashable protocol.
    func hash(into hasher: inout Hasher) {
        hasher.combine(record)
    }
}
```

The `CloudKit` struct below provides methods for
interacting with a CloudKit database.

```swift
import CloudKit
import UIKit

protocol CloudKitable {
    // This must be an optional initializer
    // due to this line in the retrieve method:
    // guard let item = T(record: record) else { return }
    init?(record: CKRecord)

    var record: CKRecord { get }
}

struct CloudKit {
    typealias Cursor = CKQueryOperation.Cursor

    // MARK: - Initializer

    init(containerId: String, usePublic: Bool = false) {
        // TODO: This doesn't result in pointing to the correct container.  Why?
        // container = CKContainer.default()

        // I discovered the container identifier by looking in CloudKitDemo.entitlements.
        // "CloudKit Console" button in "Signing & Capabilities"
        // under "Ubiquity Container Identifiers".
        // TODO: Why did it use this identifier instead of the one
        // TODO: specified in Signing & Capabilities ... Containers?

        container = CKContainer(identifier: containerId)

        database = usePublic ?
            container.publicCloudDatabase :
            container.privateCloudDatabase
    }

    // MARK: - Properties

    var container: CKContainer!
    var database: CKDatabase!

    // MARK: - Non-CRUD Methods

    private func createOperation(
        recordType: CKRecord.RecordType,
        predicate: NSPredicate,
        sortDescriptors: [NSSortDescriptor]? = nil,
        resultsLimit: Int? = nil
    ) -> CKQueryOperation {
        let query = CKQuery(recordType: recordType, predicate: predicate)
        query.sortDescriptors = sortDescriptors
        let operation = CKQueryOperation(query: query)
        if let limit = resultsLimit { operation.resultsLimit = limit }
        return operation
    }

    func statusText() async throws -> String {
        switch try await container.accountStatus() {
        case .available:
            return "available"
        case .couldNotDetermine:
            return "could not determine"
        case .noAccount:
            return "no account"
        case .restricted:
            return "restricted"
        case .temporarilyUnavailable:
            return "temporarily unavailable"
        default:
            return "unknown"
        }
    }

    // See https://nemecek.be/blog/31/how-to-setup-cloudkit-subscription-to-get-notified-for-changes.
    // This requires adding the "Background Modes" capability
    // and checking "Remote notifications".
    // Supposedly subscriptions do not work in the Simulator.
    func subscribe(recordType: CKRecord.RecordType) async throws {
        let subscription = CKQuerySubscription(
            recordType: recordType,
            predicate: NSPredicate(value: true), // all records
            options: [
                .firesOnRecordCreation,
                .firesOnRecordDeletion,
                .firesOnRecordUpdate
            ]
        )

        let info = CKSubscription.NotificationInfo()
        info.shouldSendContentAvailable = true
        info.alertBody = "" // if this isn't set, pushes aren't always sent
        subscription.notificationInfo = info
        try await database.save(subscription)
    }

    // MARK: - CRUD Methods

    // "C" in CRUD.
    func create<T: CloudKitable>(item: T) async throws {
        try await database.save(item.record)
    }

    // "R" in CRUD.
    func retrieve<T: CloudKitable>(
        recordType: CKRecord.RecordType,
        predicate: NSPredicate = NSPredicate(value: true), // gets all
        sortDescriptors: [NSSortDescriptor]? = nil,
        resultsLimit: Int = CKQueryOperation.maximumResults
    ) async throws -> [T] {
        let query = CKQuery(recordType: recordType, predicate: predicate)
        query.sortDescriptors = sortDescriptors
        let (results, cursor) = try await database.records(
            matching: query,
            resultsLimit: resultsLimit
        )

        var objects: [T] = []

        for (_, result) in results {
            let record = try result.get()
            objects.append(T(record: record)!)
        }

        try await retrieveMore(cursor, &objects)

        return objects
    }

    private func retrieveMore<T: CloudKitable>(
        _ cursor: Cursor?, _ objects: inout [T]
    ) async throws {
        guard let cursor = cursor else { return }

        let (results, newCursor) =
            try await database.records(continuingMatchFrom: cursor)

        for (_, result) in results {
            let record = try result.get()
            objects.append(T(record: record)!)
        }

        // Recursive call.
        try await retrieveMore(newCursor, &objects)
    }

    // "U" in CRUD.
    func update<T: CloudKitable>(item: T) async throws {
        try await database.save(item.record)
    }

    // "D" in CRUD.
    func delete<T: CloudKitable>(item: T) async throws {
        try await database.deleteRecord(withID: item.record.recordID)
    }
}
```

When querying for records, to limit the fields included in the returned data,
set the `desiredKeys` property on the `CKQueryOperation` object
to an array of property name strings.
