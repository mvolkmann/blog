---
eleventyNavigation:
  key: Core Data
  parent: Swift
layout: topic-layout.njk
---

## Overview

Core Data is an object/graph persistence framework.
It supports many features including
data validation, undo/redo, and lazy loading.
By default it stores data using SQLite.

Apps that use Core Data will not run in Preview mode.
The Simulator must be used instead.

One way to setup use of Core Data is to
check the "Use Core Data" checkbox on the options panel
when creating a new project.
This adds all the setup code and UI code for a simple app
that allows creating, deleting, and listing `Item` entities.
The app can be modified to work with other kinds of entities.

Another approach is to uncheck the "Use Core Data" checkbox
and set up use of Core Data manually.

## "Use Core Data" Option

When the "Use Core Data" checkbox is checked,
the generated files configure the use of Core Data.
The file `{app-name}App.swift` registers an "environment"
with the main `ContentView` that uses
a `PersistenceController` instance defined in `Persistence.swift`
and made available through a static `shared` property.
The main view defined in `ContentView.swift` has access to this
and uses the `@FetchRequest` property wrapper
to get `Item` objects from the persistent store.
The `Item` type is defined in `{app-name}.xcdatamodeld`
and has a single attribute `timestamp`.

The generated app is fully functional.
It displays a list of items that each have a timestamp.
Tap an item to navigate to a screen that displays detail about just that item.
To delete an item, swipe it left and click the "Delete" button that appears.
To add an item with the current timestamp, click the "+" button at the top.
To delete items without swiping them, click the "Edit" button at the top.
This displays a red circle containing a minus sign to left of each item.
Click those to delete items.
When finished, click the "Done" button at the top
that replaced the "Edit" button.
From here you can edit the code to provide your own CRUD functionality.

## Manual Setup

When the "Use Core Data" checkbox was not checked,
a data model must be created manually as follows:

1. Select File ... New File... or press cmd-n.
1. Filter the list of templates by entering "Data".
1. In the "Core Data" section, select "Data Model" template.
1. Click the "Next" button.
1. Select the directory where it should be stored.
1. For most projects the default file name of "Model.xcdatamodeld" is fine.
1. Click the "Create" button.

## Defining Entities

To define Core Data entity types,
click the `{app-name}.xcdatamodeld` file in the Navigator pane.
This will display an entity editor.

If the "Use Core Data" option was checked,
consider deleting the provided `Item` entity
and any code that is specific to it.

Entities are similar to tables in a relational database.
Add entities to the model by clicking the "Add Entity" button at the bottom.
This will create an empty entity named "Entity".
Double-click the name and change it.
These become the names of generated class definitions,
so they should begin with an uppercase letter.
It is recommended to give them names that end in "Entity"
so it is clear in code that uses them that they are Core Data entities.
The generated classes inherit from `NSManagedObject`.

For each entity, click the "Module" drop-down inside
the Inspector on the right, select "Current Product Module".
TODO: Is this really desirable? What is the benefit?

Add attributes to each entity by
clicking the "+" at the bottom of the attribute list.
Each attribute is given a default name of "attribute".
Double-click the name and change it.
Some names, such as "description" and "for", are not allowed.
Select a type from a drop-down list of primitive types.
The type "Data" is for byte buffers.

Optionally sort the attributes.
They can be sorted on their name or type
by clicking on the "Attribute" or "Type" column heading.

By default each attribute is optional.
For any that are required, select the attribute and
uncheck the "Optional" checkbox in the inspector.

Optionally specify default values for each attribute.
Select an attribute and enter a value for "Default Value" in the inspector.

Optionally specify validation criteria for each attribute.
Select an attribute and enter values for "Validation" in the inspector.

Optionally add relationships between entities.
Each relationship specifies a
Relationship name, Destination, Type, and Delete Rule.
The "Type" in the Inspector can be "To One" or "To Many".

The "Delete Rule" in the Inspector can be "Nullify", "Cascade", or "Deny".

- "Nullify" means instances can be deleted
  without also deleting related entities.
  References to deleted entities are set to `nil`.
- "Cascade" means instances can be deleted
  and related entities will also be deleted.
- "Deny" means instances with related entities cannot be deleted.

Specify inverse relationships for most relationships.
For example, "PersonEntity" can have
an "owns" relationship "To Many" "DogEntity" and
"DogEntity" can have an "ownedBy" relationship "To One" "PersonEntity".

Optionally view the entities and their relationships as a graph.
To switch between viewing entities and their relationships
in "table" or "graph" style,
click the buttons in the lower-right labelled "Editor Style".
In graph view it may be necessary to toggle the relationships
in each entity by clicking the triangle to the left of "Relationships"
in order to see all of them.

If you see the error "cannot find type 'SomeEntityName' in scope",
close and reopen the project in Xcode.
In some cases, such as renaming entities,
it is necessary to clear existing data and start over.
To do this:

- clean the build folder by pressing cmd-shift-k
- quit out of Xcode
- cd to ~/Library/Developer/Xcode/DerivedData
- run the `rm -rf` command on the directory for the project
- restart Xcode

## Entity Codegen

When an entity is selected in the Core Data model editor,
the Inspector displays its configuration options.
One of these is "Codegen" that has a default value of "Class Definition".
This means that Xcode will automatically generate a class definition
for the entity.
Typically this is the desired option.

By default, the source files for generated entity classes
do not appear in the Navigator.
However, they can be viewed by selecting
Product ... Show Build Folder in Finder.
The source files can be found in the ridiculously deep directory structure
Build/Intermediates.noindex/{app-name}.build/Debug-iphonesimulator/
{app-name}.build/DerivedSources/CoreDataGenerated/Model directory.

There are two generated files for each entity.
`{entity-name}+CoreDataClass.swift` defines the class
and should not be modified.
`{entity-name}+CoreDataProperties.swift` defines extensions to the class
and can be safely modified to add computed properties and methods.

Choosing a different value for the entity "Codegen" enables
generating entity sources files that appear in the Navigator.
The "Manual/None" option means that Xcode will not automatically generate
either of the files described above.
The "Category/Extension" option means that Xcode will
generate the `{entity-name}+CoreDataClass.swift` files,
but not the `{entity-name}+CoreDataProperties.swift` files.
TODO: Why does this also generate "Class" files?

To request generation of source files for entities whose
"Codegen" option is set to a value other than "Class Definition":

- Select Editor ... Create NSManagedObject Subclass...
- Select a model and click "Next". Most projects only have one model.
- Select the entities for which code should be generated and click "Next".
- Select the project subdirectory where the source files should be written.
  TODO: It seems this is ignored and the files are
  always written to the top project directory.
- Click "Create".

The generated "Class" files define a class for an entity
that inherits from `NSManagedObject`.
These file should not be modified.

The generated "Properties" files define extensions to the entity that
include a `fetchRequest` method.
This fetches all the instances of the entity,
properties that correspond to the entity attributes,
and methods for adding and removing other kinds of entities
that have a relationship to this one.
Theses file can be modified to add computed properties and methods.

## @FetchRequest vs. ViewModel

There are two popular ways for views to access data from Core Data.
One is to use `@FetchRequest`.
The other is to define a ViewModel class that views can use
which abstracts the use of Core Data away from them.

### Using @FetchRequest

TODO: Add this based on code in your SwiftUI-GiftTrack project.

### Using a ViewModel

Here are the steps to define and use a ViewModel
for accessing data in Core Data.

- Create a view model class.

  This can be defined in a file named "ViewModel.swift".
  It can define a class named "ViewModel" that inherits from `ObservableObject`
  which comes from the Combine framework.

- Declare a `container` constant property as follows:

  ```swift
  let container: NSPersistentContainer
  ```

- Declare a `context` variable property as follows:

  ```swift
  var context: NSManagedObjectContext { container.viewContext }
  ```

- Declare a published array property to hold
  all the instances of each entity type as follows:

  ```swift
  @Published var people: [PersonEntity] = []
  ```

- If Xcode doesn't recognize the entity types,
  close the project and reopen it.

- Define an initializer as follows:

  ```swift
  init() {
      // "Model" here must be the name of the "Data Model" file.
      container = NSPersistentContainer(name: "Model")

      container.loadPersistentStores { _, error in
          if let error = error {
              print("error loading Core Data:", error)
          } else {
              self.fetchPeople()
              // Possibly also fetch other kinds of entities here.
          }
      }
  }
  ```

- Define a method for each entity type that fetches all of its instances.
  The `List` view is useful for displaying them in a list.

  ```swift
  func fetchPeople() {
      // "PersonEntity" here must be the name of the entity type.
      let request = NSFetchRequest<PersonEntity>(entityName: "PersonEntity")
      // Optionally specify how the instances should be sorted.
      request.sortDescriptors = [
          NSSortDescriptor(
              key: "name",
              ascending: true,
              #selector(NSString.localizedStandardCompare) // case-insensitive
          )
      ]
      do {
          // people here must be the name of the
          // @Published property declared above.
          people = try context.fetch(request)
      } catch {
          print("fetchPeople error:", error.localizedDescription)
      }
  }
  ```

- Optionally add filtering to the `request` object above.

  TODO: Describe filter options here!

- Define a method to save changes to any data in the context.

  ```swift
  func saveContext() {
      do {
          try context.save()
      } catch {
          print("saveContext error:", error)
      }
  }
  ```

- Define a method for each entity type that the UI can call
  to add a new entity instance.

  ```swift
  func addPerson(name: String) {
      let person = PersonEntity(context: context)
      // Set all the attributes of the new entity instance.
      person.name = name
      saveContext()
      people.append(person)
      people.sort { ($0.name ?? "") < ($1.name ?? "") }
  }
  ```

- Define a method for each entity type that the UI can call
  to delete an entity instance.

  ```swift
  func deletePeople(indexSet: IndexSet) {
      for index in indexSet {
          context.delete(people[index])
      }
      saveContext()
  }
  ```

- In each view that needs to access entity data,
  declare a property with the `@StateObject` property wrapper
  that has a type of `ViewModel`.
  Do not declare this to be `private` because
  it will be passed in from a parent view.

  ```swift
  @StateObject var vm: ViewModel
  ```

- To add an entity instance,
  call an "add" method defined in `ViewModel.swift`.

  ```swift
  vm.addPerson(name: name)
  ```

- To delete an entity instance,
  call a "delete" method defined in `ViewModel.swift`.
  Often this is done with the `onDelete` view modifier on a `ForEach`
  because that provides an `IndexSet` of selected indexes.

  ```swift
  .onDelete(perform: vm.deletePerson)
  ```

- To update an entity instance directly modify the attributes
  of an existing entity and then call the `saveContext` method

  ```swift
  person.name = "Some New Name"
  vm.savePeople()
  ```

## Entity Objects

Core Data entity objects have many properties and methods.

Each attribute is represented by a property
that can be directly accessed and modified.
Changes are not persisted unless `context.save()` is called.

Each relationship is represented by a property.

Properties for "To One" relationships have
an optional type matching the referenced entity type.
For example, `DogEntity` with an `ownedBy` relationship to a `PersonEntity`
has an `ownedBy` property with a type of `PersonEntity?`.
This can be directly set to a `PersonEntity` or `nil`.
For example, `someDog.ownedBy = somePerson` or `someDog.ownedBy = nil`.

Properties for "To Many" relationships have a type of optional `NSSet`.
For example, `PersonEntity` with an `owns` relationship to `DogEntity` instances
has an `owns` property whose type is `NSSet?`.
There are also `addToOwns` and `removeFromOwns` methods on the entity
that take a `DogEntity` and add to or remove from the `NSSet`.
For example, `somePerson.addToOwns(someDog)`
or `somePerson.removeFromOwns(someDog)`.

TODO: How can all instances of a given entity type be deleted?

## Fetching Entities

To fetch entities from an `NSManagedObjectContext`,
create an `NSFetchRequest` that identifies an entity type.
Optionally specify how the fetched entity instances should be sorted.
Also, optionally specify filtering to be applied
so that only a subset of the the entity instances will be fetched.

For details on the syntax used to specify filtering, see {% aTargetBlank
"https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Predicates/Articles/pSyntax.html",
"Predicate Format String Syntax" %}.

```swift
let request = NSFetchRequest<PersonEntity>(entityName: "PersonEntity")

// Sort in ascending order on the "name" property.
request.sortDescriptors = [
    NSSortDescriptor(
        key: "name",
        ascending: true,
        #selector(NSString.localizedStandardCompare) // case-insensitive
    )
]

// Filter so only entities with a name beginning with "T" are fetched.
request.predicate = NSPredicate(format: "name beginswith %@", "T")

do {
    people = try context.fetch(request)
} catch {
    print("fetchPeople error:", error)
}
```

## Starting Over

The easiest way to delete the data an app has stored in Core Data
and start over with a clean slate
is to delete the app from the Simulator or a device
and reinstall it.

## CloudKit Integration

The contents of a Core Data database can be automatically synchronized
with a CloudKit database in iCloud.

When the database is "private" (the default), the data is
shared between all devices owned by a single user.
When the database is "public", the data is
shared between all users and all devices.
When the database is "shared", the data is
shared only with specific users.

For a working example, see this {% aTargetBlank
"https://github.com/mvolkmann/swiftui-cloudkit-core-data", "GitHub project" %}.

Both Core Data and CloudKit support storing structured data,
but they use different terminology.

| Topic   | Core Data              | CloudKit                       |
| ------- | ---------------------- | ------------------------------ |
| objects | `NSManagedObject`      | `CKRecord`                     |
| models  | `NSManagedObjectModel` | `Schema`                       |
| stores  | `NSPersistentStore`    | `CKRecordZone` or `CKDatabase` |

Conversions between these types are performed automatically by
`NSPersistentCloudKitContainer`.
The file `Model.swift` in the example app,
creates a container of this type and ties it to the model
defined in the file `Model.xcdatamodeld`.

To combine the use of Core Data and CloudKit:

1. Create a new app and check the "Use Core Data"
   and "Host in CloudKit" checkboxes.
1. Add the "iCloud" capability.
1. Under "Services", check the "CloudKit" checkbox.
1. Under "Containers", click the "+" button and enter a container name
   that is "iCloud." plus your reverse internet domain
   followed by a period and the app name.
1. Add the "Background Modes" capability.
1. Check the "Remote notifications" checkbox.
1. Add source files similar to those in the example app linked above.

CloudKit prefixes all record and field names with "CD\_",
which is an abbreviation for "Core Data".
A system record named "Users" is created by default.
These records cannot be queried.

By default, a private database is created.
This is restricted to the current app user,
but is shared across all of their devices.

To enable querying records from the CloudKit Dashboard website:

1. Browse the CloudKit Dashboard at
   https://icloud.developer.apple.com/dashboard/.
1. Click the big "CloudKit Database" button.
1. Select a CloudKit database from the dropdown at the top.
1. In the left nav under "Schema", click "Indexes".
1. For each record type, select it,
   click "Add Basic Index", select "recordName" from the dropdown,
   verify that an "Index Type" of "Queryable" is selected,
   click "Add Basic Index" again, select "modifiedTimestamp" from the dropdown,
   verify that an "Index Type" of "Queryable" is selected,
   and click the "Save Changes" button.
1. Under "Data" in the left nav, click "Records".
1. In the Database dropdown, select "Private Database".
1. In the Zone dropdown, select the zone created for you
   by `NSPersistentCloudKitDatabase` which is named
   "com.apple.coredata.cloudkit.zone", not "\_defaultZone".
1. In the "Record Type" dropdown, select a record name.
1. Click the "Query Records" button.
   Note that after adding records in the app it can take a minute
   until they appear in the CloudKit Dashboard.
1. To create a new record, click the "+" button after the "Records" heading,
   select a record type from the "Type" dropdown,
   select "Create New Record", enter a record name in the "CD_entityName" input,
   enter attribute values in the remaining inputs,
   and click the "Save" button.
   Relationship attribute values must be set to the unique id string
   of record to which they refer.
1. To modify record attributes, click a record name (blue link),
   edit data in the inspector panel on the right,
   click the "Save" button.
   To undo changes before saving them, click the "Reload" button.
1. To see changes in the record table, re-run the query.
1. To delete a record, click a record name (blue link),
   click the "Delete" button, and confirm by clicking another "Delete" button.
