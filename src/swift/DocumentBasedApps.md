---
eleventyNavigation:
  key: Document-based Apps
  parent: Swift
layout: topic-layout.njk
---

## Overview

SwiftUI document-based apps "are apps that let users
create, edit, and share documents such as text files."

The information below was derived form the Hacking With Swift article at
{% aTargetBlank
"https://www.hackingwithswift.com/quick-start/swiftui/how-to-create-a-document-based-app-using-filedocument-and-documentgroup",
"How to create a document-based app using FileDocument and DocumentGroup" %}.

## Steps

In this example we are building a way to create Lua source files
in a SwiftUI app.

The steps to create such a document-based app are:

1. Optionally create an icon for your file type.
   Create an image that is 1024 x 1024.
   Browse {% aTargetBlank "https://cloudconvert.com/png-to-icns",
   "cloudconvert" %}, click the "Select File" button, select your file,
   select "ICNS" in the "Convert to" dropdown, and click the "Convert" button.
   When it completes, click the "Download" button.

1. Define a custom file extension for your documents.

<img alt="SwiftUI Exported Type Identifiers" style="width: 100%"
    src="/blog/assets/SwiftUI-Exported-Type-Identifiers.png?v={{pkg.version}}"
    title="SwiftUI Exported Type Identifiers">

1. In the struct that conforms to `App`, in the computed property `body`,
   replace the following:

   ```swift
   WindowGroup {
       ContentView()
   }
   ```

   with this:

   ```swift
   DocumentGroup(newDocument: TextFile()) { file in
       ContentView(document: file.$document)
   }
   ```

1. Define a struct that conforms to the {% aTargetBlank
      "https://developer.apple.com/documentation/swiftui/filedocument",
      "FileDocument" %} protocol.
   This will hold all the data associated with each document
   using one or more properties.
   For example, for basic text documents we could create
   the file `TextFile.swift` containing the following:

   ```swift
   import SwiftUI
   import UniformTypeIdentifiers

   extension UTType {
       static let luaSource = UTType(exportedAs: "org.lua")
   }

   struct LuaFile: FileDocument {
       static let fileExtension = "lua"
       static var readableContentTypes = [UTType.luaSource]
       static let defaultCode = """
       x = 3
       y = 4
       if x >= y then
         color = "green"
       else
         color = "red"
       end
       -- color = x >= y and "red" or "green"

       greeting = "Hello!"
       """

       var code = ""

       // This creates a document containing the given code.
       init(initialCode: String = defaultCode) {
           code = initialCode
       }

       // This creates a document containing previously saved code.
       init(configuration: ReadConfiguration) throws {
           if let data = configuration.file.regularFileContents {
               code = String(decoding: data, as: UTF8.self)
           } else {
               throw CocoaError(.fileReadCorruptFile)
           }
       }

       // This is called when the system wants to save code in a file.
       // TODO: How can the file name be specified?
       func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
           let data = Data(code.utf8)
           return FileWrapper(regularFileWithContents: data)
       }
    }
   ```

1. Add a view for editing the current document.
   This can be a custom UI that is suitable for the struct defined above.
   For a text document it could be as simple as a `TextEditor` view.
   For example:

   ```swift
   @Binding var luaFile: LuaFile

   ...

   TextEditor(text: $luaFile.text)
       .autocapitalization(.none)
       .disableAutocorrection(true)
       .lineLimit(lines)
       .frame(
           maxWidth: .infinity,
           maxHeight: CGFloat(lines * lineHeight)
       )
       .border(.gray)
   ```

1. Change the project settings to indicate that it
   will use the system document browser.

   1. Select the top entry in the Project Navigator.
   1. Select the main target.
   1. Select the "Info" tab.
   1. Hover over an existing row and click the "+" button.
   1. Select "Supports Document Browser".
   1. Set the value to "YES".

## Result

The app in the screenshots below can be found at {% aTargetBlank
"https://github.com/mvolkmann/SwiftUICallsC", "SwiftUICallsC" %}.
It demonstrates embedding the Lua interpreter in a SwiftUI app.
Each document contains Lua code that is
executed by tapping the "Execute" button.

When users run the app they are presented with a document picker screen.
There are three tabs at the bottom labelled "Recents", "Shared", and "Browse".
The "Recents" tab displays thumbnail buttons for recently accessed documents.
Initially there will not be any.
In the screenshot below, two documents named "Complex" and "Simple"
have already been created.

<img alt="SwiftUI Document-Based App Recents" style="width: 50%"
src="/blog/assets/SwiftUI-document-based-1.png?v={{pkg.version}}"
title="SwiftUI Document-Based App Recents">

The "Browse" tab displays a "Create Document" button along with
thumbnail buttons for all documents already created.

<img alt="SwiftUI Document-Based App Browse" style="width: 50%"
src="/blog/assets/SwiftUI-document-based-2.png?v={{pkg.version}}"
title="SwiftUI Document-Based App Browse">

Clicking the "Create Document" button or the button for an existing document
advances to the view passed to `DocumentGroup`
inside the struct that conforms to `App`.

<img alt="SwiftUI Document-Based App Document" style="width: 50%"
src="/blog/assets/SwiftUI-document-based-3.png?v={{pkg.version}}"
title="SwiftUI Document-Based App Document">

To return to the document picker screen, tap the "<" button in the upper-left.

To rename a document, long press it, select "Rename",
and replace the current name.

```

```
