---
eleventyNavigation:
  key: Contacts
  parent: Swift
layout: topic-layout.njk
---

## Overview

iOS apps can access data managed by the Apple Contacts app.

## ContactPicker

We can define a `ContactPicker` SwiftUI view that allows the user
to select a contact using the same UI as the Apple Contacts app.
This is done using the {% aTargetBlank
"https://developer.apple.com/documentation/contactsui", "Contacts UI" %}
framework and its {% aTargetBlank
"https://developer.apple.com/documentation/contactsui/cncontactpickerviewcontroller",
"CNContactPickerViewController" %} class.

```swift
import ContactsUI
import SwiftUI

struct ContactPicker: UIViewControllerRepresentable {
    class Coordinator: NSObject, CNContactPickerDelegate {
        private var parent: ContactPicker

        init(_ parent: ContactPicker) {
            self.parent = parent
        }

        func contactPicker(
            _ picker: CNContactPickerViewController,
            didSelect contact: CNContact
        ) {
            parent.contact = contact
        }
    }

    @Binding private var contact: CNContact?

    init(contact: Binding<CNContact?>) {
        _contact = contact
    }

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeUIViewController(context: Context)
        -> CNContactPickerViewController {
        let vc = CNContactPickerViewController()
        vc.delegate = context.coordinator
        return vc
    }

    // This method is required, but in this case it doesn't need to anything.
    func updateUIViewController(
        _ uiViewController: CNContactPickerViewController,
        context: Context
    ) {}
}
```

To use this in a SwiftUI view, do something like the following:

```swift
import Contacts
import SwiftUI

struct ContentView: View {
    @State private var cellNumber = ""
    @State private var contact: CNContact?
    @State private var firstName = ""
    @State private var lastName = ""

    var body: some View {
        VStack {
            Text(firstName)
            Text(lastName)
            Text(cellNumber)
            Button("Find in Contacts") {
                isFindingContact = true
            }
            .buttonStyle(.borderedProminent)
        }
        .onChange(of: contact) { _ in
            guard let contact else { return }

            firstName = contact.givenName
            lastName = contact.familyName

            // Find the first "Mobile" phone number.
            var phone = contact.phoneNumbers.first { phoneNumber in
                guard let label = phoneNumber.label else { return false }
                return label.contains("Mobile")
            }

            // If none was found, just use the first phone number.
            if phone == nil { phone = contact.phoneNumbers.first }

            // Get the phone number from this object.
            if let phone { cellNumber = phone.value.stringValue }
        }
        .sheet(isPresented: $isFindingContact) {
            ContactPicker(contact: $contact)
        }
    }
}
```
