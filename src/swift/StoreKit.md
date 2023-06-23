---
eleventyNavigation:
  key: StoreKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/storekit",
"StoreKit" %} is an Apple framework that supports
in-app purchases, ad network attribution, Apple Music integration,
and enabling app ratings and reviews.

## Resources

- WWDC 2022 video {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2022/10007/",
  "What's new with in-app purchase" %}
- WWDC 2022 video {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2022/110404/",
  "Implement proactive in-app purchase restore" %}
- WWDC 2022 video {% aTargetBlank "https://developer.apple.com/videos/play/wwdc2022/10039/",
  "What's new in StoreKit testing" %}
- WWDC 2023 video {% aTargetBlank "https://developer.apple.com/wwdc23/10013",
  "Meet StoreKit for SwiftUI" %}

## Usage

To use StoreKit in an app:

1. Select File ... New ... File... or press cmd-n.
1. Scroll the "Other" category.
1. Select "StoreKit Configuration File".
1. A file name can be entered, but the default name
   of "Configuration.storekit" is fine.
1. Click the "+" in the lower-left of the editor and select a type.
   For one-time purchases, select "Add Non-Consumable In-App Purchase".
1. Enter a "Reference Name". This can be the app name.
   To find this, click the top entry in the Navigator, select the "General" tab,
   and note the value of Identity ... Display Name.
1. Enter a "Product ID". This can be the project bundle identifier.
   To find this, click the top entry in the Navigator, select the "General" tab,
   and note the value of Identity ... Bundle Identifier.
1. Enter a price.
1. Under "Localizations", double-click an option such as "English (U.S.)".
1. Enter a "Display Name".
   This can be the same as the Reference Name entered above.
1. Enter a "Description" of the purchasable item.
1. Click "+" under "Localizations" to add more supported locales.
1. Repeat steps 5-12 for each additional non-consumable that can be purchased.
1. Create a new Swift file. A good name is `StoreKit.swift`.
1. Add the following in `StoreKitStore.swift`:

   ```swift
    import StoreKit

    class StoreKitStore: NSObject, ObservableObject {
        // This is a Set of purchasable product ids.
        private var allProductIdentifiers =
            Set(["r.mark.volkmann.gmail.com.GiftTrack"])

        private var productsRequest: SKProductsRequest?

        private var fetchedProducts: [SKProduct] = []

        typealias CompletionHandler = ([SKProduct]) -> Void

        private var completionHandler: CompletionHandler?

        override init() {
            super.init()
            fetchProducts { products in
                print("products =", products)
            }
        }

        private func fetchProducts(
            _ completion: @escaping CompletionHandler
        ) {
            guard productsRequest == nil else { return }
            completionHandler = completion
            productsRequest =
                SKProductsRequest(productIdentifiers: allProductIdentifiers)
            productsRequest?.delegate = self
            productsRequest?.start()
        }
    }

    extension StoreKitStore: SKProductsRequestDelegate {
        func productsRequest(
            _ request: SKProductsRequest,
            didReceive response: SKProductsResponse
        ) {
            let loadedProducts = response.products
            let invalidProducts = response.invalidProductIdentifiers
            guard !loadedProducts.isEmpty else {
                print("failed to load products")
                if !invalidProducts.isEmpty {
                    print("invalid products found: \(invalidProducts)")
                }
                productsRequest = nil
                return
            }

            // Cache the fetched products.
            fetchedProducts = loadedProducts

            // Notify listeners of loaded products.
            DispatchQueue.main.async {
                self.completionHandler?(loadedProducts)
                self.completionHandler = nil
                self.productsRequest = nil
            }
        }
    }
   ```

1. In the `{app-name}.swift` file, add the following
   inside the struct that inherits from `App`:

   ```swift
   @StateObject private var store = StoreKitStore()
   ```

1. In any views that need to access the store, add the following:

   ```swift
   @EnvironmentObject private var store: StoreKitStore
   ```

1. In the Navigator, select `{app-name}.swift`.
1. In the top bar, click the project name to the left of the device drop-down
   and select "Edit Scheme..." which opens a dialog.
1. Click "Run" in the dialog left nav.
1. Click the "Options" tab.
1. Change the value of "StoreKit Configuration"
   from "None" to "Configuration.storekit".
1. Click the "Close" button.

## Displaying Products

To display products in SwiftUI, get an array of product ids
where each is a `String` and pass it to the `StoreView` view.

```swift
StoreView(ids: productIds)
```

This produces a UI that works on all platforms including
iOS, iPadOS, macOS, and watchOS.

This is best tested in the Simulator rather than in Previews.

## Testing from Xcode

Xcode and the Simulator can be used to test in-app purchases
without the need to configure them in AppStoreConnect.
See the YouTube video {% aTargetBlank
"https://www.youtube.com/watch?v=o_YMsmmkfFc&feature=youtu.be",
"How to easily test InApp Purchases in an iOS app" %} with Josh Holtz.

To create a StoreKit configuration file to be used for testing:

1. Select File ... New ... File...
1. Select "StoreKit Configuration File" and click the "Next" button.
1. Enter a name like "Test". The file will have an extension of `.storekit`.
1. If you have already described the products in AppStoreConnect,
   check the "Sync this file with an app in App Store Connect" checkbox.
1. Click the "Next" button.
1. Select the top project directory that contains the main `.swift` file.
1. Click the "Create" button.

To describe products in the new StoreKit configuration file:

1. In the special editor that opens for the new StoreKit configuration file,
   for each product to be offered by the app:

   1. Click the "+" in the lower-left and select a kind of product to add
      such as "Add Non-Consumable In-App Purchase".
   1. Enter a "Reference Name" that describes the product.
   1. Enter a product ID that is unique among all of your apps.
      It could be the app bundle ID followed by a product-specific identifier.
   1. Enter a price.
   1. Optionally select "On" for "Family Sharing".
   1. Expand the "Localizations" section.
   1. For each language to be supported,
      enter a "Display Name" and "Description" for the product.

To tell the Simulator to use this new `.storekit` configuration file:

1. Click the dropdown to the left of the device dropdown
   at the top of the Xcode window.
1. Select "Edit Scheme..."
1. In the left nav, select "Run".
1. Select the "Options" tab.
1. Change the value selected in the "StoreKit Configuration" dropdown
   to the name of the new `.storekit` configuration file.
1. Click the "Close" button.

To test an in-app purchase:

1. Run the app in the Simulator and make an in-app purchase.
1. If the products do not appear in the app, try running it again.
1. In Xcode, select Debug ... StoreKit ... Manage Transactions...
   to see a history of all the in-app purchase transactions.
1. Right-click a transaction to get a menu with the options
   "Approve Transaction", "Decline Transaction", "Refund Purchase",
   "Resolve Issue", "Request Price Increase Consent",
   and "Delete Transaction".
1. For subscriptions,
   click the "Subscription Options" button below a transaction
   to get a dialog where the subscription can be cancelled.
