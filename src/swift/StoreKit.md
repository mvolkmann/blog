---
eleventyNavigation:
  key: StoreKit
  parent: Swift
layout: topic-layout.njk
---

## StoreKit

{% aTargetBlank "https://developer.apple.com/documentation/storekit",
"StoreKit" %} is an Apple framework that supports
in-app purchases, ad network attribution, Apple Music integration,
and enabling app ratings and reviews.

To use StoreKit in an app:

1. Create a new file.
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
1. Create a new Swift file. A good name is `StoreKit.svelte`.
1. Add the following in `StoreKitStore.svelte`:

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
