---
eleventyNavigation:
  key: HealthKit
  parent: Swift
layout: topic-layout.njk
---

## Overview

[HealthKit](<https://developer.apple.com/documentation/healthkit?v=1.1.1>) is a library from Apple to
"Access and share health and fitness data
while maintaining the user’s privacy and control."

It supports three categories of tasks:

1. collect and store health and fitness data
1. analyze and visualize the data
1. enable social interactions

"Because health data may contain sensitive, personal information,
apps must receive permission from the user to
read data from or write data to the HealthKit store."

## Flutter

The Flutter pub.dev package [health](<https://pub.dev/packages/health?v=1.1.1>)
can access data from both Apple HealthKit and Android "Google Fit".

See my project flutter_health which is not yet working.

## Available Data

The data available in HealthKit includes:

- Activity (`HKQuantityType`)

  - `activeEnergyBurned`
  - `appleExerciseTime`
  - `appleMoveTime`
  - `appleStandHour` (`HKCategoryTypeIdentifier`)
  - `appleStandTime`
  - `basalEnergyBurned`
  - `distanceCycling`
  - `distanceDownhillSnowSports`
  - `distanceSwimming`
  - `distanceWalkingRunning`
  - `distanceWheelchair`
  - `flightsClimbed` (stairs)
  - `lowCardioFitnessEvent` (`HKCategoryTypeIdentifier`)
  - `nikeFuel` (points earned)
  - `pushCount` (wheelchair)
  - `stepCount`
  - `swimmingStrokeCount`
  - `vo2Max`
  - `walkingSpeed`
  - `walkingStepLength`

- Blood (`HKQuantityType`)

  - `bloodAlcoholContent`
  - `bloodGlucose`
  - `bloodPressureDiastolic`
  - `bloodPressureSystolic`
  - `oxygenSaturation`

- Body Measurements (`HKQuantityType`)

  - `bodyFatPercentage`
  - `bodyMass`
  - `bodyMassIndex`
  - `height`
  - `leanBodyMass`
  - `waistCircumference`

- Breathing (`HKQuantityType`)

  - `forcedExpiratoryVolume1`
  - `inhalerUsage`
  - `peakExpiratoryFlowRate`
  - `respiratoryRate`

- Characteristics (`HKCharacteristicType`)

  - `activityMoveMode` (TODO: What is this?)
  - `biologicalSex`
  - `bloodType`
  - `dateOfBirth`
  - `fitzpatrickSkinType`
  - `wheelchairUse`

- Dietary (`HKQuantityType`)

  - `dietaryBiotin`
  - `dietaryCaffeine`
  - `dietaryCalcium`
  - `dietaryCarbohydrates`
  - `dietaryChloride`
  - `dietaryCholesterol`
  - `dietaryChromium`
  - `dietaryCopper`
  - `dietaryEnergyConsumed`
  - `dietaryFatMonounsaturated`
  - `dietaryFatPolyunsaturated`
  - `dietaryFatSaturated`
  - `dietaryFatTotal`
  - `dietaryFiber`
  - `dietaryFolate`
  - `dietaryIodine`
  - `dietaryIron`
  - `dietaryMagnesium`
  - `dietaryManganese`
  - `dietaryMolybdenum`
  - `dietaryNiacin`
  - `dietaryPantothenicAcid`
  - `dietaryPhosphorus`
  - `dietaryPotassium`
  - `dietaryProtein`
  - `dietaryRiboflavin`
  - `dietarySelenium`
  - `dietarySodium`
  - `dietarySugar`
  - `dietaryThiamin`
  - `dietaryVitaminA`
  - `dietaryVitaminB12`
  - `dietaryVitaminB6`
  - `dietaryVitaminC`
  - `dietaryVitaminD`
  - `dietaryVitaminE`
  - `dietaryVitaminA`
  - `dietaryVitaminK`
  - `dietaryWater`
  - `dietaryZinc`

- Hearing (`HKQuantityType`)

  - `environmentalAudioExposure`
  - `environmentalAudioExposureEvent` (`HKCategoryTypeIdentifier`)

    "sent when the average sound level reaches or exceeds
    a specified threshold for three minutes"

  - `headphoneAudioExposure`
  - `headphoneAudioExposureEvent` (`HKCategoryTypeIdentifier`)

    "when the device generates a notification about loud headphone audio"

- Heart (`HKQuantityType`)

  - heartbeat series

    See [HKHeartbeatSeriesQuery](<https://developer.apple.com/documentation/healthkit/hkheartbeatseriesquery?v=1.1.1>).

  - `heartRate`
  - `heartRateVariabilitySDNN`
  - `highHeartRateEvent` (`HKCategoryTypeIdentifier`)

    For example, "Your heart rate rose above 120 BPM while you
    seemed to be inactive for 10 minutes starting at 10:06 AM."

  - `irregularHeartRhythmEvent` (`HKCategoryTypeIdentifier`)

    "might be suggestive of atrial fibrillation (AFib)"

  - `lowHeartRateEvent` (`HKCategoryTypeIdentifier`)

    This is similar to a `highHeartRateEvent`.

  - `restingHeartRate`
  - `walkingHeartRateAverage`

- Reproductive Health (`HKCategoryTypeIdentifier`)

  - `basalBodyTemperature` (`HKQuantityType`)
  - `cervicalMucusQuality`
  - `contraceptive`
  - `intermenstrualBleeding`
  - `lactation`
  - `menstrualFlow`
  - `ovulationTestResult`
  - `pregnancy`
  - `pregnancyTestResult`
  - `progesteroneTestResult`
  - `sexualActivity`

- Vital Signs (`HKQuantityType`)

  - `bodyTemperature`

  - electrocardiogram data

    See [HKElectrocardiogramQuery](<https://developer.apple.com/documentation/healthkit/hkelectrocardiogramquery?v=1.1.1>).

  - `forcedVitalCapacity`

    This is the standard deviation of [NN intervals](<https://hexoskin.zendesk.com/hc/en-us/articles/360045123314-Difference-between-RR-interval-and-NN-interval?v=1.1.1>).

- Other (`HKQuantityType`)

  - `appleWalkingSteadiness`
  - `appleWalkingSteadinessEvent` (`HKCategoryTypeIdentifier`)

    "an incident where the user showed a
    reduced score for their gait's steadiness"

  - `electrodermalActivity`
  - `insulinDelivery`
  - `numberOfAlcoholicBeverages`
  - `numberOfTimesFallen`
  - `peripheralPerfusionIndex`
  - `sixMinuteWalkTestDistance`
  - `stairAscentSpeed`
  - `stairDescentSpeed`
  - `uvExposure`
  - `walkingAsymmetryPercentage`
  - `walkingDoubleSupportPercentage`

## Steps to Use

1. Create a new iOS App project in Xcode.
1. Click the top entry in the Navigator.
1. Select TARGETS ... {app-name} ... Info.
1. Hover over one the entries and click the "+" button to add one.
1. Add the key "Privacy - Health Share Usage Description"
1. Enter a value like "This app needs to access your health data."
1. Hover over one the entries and click the "+" button to add another.
1. Add the key "Privacy - Health Update Usage Description"
1. Enter a value like "This app needs to update your health data."
1. Click the target under "TARGETS" which has the same name as the app.
1. Click the "Signing & Capabilities" tab.
1. Click "+ Capability".
1. Type "h" and double-click "HealthKit".

HealthKit cannot be used in the Simulator,
so the app must be run on a real device.

## Background Delivery

To enable background delivery of HealthKit events:

1. Navigate to the "Signing & Capabilities" tab for the Target.
1. Add a provisioning profile (see separate post)
1. In the HealthKit section, check the "Background Delivery" checkbox

## Permissions

The first time a user runs an app that uses HealthKit
it will prompt for permission to access health data.
Separate toggle switches are displayed
for each kind of data to be written
and each kind of data to be read.
For example, a user can grant access to read their weight (a.k.a. bodyMass),
but deny permission to write their weight.

If the user denies permission to access a particular kind of data
and the app is run again later,
it will not prompt the user for permission again.

To grant permission later:

1. Launch the Health app.
1. Tap the "Sharing" button at the bottom.
1. Tap "Apps".
1. Tap the name of an app that wants permissions.
1. Enable/disable specific permissions or tap "Turn on all".

An error is thrown if an app attempts to write data
for which the user has not granted permission.

Apps do not crash or throw an error if they attempt to read data
for which the user has not granted permission.
If the query is for a single value, ? is returned.
If the query is for a sequence of data, an empty Array is returned.

## Class Hierarchy

- [HKHealthStore](<https://developer.apple.com/documentation/healthkit/hkhealthstore?v=1.1.1>)

  "The access point for all data managed by HealthKit."

- [HKObject](<https://developer.apple.com/documentation/healthkit/hkobject?v=1.1.1>)

  "A piece of data that can be stored inside the HealthKit store."

  This stores a UUID for the value (`uuid`),
  the device that generated the data (`device`),
  the app that created the object (`sourceRevision`),
  and metadata in a map with `String` keys (`metadata`).

  - [HKSample](<https://developer.apple.com/documentation/healthkit/hksample?v=1.1.1>)

    "A HealthKit sample represents a piece of data
    associated with a start and end time."

    - [HKCategorySample](<https://developer.apple.com/documentation/healthkit/hkcategorysample?v=1.1.1>)

      "A sample with values from a short list of possible values."

    - [HKCorrelation](<https://developer.apple.com/documentation/healthkit/hkcorrelation?v=1.1.1>)

      "A sample that groups multiple related samples into a single entry."

    - [HKQuantitySample](<https://developer.apple.com/documentation/healthkit/hkquantitysample?v=1.1.1>)

      "A sample that represents a quantity, including the value and the units."

      - [HKCummulativeQuantitySample](<https://developer.apple.com/documentation/healthkit/hkcumulativequantitysample?v=1.1.1>)

        "A sample that represents a cumulative quantity."

      - [HKDiscreteQuantitySample](<https://developer.apple.com/documentation/healthkit/hkdiscretequantitysample?v=1.1.1>)

        "A sample that represents a discrete quantity."

    - [HKWorkout](<https://developer.apple.com/documentation/healthkit/hkworkout?v=1.1.1>)

      "A workout sample that stores information about a single physical activity."

      This can include multiple values with different units such as
      meters run, flights of stairs climbed, and calories burned.

- [HKObjectType](<https://developer.apple.com/documentation/healthkit/hkobjecttype?v=1.1.1>)

  "An abstract superclass with subclasses that identify
  a specific type of data for the HealthKit store."

  - [HKActivitySummaryType](<https://developer.apple.com/documentation/healthkit/hkactivitysummarytype?v=1.1.1>)

    "A type that identifies activity summary objects."

  - [HKCharacteristicType](<https://developer.apple.com/documentation/healthkit/hkcharacteristictype?v=1.1.1>)

    "A type that represents data that doesn’t typically change over time."

    Examples include birthday and blood type.

  - [HKSampleType](<https://developer.apple.com/documentation/healthkit/hksampletype?v=1.1.1>)

    "An abstract superclass for all classes that identify a
    specific type of sample when working with the HealthKit store."

    - [HKAudiogramSampleType](<https://developer.apple.com/documentation/healthkit/hkaudiogramsampletype?v=1.1.1>)

      "An abstract superclass for all classes that identify a
      specific type of sample when working with the HealthKit store."

    - [HKCategoryType](<https://developer.apple.com/documentation/healthkit/hkcategorytype?v=1.1.1>)

      "A type that identifies samples that contain
      a value from a small set of possible values."

    - [HKClinicalType](<https://developer.apple.com/documentation/healthkit/hkclinicaltype?v=1.1.1>)

      "A type that identifies samples that contain clinical record data."

    - [HKCorrelationType](<https://developer.apple.com/documentation/healthkit/hkcorrelationtype?v=1.1.1>)

      "A type that identifies samples that group multiple subsamples."

    - [HKDocumentType](<https://developer.apple.com/documentation/healthkit/hkdocumenttype?v=1.1.1>)

      "A sample type used to create queries for documents."

    - [HKElectrocardiogramType](<https://developer.apple.com/documentation/healthkit/hkelectrocardiogramtype?v=1.1.1>)

      "A type that identifies samples containing electrocardiogram data."

    - [HKQuantityType](<https://developer.apple.com/documentation/healthkit/hkquantitytype?v=1.1.1>)

      "A type that identifies samples that store numerical values."

    - [HKSeriesType](<https://developer.apple.com/documentation/healthkit/hkseriestype?v=1.1.1>)

      "A type that indicates the data stored in a series sample."

    - [HKWorkoutType](<https://developer.apple.com/documentation/healthkit/hkworkouttype?v=1.1.1>)

      "A type that identifies samples that store information about a workout."

- [HKQuantity](<https://developer.apple.com/documentation/healthkit/hkquantity?v=1.1.1>)

  "An object that stores a value for a given unit."

  The numeric value and unit this stores cannot be directly accessed.
  Instead call the `doubleValue` method which
  returns the value after converting it to a specified unit.
  For example, the value may be stored in meters,
  but can be retrieved in miles.
  There is no need to writing code to do unit conversions.

- [HKQuery](<https://developer.apple.com/documentation/healthkit/hkquery?v=1.1.1>)

  "An abstract class for all the query classes in HealthKit."

  - [HKActivitySummaryQuery](<https://developer.apple.com/documentation/healthkit/hkactivitysummaryquery?v=1.1.1>)

    "A query for reading activity summary objects from the HealthKit store."

  - [HKAnchoredObjectQuery](<https://developer.apple.com/documentation/healthkit/hkanchoredobjectquery?v=1.1.1>)

    "A query that returns changes to the HealthKit store, including a snapshot
    of new changes and continuous monitoring as a long-running query."

  - [HKCorrelationQuery](<https://developer.apple.com/documentation/healthkit/hkcorrelationquery?v=1.1.1>)

    "A query that performs complex searches based on the correlation’s contents,
    and returns a snapshot of all matching samples."

  - [HKDocumentQuery](<https://developer.apple.com/documentation/healthkit/hkdocumentquery?v=1.1.1>)

    "A query that returns a snapshot of all matching documents
    currently saved in the HealthKit store."

  - [HKElectrocardiogramQuery](<https://developer.apple.com/documentation/healthkit/hkelectrocardiogramquery?v=1.1.1>)

    "A query that returns the underlying voltage measurements
    for an electrocardiogram sample."

  - [HKHeartbeatSeriesQuery](<https://developer.apple.com/documentation/healthkit/hkheartbeatseriesquery?v=1.1.1>)

    "A query that returns the heartbeat data
    contained in a heartbeat series sample."

  - [HKObserverQuery](<https://developer.apple.com/documentation/healthkit/hkobserverquery?v=1.1.1>)

    "A long-running query that monitors the HealthKit store and updates
    your app when the HealthKit store saves or deletes a matching sample."

  - [HKQuantitySeriesSampleQuery](<https://developer.apple.com/documentation/healthkit/hkquantityseriessamplequery?v=1.1.1>)

    "A query that accesses the series data associated with a quantity sample."

  - [HKSampleQuery](<https://developer.apple.com/documentation/healthkit/hksamplequery?v=1.1.1>)

    "A general query that returns a snapshot of all the
    matching samples currently saved in the HealthKit store."

  - [HKSourceQuery](<https://developer.apple.com/documentation/healthkit/hksourcequery?v=1.1.1>)

    "A query that returns a list of sources, such as apps and devices,
    that have saved matching queries to the HealthKit store."

  - [HKStatisticsCollectionQuery](<https://developer.apple.com/documentation/healthkit/hkstatisticscollectionquery?v=1.1.1>)

    "A query that performs multiple statistics queries
    over a series of fixed-length time intervals."

  - [HKStatisticsQuery](<https://developer.apple.com/documentation/healthkit/hkstatisticsquery?v=1.1.1>)

    "A query that performs statistical calculations over a set of
    matching quantity samples, and returns the results.
    You can use statistical queries to calculate the minimum, maximum,
    or average value of a set of discrete quantities,
    or use them to calculate the sum for cumulative quantities."

  - [HKVerifiableClinicalRecordQuery](<https://developer.apple.com/documentation/healthkit/hkverifiableclinicalrecordquery?v=1.1.1>)

    "A query for one-time access to a SMART Health Card."

  - [HKWorkoutRouteQuery](<https://developer.apple.com/documentation/healthkit/hkworkoutroutequery?v=1.1.1>)

    "A query to access the location data stored in a workout route."

- [HKQueryDescriptor](<https://developer.apple.com/documentation/healthkit/hkquerydescriptor?v=1.1.1>)

  "A descriptor that specifies a set of samples
  based on the data type and a predicate."

- [HKStatistics](<https://developer.apple.com/documentation/healthkit/hkstatistics?v=1.1.1>)

  "An object that represents the result of calculating the minimum, maximum,
  average, or sum over a set of samples from the HealthKit store."

  Properties include `startDate`, `endDate`, `quantityType`, and `sources`.
  Methods include `averageQuantity`, `duration`, `maximumQuantity`,
  `minimumQuantity`, `mostRecentQuantity`, `mostRecentQuantityDateInterval`,
  and `sumQuantity`.

- [HKStatisticsCollection](<https://developer.apple.com/documentation/healthkit/hkstatisticscollection?v=1.1.1>)

  "An object that manages a collection of statistics,
  representing the results calculated over separate time intervals."

  Duplicate data collected from multiple devices can be automatically ignored.

## Reading Data

In order to access HealthKit, add the capability
in the "Signing & Capabilities" tab of the main target.

Add the key "Privacy - Health Share Usage Description"
in the "Info" tab of the main target
with a description like "To read health data".

The following code demonstrates retrieving data from HealthKit
and display it. For the full iOS project, see this [GitHub repository](<https://github.com/mvolkmann/Workouts?v=1.1.1>).

```swift
// ContentView.swift
import SwiftUI

struct ContentView: View {
    @StateObject private var viewModel = HealthKitViewModel()

    func labelledValue(_ label: String, _ value: Double) -> some View {
        Text("\(label): \(String(format: "%.0f", value))")
    }

    var body: some View {
        VStack {
            Text("Health Statistics for Past 7 Days")
                .font(.title)
            labelledValue("Average Heart Rate", viewModel.heartRate)
            labelledValue(
                "Average Resting Heart Rate",
                viewModel.restingHeartRate
            )
            labelledValue("Total Steps", viewModel.steps)
            labelledValue("Total Calories Burned", viewModel.activeEnergyBurned)
        }
    }
}
```

```swift
// HealthKitViewModel.swift
import HealthKit

@MainActor
class HealthKitViewModel: ObservableObject {
    @Published private(set) var activeEnergyBurned: Double = 0
    @Published private(set) var heartRate: Double = 0
    @Published private(set) var restingHeartRate: Double = 0
    @Published private(set) var steps: Double = 0

    init() {
        Task {
            let healthManager = HealthManager()
            do {
                try await healthManager.authorize(identifiers: [
                    .activeEnergyBurned,
                    .heartRate,
                    .restingHeartRate,
                    .stepCount
                ])

                let endDate = Date.now
                let startDate = Calendar.current.date(
                    byAdding: DateComponents(day: -7),
                    to: endDate,
                    wrappingComponents: false
                )!

                activeEnergyBurned = try await healthManager.sum(
                    identifier: .activeEnergyBurned,
                    unit: .kilocalorie(),
                    startDate: startDate,
                    endDate: endDate
                )
                heartRate = try await healthManager.average(
                    identifier: .heartRate,
                    unit: HKUnit(from: "count/min"),
                    startDate: startDate,
                    endDate: endDate
                )
                restingHeartRate = try await healthManager.average(
                    identifier: .restingHeartRate,
                    unit: HKUnit(from: "count/min"),
                    startDate: startDate,
                    endDate: endDate
                )
                steps = try await healthManager.sum(
                    identifier: .stepCount,
                    unit: .count(),
                    startDate: startDate,
                    endDate: endDate
                )
            } catch {
                print("error getting health data: \(error)")
            }
        }
    }
}
```

```swift
// HealthManager.swift
import HealthKit

@MainActor
class HealthManager: ObservableObject {
    private let store = HKHealthStore()

    func authorize(identifiers: [HKQuantityTypeIdentifier]) async throws {
        let typeSet: Set<HKQuantityType> = Set(
            identifiers.map { .quantityType(forIdentifier: $0)! }
        )
        try await store.requestAuthorization(toShare: [], read: typeSet)
    }

    func average(
        identifier: HKQuantityTypeIdentifier,
        unit: HKUnit,
        startDate: Date,
        endDate: Date
    ) async throws -> Double {
        try await withCheckedThrowingContinuation { completion in
            let quantityType = HKQuantityType.quantityType(
                forIdentifier: identifier
            )!
            let predicate: NSPredicate? = HKQuery.predicateForSamples(
                withStart: startDate,
                end: endDate
            )
            let query = HKStatisticsQuery(
                quantityType: quantityType,
                quantitySamplePredicate: predicate,
                options: .discreteAverage
            ) { (_: HKStatisticsQuery, result: HKStatistics?, error: Error?) in
                if let error {
                    completion.resume(throwing: error)
                } else {
                    let quantity: HKQuantity? = result?.averageQuantity()
                    let result = quantity?.doubleValue(for: unit)
                    completion.resume(returning: result ?? 0)
                }
            }
            store.execute(query)
        }
    }

    func sum(
        identifier: HKQuantityTypeIdentifier,
        unit: HKUnit,
        startDate: Date,
        endDate: Date
    ) async throws -> Double {
        try await withCheckedThrowingContinuation { completion in
            let quantityType = HKQuantityType.quantityType(
                forIdentifier: identifier
            )!
            let predicate: NSPredicate? = HKQuery.predicateForSamples(
                withStart: startDate,
                end: endDate
            )
            let query = HKStatisticsQuery(
                quantityType: quantityType,
                quantitySamplePredicate: predicate,
                options: .cumulativeSum
            ) { (_: HKStatisticsQuery, result: HKStatistics?, error: Error?) in
                if let error {
                    completion.resume(throwing: error)
                } else {
                    let quantity: HKQuantity? = result?.sumQuantity()
                    let result = quantity?.doubleValue(for: unit)
                    completion.resume(returning: result ?? 0)
                }
            }
            store.execute(query)
        }
    }
}
```

## Writing Data

In order to access HealthKit, add the capability
in the "Signing & Capabilities" tab of the main target.

Add the key "Privacy - Health Update Usage Description"
in the "Info" tab of the main target
with a description like "To write workout data".

The following code demonstrates writing data to HealthKit.

```swift
// ContentView.swift
import SwiftUI

// Code for this struct appears in the previous section.
struct ContentView: View {
    ...

    @State private var caloriesBurned = "850"
    @State private var cyclingMiles = "20.0"
    @State private var endTime = Date() // adjusted in init
    @State private var startTime = Date() // adjusted in init

    init() {
        // Remove seconds from the end time.
        let calendar = Calendar.current
        let endSeconds = calendar.component(.second, from: endTime)
        let secondsCleared = calendar.date(
            byAdding: .second,
            value: -endSeconds,
            to: endTime
        )!
        _endTime = State(initialValue: secondsCleared)

        // Set start time to one hour before the end time.
        let oneHourBefore = calendar.date(
            byAdding: .hour,
            value: -1,
            to: endTime
        )!
        _startTime = State(initialValue: oneHourBefore)
    }

    // Add this method and call it when a Button is tapped.
    private func addWorkout() {
        Task {
            do {
                // HealthKit seems to round down to the nearest tenth.
                // For example, 20.39 becomes 20.3.
                // Adding 0.05 causes it to round to the nearest tenth.
                let distance = (cyclingMiles as NSString).doubleValue + 0.05
                let calories = (caloriesBurned as NSString).intValue
                try await HealthKitManager().addCyclingWorkout(
                    startTime: startTime,
                    endTime: endTime,
                    distance: distance,
                    calories: Int(calories)
                )
            } catch {
                print("Error adding workout: \(error)")
            }
        }
    }
}
```

```swift
// HealthManager.swift
import HealthKit

// Code for this class appears in the previous section.
@MainActor
class HealthManager: ObservableObject {
    ...

    // Add this method.
    func addCyclingWorkout(
        startTime: Date,
        endTime: Date,
        distance: Double,
        calories: Int
    ) async throws {
        let workout = HKWorkout(
            activityType: HKWorkoutActivityType.cycling,
            start: startTime,
            end: endTime,
            duration: 0, // compute from start and end data
            totalEnergyBurned: HKQuantity(
                unit: .kilocalorie(),
                doubleValue: Double(calories)
            ),
            totalDistance: HKQuantity(unit: .mile(), doubleValue: distance),
            metadata: nil
        )
        try await store.save(workout)
    }
}


```
