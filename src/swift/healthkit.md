---
eleventyNavigation:
  key: HealthKit
  parent: Swift
  order: 4
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://developer.apple.com/documentation/healthkit",
"HealthKit" %} is a library from Apple to
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

The Flutter pub.dev package {% aTargetBlank
"https://pub.dev/packages/health", "health" %}
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

    See {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkheartbeatseriesquery",
    "HKHeartbeatSeriesQuery" %}.

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

    See {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkelectrocardiogramquery",
    "HKElectrocardiogramQuery" %}.

  - `forcedVitalCapacity`

    This is the standard deviation of {% aTargetBlank
    "https://hexoskin.zendesk.com/hc/en-us/articles/360045123314-Difference-between-RR-interval-and-NN-interval",
    "NN intervals" %}.

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
for which the usser has not granted permission.

Apps do not crash or throw an error if they attempt to read data
for which the user has not granted permission.
If the query is for a single value, ? is returned.
If the query is for a sequence of data, an empty Array is returned.

## Class Hierarchy

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkhealthstore",
  "HKHealthStore" %}

  "The access point for all data managed by HealthKit."

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkobject",
  "HKObject" %}

  "A piece of data that can be stored inside the HealthKit store."

  This stores a UUID for the value (`uuid`),
  the device that generated the data (`device`),
  the app that created the object (`sourceRevision`),
  and metadata in a map with `String` keys (`metadata`).

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hksample",
    "HKSample" %}

    "A HealthKit sample represents a piece of data
    associated with a start and end time."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkcategorysample",
      "HKCategorySample" %}

      "A sample with values from a short list of possible values."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkcorrelation",
      "HKCorrelation" %}

      "A sample that groups multiple related samples into a single entry."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkquantitysample",
      "HKQuantitySample" %}

      "A sample that represents a quantity, including the value and the units."

      - {% aTargetBlank
        "https://developer.apple.com/documentation/healthkit/hkcumulativequantitysample",
        "HKCummulativeQuantitySample" %}

        "A sample that represents a cumulative quantity."

      - {% aTargetBlank
        "https://developer.apple.com/documentation/healthkit/hkdiscretequantitysample",
        "HKDiscreteQuantitySample" %}

        "A sample that represents a discrete quantity."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkworkout",
      "HKWorkout" %}

      "A workout sample that stores information about a single physical activity."

      This can include multiple values with different units such as
      meters run, flights of stairs climbed, and calories burned.

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkobjecttype",
  "HKObjectType" %}

  "An abstract superclass with subclasses that identify
  a specific type of data for the HealthKit store."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkactivitysummarytype",
    "HKActivitySummaryType" %}

    "A type that identifies activity summary objects."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkcharacteristictype",
    "HKCharacteristicType" %}

    "A type that represents data that doesn’t typically change over time."

    Examples include birthday and blood type.

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hksampletype",
    "HKSampleType" %}

    "An abstract superclass for all classes that identify a
    specific type of sample when working with the HealthKit store."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkaudiogramsampletype",
      "HKAudiogramSampleType" %}

      "An abstract superclass for all classes that identify a
      specific type of sample when working with the HealthKit store."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkcategorytype",
      "HKCategoryType" %}

      "A type that identifies samples that contain
      a value from a small set of possible values."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkclinicaltype",
      "HKClinicalType" %}

      "A type that identifies samples that contain clinical record data."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkcorrelationtype",
      "HKCorrelationType" %}

      "A type that identifies samples that group multiple subsamples."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkdocumenttype",
      "HKDocumentType" %}

      "A sample type used to create queries for documents."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkelectrocardiogramtype",
      "HKElectrocardiogramType" %}

      "A type that identifies samples containing electrocardiogram data."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkquantitytype",
      "HKQuantityType" %}

      "A type that identifies samples that store numerical values."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkseriestype",
      "HKSeriesType" %}

      "A type that indicates the data stored in a series sample."

    - {% aTargetBlank
      "https://developer.apple.com/documentation/healthkit/hkworkouttype",
      "HKWorkoutType" %}

      "A type that identifies samples that store information about a workout."

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkquantity",
  "HKQuantity" %}

  "An object that stores a value for a given unit."

  The numeric value and unit this stores cannot be directly accessed.
  Instead call the `doubleValue` method which
  returns the value after converting it to a specified unit.
  For example, the value may be stored in meters,
  but can be retrieved in miles.
  There is no need to writing code to do unit conversions.

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkquery",
  "HKQuery" %}

  "An abstract class for all the query classes in HealthKit."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkactivitysummaryquery",
    "HKActivitySummaryQuery" %}

    "A query for reading activity summary objects from the HealthKit store."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkanchoredobjectquery",
    "HKAnchoredObjectQuery" %}

    "A query that returns changes to the HealthKit store, including a snapshot
    of new changes and continuous monitoring as a long-running query."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkcorrelationquery",
    "HKCorrelationQuery" %}

    "A query that performs complex searches based on the correlation’s contents,
    and returns a snapshot of all matching samples."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkdocumentquery",
    "HKDocumentQuery" %}

    "A query that returns a snapshot of all matching documents
    currently saved in the HealthKit store."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkelectrocardiogramquery",
    "HKElectrocardiogramQuery" %}

    "A query that returns the underlying voltage measurements
    for an electrocardiogram sample."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkheartbeatseriesquery",
    "HKHeartbeatSeriesQuery" %}

    "A query that returns the heartbeat data
    contained in a heartbeat series sample."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkobserverquery",
    "HKObserverQuery" %}

    "A long-running query that monitors the HealthKit store and updates
    your app when the HealthKit store saves or deletes a matching sample."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkquantityseriessamplequery",
    "HKQuantitySeriesSampleQuery" %}

    "A query that accesses the series data associated with a quantity sample."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hksamplequery",
    "HKSampleQuery" %}

    "A general query that returns a snapshot of all the
    matching samples currently saved in the HealthKit store."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hksourcequery",
    "HKSourceQuery" %}

    "A query that returns a list of sources, such as apps and devices,
    that have saved matching queries to the HealthKit store."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkstatisticscollectionquery",
    "HKStatisticsCollectionQuery" %}

    "A query that performs multiple statistics queries
    over a series of fixed-length time intervals."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkstatisticsquery",
    "HKStatisticsQuery" %}

    "A query that performs statistical calculations over a set of
    matching quantity samples, and returns the results.
    You can use statistical queries to calculate the minimum, maximum,
    or average value of a set of discrete quantities,
    or use them to calculate the sum for cumulative quantities."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkverifiableclinicalrecordquery",
    "HKVerifiableClinicalRecordQuery" %}

    "A query for one-time access to a SMART Health Card."

  - {% aTargetBlank
    "https://developer.apple.com/documentation/healthkit/hkworkoutroutequery",
    "HKWorkoutRouteQuery" %}

    "A query to access the location data stored in a workout route."

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkquerydescriptor?changes=_5",
  "HKQueryDescriptor" %}

  "A descriptor that specifies a set of samples
  based on the data type and a predicate."

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkstatistics",
  "HKStatistics" %}

  "An object that represents the result of calculating the minimum, maximum,
  average, or sum over a set of samples from the HealthKit store."

  Properties include `startDate`, `endDate`, `quantityType`, and `sources`.
  Methods include `averageQuantity`, `duration`, `maximumQuantity`,
  `minimumQuantity`, `mostRecentQuantity`, `mostRecentQuantityDateInterval`,
  and `sumQuantity`.

- {% aTargetBlank
  "https://developer.apple.com/documentation/healthkit/hkstatisticscollection",
  "HKStatisticsCollection" %}

  "An object that manages a collection of statistics,
  representing the results calculated over separate time intervals."

  Duplicate data collected from multiple devices can be automatically ignored.

## Reading Data

TODO: Add examples from the HealthKitDemo app.

## Writing Data

TODO: Add examples from the HealthKitDemo app.
