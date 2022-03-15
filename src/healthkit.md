---
eleventyNavigation:
  key: HealthKit
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

## Available Data

HealthKit data types are defined {% aTargetBlank
"https://developer.apple.com/documentation/healthkit/data_types",
"HKObjectType" %} subclasses.
The subclasses include:

- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkcharacteristictype", "HKCharacteristicType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkquantitytype", "HKQuantityType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkcategorytype", "HKCategoryType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkcorrelationtype", "HKCorrelationType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkactivitysummarytype", "HKActivitySummaryType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkaudiogramsampletype", "HKAudiogramSampleType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkelectrocardiogramtype", "HKElectrocardiogramType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkseriestype", "HKSeriesType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkclinicaltype", "HKClinicialType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkworkouttype", "HKWorkoutType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hkobjecttype", "HKObjectType" %}
- {% aTargetBlank "https://developer.apple.com/documentation/healthkit/hksampletype", "HKSampleType" %}

The data available includes:

- Characteristics

  - activity move mode
  - biological sex
  - blood type
  - date of birth
  - Fitzpatrick skin type
  - wheel chair use

- Activity

  - active energy burned
  - Apple exercise time
  - Apple move time
  - Apple stand time
  - basal energy burned
  - distance cycling
  - distance downhill snow sports
  - distance swimming
  - distance walking/running
  - flights climbed (stairs)
  - low cardio fitness event
  - Nike fuel points earned
  - push count while using a wheelchair
  - step count
  - swimming stroke count
  - VO2 max

- Body Measurements

  - body fat percentage
  - body mass
  - body mass index
  - height
  - lean body mass
  - waist circumference

- Reproductive Health

  - basal body temperature
  - cervical mucus quality
  - contraceptive
  - inter-menstrual bleeding
  - lactation
  - menstrual flow
  - ovulation test result
  - pregnancy
  - pregnancy test result
  - progesterone test result
  - sexual activity

- Hearing

  - environmental audio exposure
  - environmental audio exposure event
  - headphone audio exposure
  - headphone audio exposure event

- Vital Signs

  - blood pressure
  - blood pressure diastolic
  - blood pressure systolic
  - body temperature
  - electrocardiogram data
  - heart rate
  - heart rate variability SDNN (standard deviation of {% aTargetBlank
    "https://hexoskin.zendesk.com/hc/en-us/articles/360045123314-Difference-between-RR-interval-and-NN-interval",
    "NN intervals" %})
  - heartbeat series
  - high heart rate event
  - irregular heart rhythm event
  - low heart rate event
  - oxygen saturation
  - respiratory rate
  - resting heart rate
  - walking heart rate average

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

## Data Class Hierarchy

- HKHealthStore

- HKObject

  - HKSample
    - HKCategorySample
    - HKCorrelation
    - HKQuantitySample
      - HKCummulativeQuantitySample
      - HKDiscreteQuantitySample
    - HKWorkout

- HKObjectType

  - HKActivitySummaryType
  - HKCharacteristicType
  - HKDocumentType
  - HKSampleType
    - HKAudiogramSampleType
    - HKCategoryType
    - HKClinicalType
    - HKCorrelationType
    - HKElectrocardiogramType
    - HKQuantityType
    - HKSeriesType
    - HKWorkoutType

- HKQuery

  - HKActivitySummaryQuery
  - HKAnchoredObjectQuery
  - HKCorrelationQuery
  - HKDocumentQuery
  - HKElectrocardiogramQuery
  - HKHeartbeatSeriesQuery
  - HKObserverQuery
  - HKQuantitySeriesSampleQuery
  - HKSampleQuery
  - HKSourceQuery
  - HKStatisticsCollectionQuery
  - HKStatisticsQuery
  - HKVerifiableClinicalRecordQuery
  - HKWorkoutRouteQuery

- HKQueryDescriptor
