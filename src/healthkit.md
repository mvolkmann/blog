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
1. Select TARGETS ... Runner ... Info.
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

  - step count
  - distance walking/running
  - distance cycling
  - push count while using a wheelchair
  - swimming stroke count
  - distance swimming
  - distance downhill snow sports
  - basal energy burned
  - active energy burned
  - flights climbed (stairs)
  - Nike fuel points earned
  - Apple exercise time
  - VO2 max
  - low cardio fitness event

- Body Measurements

  - height
  - body mass
  - body mass index
  - lean body mass
  - body fat percentage
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
  - sexual activity
  - pregnancy test result
  - progesterone test result

- Hearing

  - environmental audio exposure
  - headphone audio exposure
  - environmental audio exposure event
  - headphone audio exposure event

- Vital Signs

  - heart rate
  - low heart rate event
  - high heart rate event
  - irregular heart rhythm event
  - resting heart rate
  - heart rate variability SDNN (standard deviation of {% aTargetBlank
    "https://hexoskin.zendesk.com/hc/en-us/articles/360045123314-Difference-between-RR-interval-and-NN-interval",
    "NN intervals" %})
  - walking heart rate average
  - heartbeat series
  - electrocardiogram data
  - oxygen saturation
  - body temperature
  - blood pressure
  - blood pressure systolic
  - blood pressure diastolic
  - respiratory rate
