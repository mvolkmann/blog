---
eleventyNavigation:
  key: Feather Weather
layout: topic-layout.njk
---

## Overview

Feather Weather is an attractive, free iOS application for
viewing weather forecasts obtained from the Apple WeatherKit API.
Download it from the iOS App Store.

Tap the buttons at the bottom of the screen to switch between
the four weather forecast views described below.

The app currently supports three languages: English, French, and Spanish.

## Contact Information

To contact the developer of this application, send email to
<a href="mailto:r.mark.volkmann@gmail.com">R. Mark Volkmann</a>.

## Current Screen

The "Current" screen displays the current weather conditions
for the selected location which defaults to your current location.

While the weather forecast is being retrieved,
the following animated progress indicator is displayed.

<img alt="Feather Weather Progress" style="width: 40%"
    src="/blog/assets/FeatherWeather-Progress.png?v={{pkg.version}}"
    title="Feather Weather Progress">

After the weather forecast has been retrieved,
the current conditions are displayed.
The switch under the city name can be toggled between
displaying temperatures in Celsius and Fahrenheit.

<img alt="Feather Weather Current screen" style="width: 40%"
    src="/blog/assets/FeatherWeather-Current.png?v={{pkg.version}}"
    title="Feather Weather Current screen">

To see the weather forecast for a city other than your current city,
enter a city name in the "New Location" text field.

To save the current city in your list of favorite cities for each recall,
tap the hollow red heart after the city name.
This changes it to a solid red heart and
adds the city to the list below the text field.
Tap a city name in the list to see its weather forecast.

## Forecast Screen

The forecast screen displays the hourly weather forecast
over the next five days.
Drag your finger down and up on the list to
move through the hours forward and backward.

<img alt="Feather Weather Forecast screen" style="width: 40%"
    src="/blog/assets/FeatherWeather-Forecast.png?v={{pkg.version}}"
    title="Feather Weather Forecast screen">

## Chart Screen

The "Chart" screen displays an area chart that shows the
hourly forecasted temperatures over the next five days.
This enables seeing a glance how the temperature is forecasted to change.
Drag your finger right and left over the chart to see an annotation
that indicates the day, hour, and temperature under your finger.

<img alt="Feather Weather Chart screen" style="width: 40%"
    src="/blog/assets/FeatherWeather-Chart.png?v={{pkg.version}}"
    title="Feather Weather Chart screen">

## Heat Map Screen

The "Heat Map" screen displays a heat map that
This enables seeing a glance when it will be the warmest and coldest
over the next five days.
Drag your finger right and left over the heat map to scroll
forward and backward over the hours in the next five days.

<img alt="Feather Weather Heat Map screen" style="width: 40%"
    src="/blog/assets/FeatherWeather-HeatMap.png?v={{pkg.version}}"
    title="Feather Weather Heat Map screen">
