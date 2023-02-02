---
eleventyNavigation:
  key: Feather Weather Forecasts
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

## Overview

Feather Weather Forecasts is an attractive, free application for
viewing weather forecasts obtained from the Apple WeatherKit API.
It runs on iPhones, iPads, and Mac computers.
Download it from the iOS or Mac {% aTargetBlank
"https://apps.apple.com/us/app/feather-weather-forecasts/id1667050253",
"App Store" %}.

Tap the buttons at the bottom of the screen to switch between
the four weather forecast views described below.

The app currently supports three languages: English, French, and Spanish.

## Contact Information

To contact the developer of this application, send email to
<a href="mailto:r.mark.volkmann@gmail.com">R. Mark Volkmann</a>.
Suggestions for new features are welcome!

## Current Screen

The "Current" screen displays the current weather conditions
for the selected location which defaults to your current location.

While the weather forecast is being retrieved,
the following animated progress indicator is displayed.

<img alt="Feather Weather Progress" style="width: 40%"
    src="/blog/assets/FeatherWeather-iPhone-Progress.png?v={{pkg.version}}"
    title="Feather Weather Progress">

After the weather forecast has been retrieved,
the current conditions are displayed.
The switch under the city name can be toggled between
displaying temperatures in Celsius and Fahrenheit.

<img alt="Feather Weather Current screen" style="width: 40%"
    src="/blog/assets/FeatherWeather-iPhone-Current.png?v={{pkg.version}}"
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
    src="/blog/assets/FeatherWeather-iPhone-Forecast.png?v={{pkg.version}}"
    title="Feather Weather Forecast screen">

## Chart Screen

The "Chart" screen displays an area chart that shows the
hourly forecasted temperatures over the next five days.
This enables seeing a glance how the temperature is forecasted to change.
Drag your finger right and left over the chart to see an annotation
that indicates the day, hour, and temperature under your finger.

<img alt="Feather Weather Chart screen" style="width: 40%"
    src="/blog/assets/FeatherWeather-iPhone-Chart.png?v={{pkg.version}}"
    title="Feather Weather Chart screen">

## Heat Map Screen

The "Heat Map" screen displays a heat map that enables seeing at a glance
when it will be the warmest and coldest over the next five days.
The top row provides temperature forecasts for today
from midnight this morning to midnight tonight.
The subsequent rows provide temperature forecasts for the next four days.

Drag your finger right and left over the heat map to scroll
forward and backward through the hours in the next five days.

<img alt="Feather Weather Heat Map screen" style="width: 40%"
    src="/blog/assets/FeatherWeather-iPhone-HeatMap.png?v={{pkg.version}}"
    title="Feather Weather Heat Map screen">

The "Colors" switch can be set to "Relative" or "Absolute".

In **Relative** mode
the color blue represents the lowest temperature over the next five days and
the color red represents the highest temperature over the next next five days.
All temperatures between those extremes are
assigned a color of the rainbow between blue and red that represents
the percentage that temperature is between the low and high.

In **Absolute** mode
the color blue represents zero degrees Fahrenheit or less and
the color red represents 100 degrees Fahrenheit or more.
All temperatures between those extremes are
assigned a color of the rainbow between blue and red that represents
the percentage that temperature is between the zero and 100.

## All Screens

All screens display a refresh button in the upper-left corner
and a settings button in the upper-right corner.

The refresh button fetches updated weather data for the currently selected location.

The settings button opens the following sheet at the bottom of the screen.

<img alt="Feather Weather Settings" style="width: 40%"
    src="/blog/assets/FeatherWeather-iPhone-Settings.png?v={{pkg.version}}"
    title="Feather Weather Settings">

The first setting chooses between
reporting temperatures in Celsius and Fahrenheit.
This defaults to Celsius for all users except those in the United States.

The second setting chooses between
displaying actual temperatures and "feels like" temperatures.
The current setting is indicated at the top of every screen
with the text "showing {kind} temperatures".
This defaults to showing actual temperatures.

The third setting chooses between using relative colors and absolute colors
to describe temperatures on the Forecast and Heat Map screens.
In absolute mode, blue represents zero degrees Fahrenheit or less
and red represents 100 degrees Fahrenheit more.
In relative mode, blue represents the lowest temperature over the next five days
and red represents the highest temperature over the next five days.";
All temperatures between those extremes will be assigned a color between
blue and red in the rainbow.
This defaults to showing relative colors.

When finished viewing and modifying the settings,
swipe the sheet down to close it.
