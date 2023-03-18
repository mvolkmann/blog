---
eleventyNavigation:
  key: Animated GIFs
layout: topic-layout.njk
---

## Overview

Video files in many formats include `.mov` can be converted to animated GIFs.

## Using cloudconvert

One way to create an animated GIF from a `.mov` file is to use the website
{% aTargetBlank "https://cloudconvert.com/mov-to-gif",
"MOV to GIF Converter" %}.

1. Click the "Select File" button.
1. Select a `.mov` file.
1. Click the wrench button.
1. In the "Fps" text field enter "24".
1. Scroll to the bottom.
1. Click the "Okay" button.
1. Click the "Convert" button.
1. Click the "Download" button.

## Using FFmpeg

Another way to create an animated GIF from a `.mov` file is to use
{% aTargetBlank "https://ffmpeg.org", "FFmpeg" %}.

To install this in macOS using Homebrew, enter `brew install ffmpeg`.
This takes around 10 minutes to complete.

To convert a `.mov` file to a GIF, enter the following command:

```bash
ffmpeg -i sample.mov -r 24 sample.gif
```

## Screen Recording

To create a `.mov` file from a screen recording in macOS:

- Press cmd-shift-5.
- Move and size the rectangle the defines the
  portion of the screen that will be recorded.
- Click the "Record" button.
- Wait a few seconds before beginning the actions to be recorded.
- When finished, click the stop button in the menu bar
  which looks like a circle with a square inside.
- Open the Finder and navigation to the Desktop directory.
- Look for files whose names begin with "Screen Recording" and end with ".mov".
