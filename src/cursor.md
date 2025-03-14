---
eleventyNavigation:
  key: Cursor
layout: topic-layout.njk
---

<style>
    img {
        border: 1px solid gray;
    }
</style>

## Overview

{% aTargetBlank "https://www.cursor.com.", "Cursor" %} is anu
"AI Code Editor" that aims to make developers "extraordinarily productive".

## Installing

Click a download button on the
{% aTargetBlank "https://www.cursor.com.", "Cursor home page" %}.
Double-click the downloaded installer and follow the directions.

## Starting

Double-click the app to start it.
On first launch it will prompt for:

- Keyboard

  The options are Default (VS Code), Vim, Jetbrains, Emacs, Sublime, and Atom.

- Language for AI - defaults to English

- Codebase-wide

  This allows Cursor to index the entire code base of the current project.

- Add Terminal Command

  This enables launching Cursor from a terminal by entering
  `code`, `cursor`, or both.

- VS Code Extensions

  Click "Use Extensions" to install all the extensions currently being used in VS Code,
  or click "Start from Scratch" to begin with no extensions installed.

- "Cursor.app" would like to access files in your Documents folder.

  Click "Don't Allow" or "Allow" (preferred).

- Data Preferences

  Select "Help Improve Cursor" (to share usage data)
  or "Privacy Mode" (to not share usage data).

- Click "Log In" (if you already have an account) or "Sign Up".

## Security

See {% aTargetBlank "https://www.cursor.com/security", "Security" %}
for details on how Cursor keeps your
source code and development environment secure.

Enabling "Privacy Mode" makes it so none of your code will be stored by Cursor.
This setting can be found at
Cursor ... Settings ... Cursor Settings ... General ... Privacy mode.
It seems this is enabled by default, but double-check it.

Cursor depends on several "subprocessors" where code is sent
in order to respond to queries.
So Cursor is only as secure as those subprocessors which include:

- AWS - hosts Cursor infrastructure
- Fireworks - hosts custom AI models on servers in
  the United States, Tokyo, and London.
- Open AI - provides models that are used to give AI responses
- Anthropic - provides models that are used to give AI responses
- Google Cloud Vertex API - provides Gemini models
  that are used to give AI responses
- Turbopuffer - stores obfuscated code on Google Cloud servers
  in the United States
- Exa and SerpApi - used for web searches
- MongoDB - used for analytics data when privacy mode is not enabled
- Datadog
- Databricks
- Foundary
- Voltage Park
- Slack
- Google Workspace
- Pinecone
- Amplitude
- Hashicorp
- Stripe
- Vercel
- WorkOS

TODO: Finish document what each subprocessor above is used for.

TODO: Explain what enabling privacy mode does.

TODO: Add more detail from https://www.cursor.com/security.

TODO: Can Cursor be configured to never upload any of our source code?

TODO: Can Cursor be configured to not use our source code to train its models?

## Functionality

Cursor provides the following functionality:

- Predicts your next edit which can be accepted by pressing the tab key.
- Answers questions about the codebase of your current project.
- Enables writing code with natural language
  instead of specific programming language syntax.

## Paid vs. Free Versions

TODO: What features does the paid version add?

## Advice

For best results, make detailed requests rather than general ones.
For example, requesting "Write a todo app" is too general.
Have a clear vision of what you want to build.

## Chat Attempt #1 - FAIL!

Press cmd-i (macOS) or ctrl-i (others) to open Chat mode.
Requests entered here can create and/or edit multiple source files.
For example, "Create a web application that allows users to
select a kind of food such as Chinese or Mexican from a drop-down menu.
Then display on a Google Map all the matching restaurants
within 10 miles of the current location of the user.
Use the latest version of Svelte."

This will prompt for permission to run specific commands.
If this runs for a long time, it may be prompting for more information
or there may be an error.
To see this, click "Pop out terminal".

In my case it told me "create-svelte has been deprecated"
and that "npm create svelte" has been replaced with "npx sv create".
So I entered the following in the chat:
"You tried to use "npm create svelte@latest", but create-svelte
has been deprecated. You need to use "npx sv create" instead."

I once again had to click "Pop out terminal" where I was asked
several questions related to creating a new Svelte project.
After answering them, the project was created
in my home directory in a new directory named "restaurant-finder".

Instructions for running the project were output.
These included optional commands to create a local git repository.
It did not suggest running `npm install`, but that is required.

The command to run the app failed due to an issue with optional dependencies.
I had to delete the `package-lock.json` file and the `node_modules` directory,
and then run `npm install` again which worked.

Enhance the project code with followup requests.
For example, "Instead of displaying default map markers for each restaurant,
display their Yelp rating."

Back in Cursor I clicked the "Open Project" button
and selected the new project directory.
This contained a fresh Svelte project with none of the code I requested.

I requested the following in the chat: "Modify the web application to
allow users to select a kind of food such as Chinese or Mexican
from a drop-down menu. Then display on a Google Map all the matching
restaurants within 10 miles of the current location of the user."
This ran for a couple of minutes.
Then I press cmd-return to accept all the changes.

This output the following request:
"Also, please make sure to provide your Google Maps API key by
replacing the empty string in the Map.svelte component.
The application won't work without a valid API key."

I asked it "How can I put the Google Maps API key in a .env file
that is not commited to Git so I can keep it secret?"
It created a `.env` file for me and modified `Map.svelte` to use it.
I modified the `.env` file to use a Google Maps API key from a previous project.

I ran `npm install` again to install new dependencies that were added.

I ran `npm run dev` to start a local web server
and browsed localhost:3000.

## Chat Attempt #2 - FAIL!

Creating a new SvelteKit project is somewhat complicated at the moment,
so I did that manually and then opened that project in Cursor.

I entered the following in the chat:

- Create a component that renders a drop-down for selecting
  a cuisine like Chinese or Mexican and use it in the main page.

- Alphabetize the cuisines in the drop-down.

- Prettier complains that CuisineSelector.svelte should
  include a key for each block. Fix that.

- When a cuisine is selected, display a map that shows
  all the restaurants of that type that are
  within 10 miles of the current location of the user.

- Change the code to use Google Maps instead of mapbox.

  This output the following:

  > To get this working, you'll need to:
  >
  > 1. Get a Google Maps API key:
  >    - Go to the Google Cloud Console (https://console.cloud.google.com/)
  >    - Create a new project or select an existing one
  >    - Enable the Maps JavaScript API
  >    - Create credentials (API key)
  >    - Replace YOUR_GOOGLE_MAPS_API_KEY in the main page with your actual API key

- Get the Google Maps API key from a .env file.

  This created a `.env` at the root of the project.
  I edited that file and set PUBLIC_GOOGLE_MAPS_API_KEY to my API key.

- I ran the app again and selected "Mexican" from the cuisine drop-down.
  It opened a dialog that said "http://localhost:5173 wants to know your location."
  I clicked the "Allow while visiting this site" button.
  It rendered a map with the caption "Showing Mexican restaurants near you"
  and a dialog that said "This page can't load Google Maps correctly.
  Do you own this website?"
  I clicked the "OK" button.
  In the DevTools console I saw the following messages:

  - "Google Maps JavaScript API has been loaded directly without loading=async.
    This can result in suboptimal performance."
  - "Google Maps JavaScript API warning: InvalidKey"
  - "As of February 21st, 2024, google.maps.Marker is deprecated.
    Please use google.maps.marker.AdvancedMarkerElement instead.
    At this time, google.maps.Marker is not scheduled to be discontinued,
    but google.maps.marker.AdvancedMarkerElement
    is recommended over google.maps.Marker."

- I see a message in the DevTools console that says "Google Maps
  JavaScript API has been loaded directly without loading=async.
  This can result in suboptimal performance." Please fix that.

  Cursor attempted to fix this, but the changes it made
  did not make the warning go away.

- google.maps.Marker is deprecated.
  Please use google.maps.marker.AdvancedMarkerElement instead.

  The changes removed the warning about google.maps.Marker being deprecated,
  but introduced these new issues:

  - Please use addEventListener('gmp-click', ...)
    instead of addEventListener('click', ...).
  - The map is initialized without a valid Map ID,
    which will prevent use of Advanced Markers.

- Please use addEventListener('gmp-click', ...)
  instead of addEventListener('click', ...).

  The changes removed the warning about addEventListener.

- The map is initialized without a valid Map ID,
  which will prevent use of Advanced Markers.
  Please initialize the map with a valid map id.

  This make changes and output the following:

  To get this working, you'll need to:

  1. Create a Map ID in the Google Cloud Console:
     - Go to https://console.cloud.google.com/google/maps-apis/maps
     - Click "Create Map ID"
     - Give your map a name
     - Copy the generated Map ID
  2. Update your .env file with your Map ID:
     PUBLIC_GOOGLE_MAPS_ID=your_map_id_here

  The URL given to create a map id is incorrect. The correct URL is
  https://console.cloud.google.com/google/maps-apis/studio/maps.
  I browsed that URL and did the following:

  - click "Create map ID" button
  - enter a name and description
  - selected "JavaScript" for the Map type
  - accepted the default of Raster rather than Vector
  - clicked the "Save" button
  - copied the "Map ID" that was displayed
  - modified `PUBLIC_GOOGLE_MAPS_ID` in the `.env` file to have that value

- When I restart the Svelte server, I'm getting the error
  "No loader is configured for '.node' files:
  node_modules/fsevents/fsevents.node". Please fix this.

- Now I get SyntaxError: Identifier '**vite**injectQuery'
  has already been declared in app.js. Please fix this.

- The changes you made did not fix the problem. Please try again.

- That didn't fix the problem either. Do you have another idea?

- Check my SvelteKit version and ensure it's compatible with your Vite version.

  This output the following:

  I see the issue now. You're using some very recent versions that might
  have compatibility issues. The problem is that you're using Svelte 5
  (which is still in beta) along with Vite 6 (which is also very new).
  Let's downgrade to more stable versions. I'll update your package.json.

- Now I get the error "Top-level await is not available in the
  configured target environment" Can you fix this?

- That did not fix the problem. The file node_modules/vitefu/src/index.js
  uses a top-level await.

- It's back to giving me
  "SyntaxError: Identifier '**vite**injectQuery' has already been declared".
  Can you fix that?

- Now I get SyntaxError: Invalid left-hand side in assignment in hook.js.
  Can you fix that?

- Now we are back to ERROR: Top-level await is
  not available in the configured target environment.
  It seems that your fixes are going in circles. Is it time to give up?

## Resources

- {% aTargetBlank "https://www.youtube.com/watch?v=ocMOZpuAMw4", "Cursor Tutorial for Beginners" %}
