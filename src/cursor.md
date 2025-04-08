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

{% aTargetBlank "https://www.cursor.com.", "Cursor" %} is an
"AI Code Editor" that aims to make developers "extraordinarily productive".
It is a fork of Microsoft VS Code.

While Cursor offers a free tier, I frequently saw the following message
when asking it to explain code, modify code, or generate new code:
"High Load - We're experiencing high demand for Claude 3.5 Sonnet right now.
Please upgrade to Pro, switch to the 'default' model, Claude 3.7 sonnet,
another model, or try again in a few moments."
This makes the free tier somewhat unusable.

The paid tiers start at $20 per month.

Cursor works best when given a somewhat detailed plan of what it should build.
This includes identifying the frameworks and libraries to use.

When Cursor is asked to implement code for a specific task,
it provides a detailed explanation of the code it generates and
gives the developer an opportunity to review the code before accepting it.
It can be very tempting to accept the code without reviewing it,
especially if the app is running and live reload indicates
that the desired result is achieved.
Developers need to resist the urge to skip reviewing the code.

Every piece of text in the Cursor chat pane is a clickable link
that leads to the original or modified code, or related documentation.

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

TODO: Finish documenting what each subprocessor above is used for.

TODO: Explain what enabling privacy mode does.

TODO: Add more detail from https://www.cursor.com/security.

TODO: Can Cursor be configured to never upload any of our source code?

TODO: Can Cursor be configured to not use our source code to train its models?

## Functionality

Cursor provides the following functionality:

- Predicts your next edit which can be accepted by pressing the tab key.
- Answers questions about the codebase of your current project.
- Enables writing code with natural language
  instead of writing programming language syntax.

To test some of the functionality, I opened a source file containing
`const cuisines = [ "American", "Chinese", ... ]`.
After that code I inserted "for (" and it suggested
"for (const cuisine of cuisines) {".

I selected the code that creates the `cuisines` array.
Then I clicked "Add to Chat" and entered "Add more cuisines."
It successfully added many more.

Then I selected code again, clicked "Edit", and
entered "sort from shortest to longest".
It responded with "High Load - We're experiencing high demand for
Claude 3.5 Sonnet right now. Please upgrade to Pro, switch to the
'default' model, Claude 3.7 sonnet, another model,
or try again in a few moments.

I selected a function definition that was inside a `useEffect`,
clicked "Add to Chat", and entered "Move this code
to its own function defined outside the useEffect."
It did this correctly, but did not wrap the new function in `useCallback`.

I selected the function name, clicked "Edit", and
entered "Wrap this function in a useCallback."
I had to press the tab key several times to get it to make all
the necessary changes, but it successfully made correct changes.

I selected the function name, clicked "Edit", and
entered "rename this to {new-name}".
After accepting the change I had to press tab once for
each call to update the name of the function being called.

After the array of cuisine names, I entered "const longestCuisine ="
and it suggested following code which is perfect:

```js
const longestCuisine = cuisines.reduce((longest, current) => {
  return current.length > longest.length ? current : longest;
}, cuisines[0]);
```

I selected the following CSS code:

```css
@media (prefers-reduced-motion: no-preference) {
  .App-logo {
    animation: App-logo-spin infinite 20s linear;
  }
}
```

Then I clicked "Add to Chat" and entered "What does this code do?".
I received the following detailed response:

```text
This code is a CSS media query that controls animation behavior based on
user preferences. Specifically: @media (prefers-reduced-motion: no-preference)
targets users who haven't set any preference for reduced motion in their
system settings.  When this condition is met, it applies an animation to
elements with the class App-logo: The animation is named App-logo-spin
It runs infinitely (keeps repeating)
Takes 20s (20 seconds) to complete one cycle
Uses linear timing function (constant speed throughout the animation)
This is an accessibility feature that respects user preferences for
motion sensitivity.  Users who have enabled "reduced motion" in their
system settings (like those who might get motion sickness from animations)
won't see the spinning animation,
while other users will see the full animated effect.
This is a common accessibility practice that helps make web applications
more inclusive for users who are sensitive to motion
or prefer minimal animations.
```

Supposedly Cursor can implement a UI based on a provided mockup.
I tried this, but the cursor servers were
too busy to do this using the free tier.

## Paid vs. Free Versions

TODO: What features does the paid version add?

## Advice

For best results, make detailed requests rather than general ones.
For example, requesting "Write a todo app" is too general.
Have a clear vision of what you want to build.

## Chat Attempt #1: Svelte - FAIL!

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
that is not committed to Git so I can keep it secret?"
It created a `.env` file for me and modified `Map.svelte` to use it.
I modified the `.env` file to use a Google Maps API key from a previous project.

I ran `npm install` again to install new dependencies that were added.

I ran `npm run dev` to start a local web server
and browsed localhost:3000.

## Chat Attempt #2: Svelte - FAIL!

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

- Change the code to use Google Maps instead of Mapbox.

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

## Chat Attempt #3: React - Mostly successful

I entered the following in the chat:

- Create a React application that renders a drop-down for selecting
  a cuisine like Chinese or Mexican and use it in the main page.

  After this I started the server with `yarn start`.

- Alphabetize the cuisines in the drop-down.

- When a cuisine is selected, display a map that shows
  all the restaurants of that type that are
  within 10 miles of the current location of the user.

- Change the code to use Google Maps instead of Mapbox.

- Get the Google Maps API key from a .env file.

  After this I modified the `.env` file contain my Google Maps API key
  and restarted the server with `yarn start`.

- In the DevTools Console I see
  "As of February 21st, 2024, google.maps.Marker is deprecated.
  Please use google.maps.marker.AdvancedMarkerElement instead." Fix this.

- Now I get "Performance warning! LoadScript has been reloaded unintentionally!
  You should not pass `libraries` prop as new array. Please keep an array of
  libraries as static class property for Components and PureComponents,
  or just a const variable outside of component,
  or somewhere in config files or ENV variables" Fix this.

- Now I get "The map is initialized without a valid Map ID, which will prevent use of Advanced Markers." Fix this.

  After this I modified the `.env` file contain my Google Maps map ID
  and restarted the server with `yarn start`.

- Now I get an error on line 199 of RestaurantMap.tsx that says
  "Property 'mapId' does not exist on type 'IntrinsicAttributes &
  IntrinsicClassAttributes<GoogleMap> & Readonly<GoogleMapProps>'." Fix this.

- Now I get the error "This API key is not authorized to use this
  service or API. Places API error: ApiTargetBlockedMapError".
  How can I fix this?

  After this I followed the instructions it output.
  I also updated my Google Cloud payment method to use a new credit card.
  Then I restarted restarted the server with `yarn start` and tried again.
  It still says "This API key is not authorized to use this service or API."

  It turns out that I needed to enable "Places API" under "Credentials"
  for the GCP project I'm using.

- Now I get the error "<gmp-advanced-marker>:
  Please use addEventListener('gmp-click', ...)
  instead of addEventListener('click', ...).
  Error Component Stack". Fix this.

- Add .env to the .gitignore file.

- Change the map markers for each restaurant to be
  circles that contain a rating for the restaurant.

  This works! Where is it getting the ratings?

- When a map marker is clicked, show the best route on the map
  and remove any previously displayed route.

- Your solution is not working. When I click a marker,
  I don't see any route lines on the map.

  Cursor said I might need to enable the Directions API in GCP. When I
  searched for that I discovered that it was replaced with the Routes API.
  I enabled that, but it did not fix the problem.
  I'm giving up on this feature for now.

- Change the cursor to a wait cursor
  while the map is being generated or updated.

  This claimed to add changes for the cursor AND
  add a semi-transparent white overlay that says "Loading restaurants...".
  The cursor does not change and the overlay doesn't display any text.

- I don't see any text in the semi-transparent white overlay.

- Clicking a restaurant marker does nothing.
  I don't see a popup containing the restaurant name and address,
  and no route lines are drawn on the map. Fix these issues.

  Cursor modified the code and said "If you're still not seeing the popup
  or route lines, please check that you have the correct Google Maps API key
  with the following APIs enabled:
  Maps JavaScript API, Places API, and Directions API.

- Where are you getting the restaurant ratings?

  The answer was "The restaurant ratings are coming from the Google Places API.
  Specifically, in the getDetails request, we're requesting
  the "rating" field from the Places API." along with much more detail.

- I'm seeing two ESLint errors in RestaurantMaps.tsx about no-unused vars.
  Fix those.

  It fixed them by commenting out code, not deleting it.

- Delete all code that is commented out.

  This took a couple of minutes to complete.

- I'm seeing an ESLint warning react-hooks/exhaustive-deps in RestaurantMap.tsx.
  Fix it.

  It did not fix the error.

- Your changes did not fix the error. Add a dependency on markers
  to the useEffect in RestaurantMap.tsx where it is needed.

- ESLint says "The createMarkerContent function makes the dependencies
  of useEffect Hook (at line 247) change on every render.
  Move it inside the useEffect callback." Fix this.

  Finally I'm getting a clean startup of the app!

- The app is broken now. I'm seeing this in the DevTools Console:
  Maximum update depth exceeded. This can happen when a component
  calls setState inside useEffect, but useEffect either
  doesn't have a dependency array, or
  one of the dependencies changes on every render.

- When the user hovers over a restaurant map marker,
  display the restaurant name, phone number, address, and distance from me
  in a popup.

  This did not work.

- Nothing happens when I hover over a restaurant map marker.

  This mostly fixed the problem.

- The popups that appear when I hover over a restaurant map marker
  are not showing the restaurant name.
  Maybe it is missing or maybe the font color is white. Fix this.

- The distance from me to a restaurant is being computed as the crow flies.
  Change this to show the shortest possible driving distance.

  Now I see this in the DevTools Console:
  Distance Matrix Service: This API project is not authorized to use this API.
  For more information on authentication and Google Maps JavaScript API services
  please see: https://developers.google.com/maps/documentation/javascript/get-api-key

  Documentation in the GCP console says "Routes API is the next generation
  version (v2) of the Directions and Distance Matrix APIs."
  I already have the Routes API enabled.

- There are a bunch of ESLint errors again. Fix them.

- You did not fix the ESLint errors. There are two react-hooks/exhaustive-deps
  errors in RestaurantMap.tsx. Can you fix them or do I need to do it?

  This failed again. It turns out that the `markers` and `setMarkers`
  variables set from a `useState` weren't even needed.
  I manually deleted all references to those and the ESLint errors went away.

I'm getting the error "Distance Matrix Service: This API project is not
authorized to use this API." The Distance Matrix Service has been replaced
by "Routes API" which I do have enabled.

- Change the code to use the Google Maps Routes API
  instead of the Distance Matrix Service.

- When I click a map marker, no popup is appearing. Fix that.

- Your changes did not fix the issue.
  I still don't see a popup when I click a map marker.

  It fixed the issue this time AND
  I now see route lines when a marker is clicked!
  But the ESLint errors related to `markers` are back.
  I manually deleted that unused code.

In the DevTools Console I see the warning "<gmp-advanced-marker>:
Please use addEventListener('gmp-click', ...)
instead of addEventListener('click', ...)".
But when I make that change, I no longer see a popup
when I click a map marker. So I'm ignoring this warning.

I'm calling the app good now!

- Generate unit tests.

  This added the following line in RestaurantMap.tsx,
  but `loadavg` is not used so I deleted the line:

  ```js
  import {loadavg} from 'os';
  ```

  I ran `npm test` to verify that the new tests pass. They do not.
  The first issue is that `src/App.test.tsx` isn't useful or correct.
  I deleted that file.
  The second issue is that `src/components/__tests__/RestaurantMap.test.tsx`
  gives a "Geolocation error".

- When I run the tests I get a Geolocation error in RestaurantMap.test.tsx. Fix this.

  There are SO MANY errors in the tests now.
  Honestly I'm tired of fighting with Cursor.
  The app works fine, but the tests do not.

## Nim Game

- Create a web app using Svelte that implements a Nim game with
  three rows containing 3, 5, and 7 pink tabs on a black background.
  Allow the user to choose whether they go first or the computer goes first.
  Then play out the game until there is a winner.

- Change the game so at each turn the player can
  take any number of tabs from a single row.

- Change the game so the player that takes the last tab loses.

  It claimed to make this change, but is still wrong.
  And the computer does use good logic in its moves.

Overall Cursor did a decent job at generating this app.

## Summary

Cursor is a very useful tool and I recommend allowing all developers
to have access to it. However:

- Developers must still be able to distinguish between good and bad code.
  Junior developers are unlikely to achieve good results.
- The tool works best when it is sent detailed requests.
  For example, instead of asking "Write a todo application", ask
  "Write a todo web application using the React framework
  and use Tailwind for styling."
- There is a strong temptation to just accept all the generated code
  and not review it. This temptation must be resisted.
- Often when asking the tool to add a feature or fix a problem,
  it introduces new problems.
- Using the tool to build a non-trivial application
  is generally not possible in a single request.
  Instead, a dialog with the tool that is composed of many requests is needed.

## Resources

- {% aTargetBlank "https://www.youtube.com/watch?v=ocMOZpuAMw4",
  "Cursor Tutorial for Beginners" %}
