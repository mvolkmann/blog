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

## Chat Attempt #2

Open a chat. One way is to open the command palette and select "New Chat".
Another way is to press cmd-i (macOS) or ctrl-i (others).

Requests entered here can create and/or edit multiple source files.
For example, "Create a Svelte project in a directory named "restaurant-finder".
This will suggest commands to run to create the project.
For each command, click the "Run command" button
(or press cmd-return) to run it.
Click "Pop out terminal" in order to answer questions
being asked by `npx sv create`.

Ask Cursor to customize the default application.
For example, "Change the main page to contain a drop-down
from which users can select a kind of food such as Chinese or Mexican."

## Resources

- {% aTargetBlank "https://www.youtube.com/watch?v=ocMOZpuAMw4", "Cursor Tutorial for Beginners" %}
-
