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

## Security

See {% aTargetBlank "https://www.cursor.com/security", "Security" %}
for details on how Cursor keeps your
source code and development environment secure.

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

## Resources

- {% aTargetBlank "https://www.youtube.com/watch?v=ocMOZpuAMw4", "Cursor Tutorial for Beginners" %}
-
