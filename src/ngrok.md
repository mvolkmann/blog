---
eleventyNavigation:
  key: ngrok
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://ngrok.com", "ngrok" %} is an HTTP tunneling tool.
It can be used to expose a locally running server to the outside world
using HTTP or HTTPS.

ngrok has Free, Pro, and Enterprise tiers.
Basic usage is free.

## Steps to Use

1. Sign up for an account at {% aTargetBlank "https://ngrok.com", "ngrok" %}.
1. Login at the same URL.
1. Download an OS-specific version of the `ngrok` command.
1. Copy your auth token from the "Setup & Installation" page.
1. Authenticate with ngrok by entering the command
   `ngrok authtoken {your-auth-token}`
1. Start any local server that listens on a given localhost port.
1. Create a tunnel by entering the command `ngrok http {local-port-number}`
1. In the output of this command, copy the URL after "Forwarding".
   Note that there are two to choose from: one for `http` and one for `https`.
1. Use the copied URL from any device to send HTTP requests to the local server.

A limitation of the Free tier is that the forwarding URLs are ephemeral,
meaning they change each time the `ngrok` command creates a new tunnel.
Code that uses an ngrok URL must be modified to use the new value.
The Pro and Enterprise versions can use a custom domain,
removing this burden.
