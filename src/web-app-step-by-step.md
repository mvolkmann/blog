---
eleventyNavigation:
  key: Web App Step-by-Step
layout: topic-layout.njk
---

Many people talk about how easy it is to build web applications using Node.js.
However, it's difficult to find resources that cover all the steps.
We will do that here.
Some details will be omitted.
We want to focus on the primary steps.

This article makes specific technology/tool choices.
Obviously, the steps may differ if different choices are made.
The primary choices made here include Node.js, PostgreSQL, and React.
This article was inspired by the Frontend Masters workshop
"Zero to Production Node.js on Amazon Web Services", by Kevin Whinnery,
where he made different tooling choices.

We will cover many topics including these:

- Node.js
- npm
- Express
- PostgreSQL
- REST services
- CORS
- HTTPS
- authentication with encrypted tokens
- creating SSL certificates
- React and create-react-app
- {% aTargetBlank "https://socket.io", "socket.io" %}
  for server to client communication
- nodemon

See my Object Computing (OCI) article {% aTargetBlank
"https://objectcomputing.com/resources/publications/sett/april-2017-web-app-step-by-step",
"Web App Step by Step" %}.
