---
eleventyNavigation:
  key: Thunder Client
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.thunderclient.io", "Thunder Client" %}
is a VS Code extension for testing APIs.
It is similar to {% aTargetBlank "", "Postman" %}.

## Installing

To install Thunder Client in VS Code, ...

## Using

To start Thunder Client, click the sidebar icon
that is a circle containing a lightning bolt.

To create a new collection of requests,
click the "Collections" tab, click the hamburger menu on the right,
click "New Collection", and enter a name.

To create a new request in an existing collection,
click the "Collections" tab, click the ellipsis after the collection name,
click "New Request", and enter a name for the request.
Then select the method from a drop-down (defaults to GET)
and enter the URL for the request.
The following optional features can also be specified:
query parameters, authentication, request headers, body, and tests.

Any number of tests can be added to a request.
Result data that can be tested includes ResponseCode, ResponseType,
ResponseTime, Content-Type, Content-Length, Content-Encoding, "Set Env Var",
Header, and "Json Query".
Each test has an action and a value.
Actions include equal, notEqual, contains, count,
isType, isJson, setTo, <, <=, >, and >=.

To view the requests in a given collection,
click the "Collections" tab and click the collection name.

To execute a request, select it.
This opens the request in an editor tab.
Optionally modify the request details and then click the "SEND" button.
The results include the status, size, time,
response body, response headers, cookies, and test results.

To rename a request, click the ellipsis after it, select "Rename",
and enter a new name.

To duplicate a request in order to
create one that is similar to an existing request,
click the ellipsis after it, select "Duplicate".
The name of the new request will match the original
with " Copy" added to the end.
Then edit the new request.

To delete a request, click the ellipsis after it and select "Delete".

To delete a collection of requests, ...

## Env tab

The Env tab is for entering repetitive or sensitive data
that is used in requests.
But it doesn't seem to be a collection of key/value pairs,
so it's not clear how the values can be referenced by requests.

## Questions

What is the purpose of the "Activity" tab?
Nothing new ever seems to appear there.
