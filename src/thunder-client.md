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

To install Thunder Client in VS Code,
click the side bar extensions icon,
search for "Thunder Client",
and click the Install button.

Good documentation for this extension is provided inside VS Code.

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
The method can be GET, POST, PUT, DELETE, PATCH, HEAD, or OPTIONS.
The following optional features can also be specified:
query parameters, authentication, request headers, body, and tests.
Authentication can use Basic Auth, Bearer Token, or OAuth 2.0.
The body can contain text, JSON, XML, form data, files,
form-url-encoded key/value pairs, or a GraphQL query.

To create a one-time request that is not associated with a collection,
click the "New Request" button above the Activity, Collections, and Env tabs.

Any number of tests can be added to a request.
Result data that can be tested includes ResponseCode, ResponseType,
ResponseTime, Content-Type, Content-Length, Content-Encoding, "Set Env Var",
Header, and "Json Query".
Each test has an action and a value.
Actions include equal, notEqual, contains, count,
isType, isJson, setTo, <, <=, >, and >=.
A "Json Query" specifies the path to a particular piece of data
starting with `json.`.
For example, if the API returns a JSON array of user objects
that have a lastName property, the path to this property
in the first user object is `json.0.lastName`.

To view the requests in a given collection,
click the "Collections" tab and click the collection name.

To execute a request, select it.
This opens the request in an editor tab.
Optionally modify the request details and then click the "SEND" button.
The results include the status, size, time,
response body, response headers, cookies, and test results.

To execute all the requests in a collection,
click the ellipsis after the collection name and select "Run All".
This is particularly useful when the requests have associated tests.
It provides a quick way to verify that all the APIs
exercised by the collection are still returning the expected responses.

To rename a request, click the ellipsis after it, select "Rename",
and enter a new name.

To duplicate a request in order to
create one that is similar to an existing request,
click the ellipsis after it, select "Duplicate".
The name of the new request will match the original
with " Copy" added to the end.
Then edit the new request.

To delete a request,
click the ellipsis after it and select "Delete".

To delete a collection of requests,
click the ellipsis after it and select "Delete".

## Env tab

The Env tab is for entering repetitive or sensitive data
that is used in requests.
To create a new environment, click the Env tab,
click the hamburger menu on the right,
select "New Environment", and enter a name.
To add variables to the environment, click the environment name,
enter name/value pairs, and click the Save button.
To refer to a variable value in a request,
enclose the name in double curly braces.
For example, `{{my-name}}`.

## Data

The data associated with collections of requests (Collections tab)
and environments (Env tab) are shared by all VS Code windows.
However, newly added data is not
automatically available in other VS Code windows.
To update the data in the current VS Code window,
open the context menu and select "Developer: Reload Window".

## Questions

What is the purpose of the "Activity" tab?
Nothing new ever seems to appear there.
