---
eleventyNavigation:
  key: Thunder Client
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://www.thunderclient.io", "Thunder Client" %}
is a VS Code extension for testing APIs.
It is similar to {% aTargetBlank "", "Postman" %},
but has the advantage of being able to test APIs
in the same tool where code is being written.

<img alt="Thunder Client" style="width: 90%"
  src="/blog/assets/vs-code-thunder-client.png?v={{pkg.version}}"
  title="Thunder Client">

## Installing

To install Thunder Client in VS Code,
click the side bar extensions icon,
search for "Thunder Client",
and click the Install button.

Good documentation for this extension is provided inside VS Code.

## Using

To start Thunder Client, click the sidebar icon
that is a circle containing a lightning bolt.

<img alt="Thunder Client icon" style="width: 10%"
  src="/blog/assets/vs-code-thunder-client-icon.png?v={{pkg.version}}"
  title="Thunder Client icon">

To create a new collection of requests,
click the "Collections" tab, click the hamburger menu on the right,
click "New Collection", and enter a name.

To create a new request in an existing collection,
click the "Collections" tab, click the ellipsis after the collection name,
click "New Request", and enter a name for the request.
Then select the method from a drop-down (defaults to GET)
and enter the URL for the request.
The method can be GET, POST, PUT, DELETE, PATCH, HEAD, or OPTIONS.
The following optional request features can also be specified:
query parameters, authentication, request headers, body,
and tests (more on these later).
Authentication can use Basic Auth, Bearer Token, or OAuth 2.0.
The body can contain text, JSON, XML, form data, files,
form-url-encoded key/value pairs, or a GraphQL query.

To create a one-time request that is not associated with a collection,
click the "New Request" button above the Activity, Collections, and Env tabs.

To view the requests in a given collection,
click the "Collections" tab and click the collection name.

To execute a request, select it.
This opens the request in an editor tab.
Optionally modify the request details and then click the "SEND" button.
The results include the status code, size, time,
response body, response headers, cookies, and test results.

To execute all the requests in a collection,
click the ellipsis after the collection name and select "Run All".
This is particularly useful when the requests have associated tests.
It provides a quick way to verify that all the APIs
exercised by the collection are still returning the expected responses.

To rename a request, click the ellipsis after it, select "Rename",
and enter a new name.

To duplicate a request in order to create one that is similar,
click the ellipsis after it and select "Duplicate".
The name of the new request will match the original
with " Copy" added to the end.
Then rename and edit the new request.

To delete a request,
click the ellipsis after it and select "Delete".

To delete a collection of requests,
click the ellipsis after it and select "Delete".

## Tests

Any number of tests can be added to a request.
Types types include ResponseCode, ResponseType, ResponseTime,
Content-Type, Content-Length, Content-Encoding, "Set Env Var",
Header, and "Json Query".
Each test has an action and a value.
Each action can be equal, notEqual, contains, count,
isType, isJson, setTo, <, <=, >, or >=.
For example, to test that the status code is 200,
add a ResponseCode test with an action of equal and a value of 200.

A "Json Query" test specifies the path to a
particular piece of response data starting with `json.`.
For example, if the API returns a JSON array of user objects
that have a lastName property, the path to this property
in the first user object is `json.0.lastName`.

A "Set Env Var" test specifies that the response value at a Json Query path
matches the value of a specified environment variable.

## Environments

The Env tab is used to create collections of key/value pairs
referred to as an "environment"
that holds repetitive or sensitive data used in requests.

To create a new environment, click the Env tab,
click the hamburger menu on the right,
select "New Environment", and enter a name.

To add variables to the environment, click the environment name,
enter name/value pairs, and click the Save button.

{% raw %}
To refer to a variable value in a request,
enclose the name in double curly braces.
For example, <code>{{my-name}}</code>.
This syntax can appear in request URLs and in Auth values.
It likely can also appear in request headers and bodies,
but I have not test that.
{% endraw %}

## Data

The data associated with collections of requests (Collections tab)
and environments (Env tab) are shared by all VS Code windows.
However, newly added data is not
automatically available in other VS Code windows.
To update the Thunder Client data in the current VS Code window,
open the context menu and select "Developer: Reload Window".

## Questions

What is the purpose of the "Activity" tab?
It should at least provide a list of recently executed requests,
but nothing new ever seems to appear here.
