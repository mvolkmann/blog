---
eleventyNavigation:
  key: Web Development
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

One package for implementing a web server is
<a href="https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Packages/Features/WebClient.pck.st"
target="_blank">WebClient</a>.
To install this, open a Workspace, enter `Feature require: 'WebClient'`,
and "Do it".
This adds many classes in the "WebClient - Core" category including
`WebClient`, `WebRequest`, `WebResponse`, `WebServer`, and `WebSocket`.
In addition, to send and receive requests with JSON bodies,
open a Workspace, enter `Feature require: 'Json'`, and "Do it".

Also see the
<a href="https://github.com/SeasideSt/Seaside" target="_blank">Seaside</a> and
<a href="https://github.com/zeroflag/Teapot" target="_blank">Teapot</a> frameworks.

See the <a href="https://book.seaside.st/book" target="_blank">Seaside Book</a>.

## Basic Example

Here is a very simple use of the WebServer class that can be installed with
`Feature require: 'WebClient'`.
Create the class `BasicWebServer` as follows:

```smalltalk
Object subclass: #BasicWebServer
    instanceVariableNames: 'server'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Volkmann'
```

Add the following instance methods:

```smalltalk
initialize
    server := WebServer new.
    server addService: '/' action: [:req | req send200Response: 'Hello, World!'].

start
    server listenOn: 3000

stop
    server destroy
```

Create and start the web server with:

```smalltalk
server := BasicWebServer new.
server start.
```

Browse localhost:3000 to see "Hello, World!".

Stop the web server with `server stop`.

Restart it with `server start`.

## Sending HTTP Requests

The `WebClient` class defines class methods that send HTTP requests.
For example:

```smalltalk
res := WebClient httpGet: 'https://pokeapi.co/api/v2/pokemon/pikachu'.
```

The variable `res` above will refer to a `WebResponse` object.
This has many instance variables including:

- `code` - status code such as 200
- `content` - response body
- `headers` - an `OrderedCollection` of `Association` objects
  that describe the HTTP response headers such as `content-type` with a
  value like `text/html; charset=utf-8` or `application/json; charset=utf-8`
- `protocol` - such as the `String` `HTTP/1.1`
- `status` - such as the `String` `HTTP/1.1 200 OK`
- `url` - the URL to which the request was sent

## Implementing an HTTP Server

To start a web server, create a `WebServer` instance
and send it the `listenOn:` message.
this starts a Smalltalk process called "WebServers's listener process".
To kill it, open a "Process Browser", select the process,
and press cmd-t (Terminate).

See the class `MyWebServer` in the `Volkmann` package.
The `handleDog:` method defines CRUD endpoints.
TODO: Provide more detail about defining endpoints.

## WebClient Issues

- It doesn't support paths containing path parameters.
- It doesn't support query parameters.
- It doesn't support PATCH requests.

## Work In Progress

In your Volkmann2 image, see the classes
`MyWebServer`, `WebContext`, `WebHtmlStr`, and `WebRoute`.
Routes are defined in the `initialize` method of `MyWebServer`
by sending messages to `method:path:selector:`.

To start the server:

```smalltalk
server := MyWebServer new.
server start.
```

To restart the server:

```smalltalk
server stop.
server := MyWebServer new.
server start.
```

To test the server, send the following requests:

To get all dogs, GET http://localhost:3000/dog.
This returns JSON if Accept header is "application/json" or as HTML otherwise.

To get a specific dog as JSON, GET http://localhost:3000/dog/{id}.

To get all dogs and print the query parameters in the Transcript,
GET http://localhost:3000/dog?size=medium&color=brindle.

To create a new dog, POST http://localhost:3000/dog
with JSON body { "name": "Snoopy", "breed": "Beagle" }.

To update an existing dog, PUT http://localhost:3000/dog/{id}
with JSON body { "name": "Fireball", "breed": "Greyhound" }.

To delete an existing dog, DELETE http://localhost:3000/dog/{id}.
