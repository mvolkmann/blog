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

## Major Issue

I sent the folllowing to the Cuis mailing list on September 24, 2024:

Here is a very simple use of the WebServer class that can be installed with
`Feature require: 'WebClient'`. A fileOut containing this code is attached.
The class name is BasicWebServer
and it has the single instance variable `server`.
It contains only these instance methods.

```smalltalk
initialize
    server := WebServer new.
    server addService: '/' action: [:req | req send200Response: 'Hello, World!'].

start
    server listenOn: 3000

stop
    server stopListener
```

I can start the web server with:

```smalltalk
server := BasicWebServer new.
server start.
```

Then I can browse localhost:3000 and see "Hello, World!" in the web browser.

I can stop the web server with:

```smalltalk
server stop.
```

But I cannot start it again unless I restart the VM.
Can someone please explain what else I can do to enable
restarting the server without needing to restart the VM?

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
