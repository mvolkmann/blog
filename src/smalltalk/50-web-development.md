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

## WebClientPlus package

I created the package WebClientPlus to add features to the WebClient package.
See the [GitHub repository](https://github.com/mvolkmann/Cuis-Smalltalk-WebClientPlus).

### WebServerPlus class

The class `WebServerPlus` is a subclass of `WebServer` which is defined in the WebClient package.
It adds:

- an easier way to define routes
- ability to access path parameters in route handlers
- ability to access query parameters in route handlers

To use it, define a subclass. The provided example is `DogWebServer`.
Its `initialize` instance method creates some initial data and
registers several routes by sending it the message `method:path:handler:`.
Each route has a handler that is either a block or a method selector.

```smalltalk
initialize
    | dog1 dog2 |

    super initialize.

    "Create some initial dogs."
    dog1 := Dog name: 'Comet' breed: 'Whippet'.
    dog2 := Dog name: 'Oscar' breed: 'German Shorthaired Pointer'.
    dogDict := Dictionary newFrom: {
        dog1 id -> dog1.
        dog2 id -> dog2
    }.

    "Register routes."
    self method: #GET path: '/hello' handler: [ :context |
        context request send200Response: 'Hello, World!'
    ].
    "Don't forget the colon the end of the selectors!"
    self method: #GET path: '/dog' handler: #getDogs:.
    self method: #HEAD path: '/dog' handler: #headDogs:.
    self method: #GET path: '/dog/:id' handler: #getDogAsJson:.
    self method: #POST path: '/dog' handler: #createDog:.
    self method: #PATCH path: '/dog/:id' handler: #patchDog:.
    self method: #PUT path: '/dog/:id' handler: #updateDog:.
    self method: #DELETE path: '/dog/:id' handler: #deleteDog:.
    self method: #DELETE path: '/dog' handler: #deleteAllDogs:.
```

### WebContext class

The route handler is passed a `WebContext` object that has
the instance variables `request` (a `WebRequest` object)
and `route` (a `WebRoute` object).

To get the value of a specific path parameter,
send `#pathParameter:` with its name to the `WebContext` object.

To get a `Dictionary` of query parameters,
send `#queryParameters` to the `WebContext` object.

To get the `WebRequest` object, send `#request`.

To get the `WebRoute` object, send `#route`.

### Controlling the server

To start the example server:

```smalltalk
server := DogWebServer new.
server listenOn: 3000.
```

To restart the example server:

```smalltalk
server destroy.
server := DogWebServer new.
server listenOn: 3000.
```

### Testing the example server

To get the port on which the server is listening,
send `#listenerPort` to the server object.

To test the example server, open an SUnit Test Runner,
select `DogWebServerTests`, and click the "Run" button.

To test the individual routes of the example server,
send the requests described below, perhaps using a tool like Postman.

To get all the dogs, send GET http://localhost:3000/dog.
This returns HTML if the Accept header is "text/html" or JSON otherwise.

To get a specific dog as JSON, send GET http://localhost:3000/dog/{id}.

To get all dogs and print the query parameters in the Transcript,
GET http://localhost:3000/dog?size=medium&color=brindle.

To create a new dog, send POST http://localhost:3000/dog
with JSON body { "name": "Snoopy", "breed": "Beagle" }.

To update an existing dog, send PUT http://localhost:3000/dog/{id}
with JSON body { "name": "Fireball", "breed": "Greyhound" }.

To delete an existing dog, send DELETE http://localhost:3000/dog/{id}.

### WebClientPlus class

The class `WebClientPlus` is a subclass of `WebClient`
which is defined in the WebClient package.
This simplifies sending HTTP requests.
It is used by the `DogWebServerTests` class
to test each of the routes defined above.
