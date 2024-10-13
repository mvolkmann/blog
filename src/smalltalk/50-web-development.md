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

To enable sending and receiving requests with JSON bodies,
open a Workspace, enter `Feature require: 'Json'`, and "Do it".

The packages
<a href="https://github.com/SeasideSt/Seaside" target="_blank">Seaside</a> and
<a href="https://github.com/zeroflag/Teapot" target="_blank">Teapot</a>
are often used with other Smalltalk distributions,
but they do not support Cuis Smalltalk.

See the <a href="https://book.seaside.st/book" target="_blank">Seaside Book</a>.

## Simple Example

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
```

Create and start the web server with:

```smalltalk
server := BasicWebServer new.
server listenOn: 3000.
```

Browse localhost:3000 to see "Hello, World!".

Stop the web server with `server destroy`.

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

## WebClientPlus package

I created the package WebClientPlus to add features to the WebClient package.
See the [GitHub repository](https://github.com/mvolkmann/Cuis-Smalltalk-WebClientPlus).

Let's walk through the classes this package defines and example routes
that support CRUD (Create, Read, Update, Delete) operations on dogs.

### WebServerPlus class

The class `WebServerPlus` is a subclass of `WebServer`,
which is defined in the WebClient package.
It adds:

- an easier way to define routes
- ability to access path parameters in route handlers
- ability to access query parameters in route handlers

To use the `WebServerPlus` class, define a subclass.
The provided example `DogWebServer` does that.
Its `initialize` instance method creates some initial data in a `Dictionary`
where the keys are increasing integers and the values are `Dog` objects.
It then registers several routes
by sending the message `method:path:handler:` to `self`.
Each route has a handler that is either a block or a method selector.

The route is passed a `WebContext` object
that holds a `WebRequest` object and a `WebRoute` object.
Passing a single object to a route handler
that encapsulates everything needed to a handle a request
is an idea that was copied from the Hono JavaScript framework.

```smalltalk
initialize
    | dog1 dog2 |

    super initialize.

    "Create some initial dogs."
    "The Dog class method name:breed: assigns an id."
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
    self method: #GET path: '/dog' handler: #getDogs:.
    self method: #HEAD path: '/dog' handler: #headDogs:.
    self method: #GET path: '/dog/:id' handler: #getDogAsJson:.
    self method: #POST path: '/dog' handler: #createDog:.
    self method: #PATCH path: '/dog/:id' handler: #patchDog:.
    self method: #PUT path: '/dog/:id' handler: #updateDog:.
    self method: #DELETE path: '/dog/:id' handler: #deleteDog:.
    self method: #DELETE path: '/dog' handler: #deleteAllDogs:.
```

When defining routes where the handler is specified with a method selector,
remember to include a colon at the end of the selector.

The following code is an example of defining route handler.
It gets data describing a dog whose id found in a path parameter
and it sends a response with a JSON body.

```smalltalk
getDogAsJson: aWebContext
    | dog id req |

    req := aWebContext request.
    id := aWebContext pathParameter: #id.
    id := [id asNumber] on: Error do: [ :e | ^ req send400Response: e messageText].

    dog := dogDict at: id ifAbsent: nil.
    dog
        ifNil: [ req send404Response ]
        ifNotNil: [
            req
                send200Response: (Json render: dog)
                contentType: WebServerPlus jsonContentType
        ].
```

### WebContext class

The route handler is passed a `WebContext` object that has
the instance variables `request` (a `WebRequest` object)
and `route` (a `WebRoute` object).

To get the value of a specific path parameter,
send `#pathParameter:` with its name.

To get a `Dictionary` of query parameters, send `#queryParameters`.

To get the `WebRequest` object, send `#request`.

To get the `WebRoute` object, send `#route`.

### Controlling the server

To start the example server:

```smalltalk
server := DogWebServer new.
server listenOn: 3000.
```

To stop the server, send `#destroy` to the server object.

To get the port on which the server is listening,
send `#listenerPort` to the server object.

### Testing the example server

To test the example server, open an SUnit Test Runner,
select `DogWebServerTests`, and click the "Run" button.
This will start the server if it is not already running.

To test the individual routes of the example server,
send the requests described below, perhaps using a tool like Postman.

To get all the dogs, send GET http://localhost:3000/dog.
This returns HTML if the Accept header is "text/html" or JSON otherwise.

To get a specific dog as JSON, send GET http://localhost:3000/dog/{id}.

To get all the dogs and print query parameters in the Transcript,
GET http://localhost:3000/dog?size=medium&color=brindle.

To create a new dog, send POST http://localhost:3000/dog
with JSON body { "name": "Snoopy", "breed": "Beagle" }.

To update an existing dog, send PUT http://localhost:3000/dog/{id}
with JSON body { "name": "Fireball", "breed": "Greyhound" }.

To delete an existing dog, send DELETE http://localhost:3000/dog/{id}.

### WebClientPlus class

The class `WebClientPlus` is a subclass of `WebClient`
which is defined in the WebClient package.
This simplifies sending HTTP requests by providing the following class methods:

```smalltalk
method:url:
method:url:content:
method:url:headers:
method:url:headers:content:
```

The first three of these methods all delegate to the last method.

These methods are used by the `DogWebServerTests` class
to test each of the routes defined in the `DogWebServer` initialize method.
