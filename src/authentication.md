---
eleventyNavigation:
  key: Authentication
layout: topic-layout.njk
---

This page describes an approach for implementing
web-based authentication from scratch, not using libraries.
This is useful for understanding all the underlying steps
or implementing a custom authentication approach.

Encryption typically encodes data using a key and an algorithm.
The data can be decoded if the key and algorithm are known.

Hashing is a one-way encoding of data.
While it is possibly given sufficient computing power
to recover the original data, doing so is very difficult.
This is often used to store passwords in a database.
Commonly used hashing algorithms include MD4, MD5,
and SHA (Security Hashing Algorithm).

Salting makes data more secure by adding "salt"
to the data to be encrypted or hashed.
Often a fixed string is added to the
beginning, end, or both ends of the data.

Passing user names, passwords, and email addresses
from browsers to servers in plain text
can be made safe by using HTTPS.

`npm install bcryptjs`
`npm install fastify`
`npm install fastify-cookie`
`npm install fastify-cors`
`npm install jsonwebtoken`
`npm install nodemailer`

HTTP-only cookies can only be created by a server
and can only be retrieved by the server that created them.
They can be made secure and can expire after a given amount of time.
Like other cookies, DevTools can see that they exists,
but their values are not available.
In addition, browser extensions cannot access them.

A JSON Web Token (JWT) encapsulates encoded information in a token
that can be passed to and from servers.
The data is not encrypted, so it should not contain sensitive data.
It is comprised of three parts, a header, payload, and a signature.
The header specifies the algorithm used to encode the data.
The payload holds the data as a JSON string.
Often this includes a user id and session id.
The signature provides information required to decode the data.

Logging in can create a session that
maintains data until the end of the session.

"Access tokens" are JWTs that are used to
indicate that a given user has been authenticated.
They often contain a user id, session id, and role information.
They are only valid for the current session and
typically has a short lifetime.

"Refresh tokens" are JWTs that typically only contains a session id.
They have a longer lifetime than access tokens.
When an access token expires,
a web application can use a refresh token
to request a new access token.
This only works if the session is still valid.
Site administrators can revoke sessions to prevent this.

## Local SSL Setup

In macOS, edit the "hosts" file by entering `sudo vim /etc/hosts`.
Then add the following lines:

```text
127.0.0.1 nodeauth.dev
127.0.0.1 api.nodeauth.dev
```

Caddy is a local server that is implemented in Go and supports HTTPS.
Browse {% aTargetBlank "https://caddyserver.com", "caddyserver.com" %}
for installation instructions.
In macOS this can be installed by installing Homebrew
and entering `brew install caddy` and `brew install nss`.
Create the file `Caddyfile` in the project root directory
containing the following:

```text
{
  http_port 81
  local_certs
}

# This is for the UI server.
nodeauth.dev {
  reverse_proxy 127.0.0.1:5000
}

# This is for the API server.
# Why is it important for this to be a subdomain of the UI server?
api.nodeauth.dev {
  reverse_proxy 127.0.0.1:1919
}
```

To start the Caddy server, cd to the project root directory
and enter `caddy run`.

To stop the Caddy server, press ctrl-c or
enter `caddy stop` in another terminal window.

To reload changes to `Caddyfile` in a running server,
enter `caddy reload` in another terminal window.
