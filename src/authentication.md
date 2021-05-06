---
eleventyNavigation:
  key: Authentication
layout: topic-layout.njk
---

This page describes an approach for implementing
web-based authentication from scratch, not using libraries.
This is useful for understanding all the underlying steps
or implementing a custom authentication approach.

The solution described here is implemented in the
{% aTargetBlank "https://github.com/mvolkmann/authentication-fastify",
"authentication-fastify" %} GitHub repo.

## Terminology

**Encryption** typically encodes data using a key and an algorithm.
The data can be decoded if the key and algorithm are known.

**Hashing** is a one-way encoding of data.
When applied to passwords the result is referred to as a "password digest".
These values are often stored in a database and used for authentication.
The password entered in a login attempt is hashed using the same approach
and that is compared to the hashed value in the database.
Commonly used hashing algorithms include
{% aTargetBlank "https://github.com/kelektiv/node.bcrypt.js", "bcrypt" %},
MD4, MD5, and SHA (Security Hashing Algorithm).

**{% aTargetBlank "https://en.wikipedia.org/wiki/Rainbow_table",
"Rainbow tables" %}** can be used by hackers to determine if
a hashed password corresponds to a commonly used password.
These map the hashes of commonly used passwords
to their plain text equivalents.
Rainbow tables for specific hashing algorithms can be found on the internet.

**Bcrypt** as described by {% aTargetBlank
"https://en.wikipedia.org/wiki/Bcrypt", "Wikipedia" %}
is a password-hashing function designed by
Niels Provos and David Mazières, based on the Blowfish cipher."
The bcrypt hashing algorithm is purposely slower than other hashing algorithms
in order to make generating rainbow tables very time consuming,
while still being fast enough for single hashing operations
required for uses like authentication.

**Salting** makes data more secure by adding "salt"
to the data to be encrypted or hashed.
This is typically a fixed-length string that is added to
the beginning, end, or both ends of data to be hashed.
This makes existing rainbow tables useless to hackers
because the hashes they include are not based on salted passwords.

**Hypertext Transfer Protocol Secure (HTTPS)**
uses Transport Layer Security (TLS) encryption
to send data between browsers and servers.
Using this makes it safe to pass user names, passwords, and email addresses.

**HTTP-only cookies** can only be created by a server
and can only be retrieved by the server that created them.
Browser extensions cannot access them.

**Secure cookies** can only be sent over HTTPS.
And like all cookies, they can expire after a given amount of time.

**JSON Web Tokens (JWTs)** encapsulate encoded information in a token
that can be passed to and from servers.
The data is not encrypted, so it should not contain sensitive data.
It is comprised of three parts, a header, payload, and a signature.
The header specifies the algorithm used to encode the data.
The payload holds the data as a JSON string.
Often this includes a user id and session id.
The signature provides information required to decode the data.

**Sessions** hold data associated with a user session.
The login process of web applications often creates a session
represented by a record in a database.
This can be used to maintain data until the end of the session.

**Access tokens** are JWTs that are used to
indicate that a given user has been authenticated.
They often contain a user id, session id, and role information.
They are only valid for the current session and
typically has a short lifetime.

**Refresh tokens** are JWTs that typically only contains a session id.
They have a longer lifetime than access tokens.
When an access token expires,
a web application can use a refresh token
to request a new access token.
This only works if the session is still valid.
Site administrators can revoke sessions to prevent this.

## Strategies

This section describes a number of strategies to securely store user passwords
in order from least to most secure.

1. Store passwords in plain text in an "inaccessible" location.

   This approach is strongly discouraged because hackers often find a way
   to access data stores that were thought to be inaccessible.

1. Store encrypted versions of passwords.

   If a hacker gains access to the data store AND
   determines the encryption algorithm that was used,
   they can decrypt any of the passwords.

1. Store hashed versions of passwords.

   Encryption and hashing algorithms always
   produce the same output for a given input.
   But encryption is two-way and hashing is one-way.
   Encrypted values can be decrypted.
   Hashed values cannot be used to recovered the original value.
   This is analogous to the distinction between
   loss-less and lossy image compression.

   A hacker can use a rainbow table to discover commonly used passwords.
   From {% aTargetBlank "https://en.wikipedia.org/wiki/Rainbow_table",
   "Wikipedia" %}, "A rainbow table is a precomputed table for caching the
   output of cryptographic hash functions, usually for cracking password hashes."

1. Store passwords that add a common salted and are hashed.

   Salting adds a fixed number of bytes to the passwords before they are hashed.
   This prevents rainbow tables from being used to discover the use of common passwords.
   However, if a hacker learned the salt value that was added to all of the passwords,
   they could generate a new rainbow table that is useful.

1. Store passwords that add a different salt to each password before hashing.

   With this approach the salt used for each user will also need to be stored.
   A common approach is to add it to the beginning of hashed password.
   The steps to hash the password for a single user would be:

   - Generate a new salt for the user.
   - Hash the password using the new salt and the plain text password.
   - Concatenate the new salt and the hashed value.
   - Store this value.

   If a hacker gains accessed to these hashed values
   and they know the length of the salt values and
   where there are added to the hashed value (ex. beginning or end),
   they can generate a rainbow table in order to determine if
   the password is one from a list of commonly used passwords.
   However, a new rainbow table would be needed for each user
   since each user uses a different salt value.

   The bcrypt hashing algorithm is purposely slower than other hashing algorithms
   in order to make creation of rainbow tables time consuming,
   while still being fast enough for single hashing operations
   required for uses like authentication.

## Libraries

The npm packages used in the demonstration app include:

- `bcryptjs`
- `fastify`
- `fastify-cookie`
- `fastify-cors`
- `jsonwebtoken`
- `nodemailer`

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
