---
eleventyNavigation:
  key: Podman
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

{% aTargetBlank "https://podman.io", "podman" %}
is a tool for working with software containers and Kubernetes.
It is a popular alternative to Docker.

## Installing in macOS

- If you previously installed the "Podman Desktop" app, delete it.
- If you previously installed podman using Homebrew,
  enter `brew uninstall podman`.
- Delete any existing `podman` executables in your PATH
  by repeatedly entering `which podman` and deleting the file that finds.
- Download the installer for the podman CLI from https://podman.io.
- Run that installer.
- `podman machine init`
- `podman machine start`
- `podman info`

## Node.js Example

1. Create a new directory and cd into it.

1. Enter `npm init` to create a `package.json` file.

1. Add the following scripts in `package.json`:

   ```json
   "build": "podman build -t podman-node-image .",
   "run": "podman run -d -p 3000:3000 --name podman-node-container podman-node-image"
   ```

1. Create the file `index.js` containing the following:

   ```js
   const http = require('http');
   const port = 3000;

   const server = http.createServer((req, res) => {
     res.statusCode = 200;
     res.setHeader('Content-Type', 'text/plain');
     res.end('Hello, Podman!\n');
   });

   server.listen(port, () => {
     console.log(`Server running on port ${port}`);
   });
   ```

1. Create the file `Docker` containing the following:

   ```docker
   # Use an official Node.js runtime as a parent image.
   FROM node:latest

   # Set the working directory in the container.
   WORKDIR /usr/src/app

   # Copy package.json and package-lock.json if they exist.
   COPY package*.json ./

   # Install application dependencies.
   RUN npm install

   # Copy the rest of the application code.
   COPY . .

   # Expose the port the app listens on.
   EXPOSE 3000

   # Define the command to run the app.
   CMD [ "node", "index.js" ]
   ```

1. Enter `npm run build` to build the image.

1. Enter `npm run` to run the image in a container.

## Finding Containers

`podman search docker.io/busybox`
