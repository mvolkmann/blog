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
- Enter `podman machine init` to create a podman machine.
- Enter `podman machine start` to start the podman machine
- Enter `podman info` to verify that the previous commands worked.

## Names

The names of images and containers are composed of three parts
and have the syntax `{registry}/{repository}:{tag}`.

When an image or container is downloaded from the web, the
`{registry}` part indicates the source code registry it came from".

When an image or container is created locally and
the `{registry}` part cannot be specified and defaults to "localhost".

The tag can be used to identify multiple versions of an image or container.
When `:{tag}` part is omitted, it defaults to "latest".

## Images

To build an image using the `Dockerfile` file in the current directory,
enter `podman build -t {image-name} .`
The `.` at the end indicates that the file `Dockerfile`
can be found in the current directory.

To list all the images created on your machine, enter `podman images`.

To delete an image, enter `podman rmi {name-or-id}`.
If a name is specified,
it must match the pattern `{registry}/{repository}:{tag}`
or `{registry}/{repository}` if no tag was specified when it was created.
Attemtping to delete an image will fail
if it is being used by an existing container.

## Containers

To create a container using a given image, enter the following command:

```bash
podman create --name {container-name} {image-name}"
```

Often the container exposes a given port (container-port)
and you want to map that to port on your machine (host-port).
This is done by adding the flag `-p {host-port}:{container-port}`
to the previous command.
Typically `host-port` and `container-port` are the same.

To list all the containers created on your machine,
enter `podman ps -a` (for all) or `podman container list -a`.

To start a container, enter `podman start {name-or-id}`.

To stop a running container, enter `podman stop {name-or-id}`.
This can take around 10 seconds.

To delete a container, enter `podman rm {name-or-id}`.
This will fail if the container is running.

To create and run a container using a given image in a single command,
enter the following command:

```bash
podman run --name {container-name} {image-name}"
```

Add the `--rm` flag to delete the container after it runs.

Add the `-d` flag to run the container in detached mode (in the background).
When not running in detached mode, the process cannot be stopped by pressing ctrl-c.
Instead, open another terminal and enter `podman stop {name-or-id}`.

## Finding Available Containers

`podman search docker.io/busybox`

## Executable Example

1. Create a new directory and cd into it.

1. Create the file `script.sh` containing the following:

   ```bash
   #!/bin/sh
   echo "Hello, World!"
   ```

1. Enter `chmod a+x script.sh` to make it executable.

1. Enter `. ./script.sh` to test the script.

1. Create the file `Docker` containing the following:

   ```docker
   # Use a lightweight base image.
   FROM alpine:latest

   # Set the working directory in the container
   WORKDIR /app

   # Copy the local executable (a script here) into the image.
   COPY script.sh /app/script.sh

   # Give execute permission to the executable.
   RUN chmod +x /app/script.sh

   # Set the default command to run the executable
   CMD ["/app/script.sh"]
   ```

1. Build an image by entering the following:

   ```bash
   podman build -t podman-executable-image .
   ```

1. Create a container by entering the following:

   ```bash
   podman create --name podman-executable-container podman-executable-image
   ```

1. Start the container by entering the follwoing:

   ```bash
   podman start podman-executable-container
   ```

1. View the output by entering the following:

   ```bash
   podman logs podman-executable-container
   ```

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
