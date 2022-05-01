---
eleventyNavigation:
  key: Containers
layout: topic-layout.njk
---

Containers are packages of application and dependency code.
These can be executed on all the popular operating systems.

For many years {% aTargetBlank "https://www.docker.com", "Docker" %}
has been the most popular tool for creating and running containers.
In 2022 Docker licensing changed so that it is no longer free to use.

A {% aTargetBlank
"https://www.docker.com/blog/do-the-new-terms-of-docker-desktop-apply-if-you-dont-use-the-docker-desktop-ui/",
"blog post" %} on the Docker web site says the following:
"Docker Desktop now requires a per-user paid subscription
(Pro, Team, or Business) for professional use in larger companies
(larger than 250 employees OR greater than $10 million in annual revenue)."

A popular alternative to Docker is to use
{% aTargetBlank "https://buildah.io", "Buildah" %} to create containers and
{% aTargetBlank "https://podman.io", "podman" %} to run them.
Unfortunately, Buildah only runs on Linux systems and
cannot be used on macOS and Windows.

Another alternative that works on Linux, macOS, and Windows is
{% aTargetBlank "https://rancherdesktop.io", "Rancher Desktop" %}.

## Why Containers?

Using containers to package and run apps avoids the issues of:

- missing files
- software version mismatches
- differing configuration settings, including environment variables

Using containers makes it much easier to:

- get new developers started quickly with minimal configuration
  (`docker-compose up`)
- deploy apps
- remove the container for an app and all of its dependencies
  (`docker-compose down --rmi all`)

A container is an isolated environment/process for running an application image.
This includes:

- a minimal version of an operation system
- a runtime environment such as Node.js
- application files
- libraries used by the application files
- environment variables

Docker images can be pushed to a registry like "Docker Hub".
The images can then be download on different machines and run.

## Rancher Desktop

To install Rancher Desktop, browse the link above,
scroll to the "Download Rancher Desktop" section,
and click the link for your operating system.
This is a large download, so it may take several minutes to finish.
On macOS this is a .dmg file that can be double-clicked
to complete the installation.

Double-click the "Rancher Desktop" app in the Applications directory
to start it.
The first time this is done it will prompt to confirm some settings.

An icon will be added to the macOS menu bar.
Click this provides Kubernetes status, enables changing preferences,
and enables changing the active Kubernetes context .
Open "Preferences", and select "Kubernetes Settings".
Under "Container Runtime" select "dockerd".

Once installed, `docker` commands can be entered in a terminal.
To verify that this is working, enter `docker run hello-world`.

## Dockerfile

To run an app in Docker, create a file named `Dockerfile`.
This defines everything that must be included in an "image" in order to run.

Publicly available images can be found at
{% aTargetBlank "https://hub.docker.com", "DockerHub" %}.
This site requires creating a free account.
These images can be used as the base
for creating an application-specific image.

Let's walk through a basic example where all we want to do
is execute a Node.js script that outputs a message.

Here is the Node.js script in the file `index.js`:

```js
console.log('Hello, Docker!');
```

Here is the `Dockerfile`.

```text
# This uses the latest version of Node.js running in Alpine,
# which is a small Linux distribution.
FROM node:alpine

# This copies all files in current directory
# into the app directory of the image.
COPY . /app

# This similar to the UNIX cd command for
# changing the current directory within the image.
WORKDIR /app

# This specifies a command to run inside the image.
CMD node index.js
```

To create an image, enter `docker build -t hello-docker .`
where `-t` specifies a tag for the image
and `.` indicates that `Dockerfile` is in the current directory.

To run this image in a container, enter `docker run hello-docker`.

## Images

To list the current images, enter `docker images` or `docker image ls`.

To run an image in a container, enter `docker run {image-name}`.

To delete an image, enter `docker rmi {image-id}`
or `docker rmi {image-name}`.

## Containers

To list the current containers, enter `docker ps -a` or `docker container ls`.

To delete a container, enter `docker rm {container-id-prefix}`.

To run a container, enter `docker run {container-id}`.
To run in interactive mode, add the `-it` flag.

## Docker Hub

To upload a Docker image to Docker Hub:

- browse https://hub.docker.com
- if you do not already have an account, create one
- otherwise click "Sign In" and login
- in the top nav, click "Repositories"
- click the "Create Repository" button
- enter a name for the repository
- choose between Public and Private
- enter `docker push {user-name}/{repository-name}:{tag-name}`.
- TODO: I could not get this to work!

To download a Docker image from Docker Hub,
enter `docker pull {user-name}/{repository-name}:{tag-name}`.

## Volumes

A volume is a directory or file on the host file system
that is accessible from a container.
Volumes allow an image to read or write data not found inside the container.
Writing to a volume allows data to outlive the container.
Volumes can also be used to share data between containers.

Here is a simple Node.js example that writes to the file `$HOME/demo.txt`
and then reads from it.

The file `index.js` contains:

```js
const fs = require('fs');
const filePath = '/data/test.txt';

const data = 'Hello, World!';
fs.writeFile(filePath, data, err => {
  if (err) throw err;

  console.log('write was successful');

  fs.readFile(filePath, (err, buf) => {
    if (err) throw err;
    console.log('read got:', buf.toString());
  });
});
```

The file `Dockerfile` contains:

```text
FROM node:alpine
COPY . /
WORKDIR /
CMD ["npm", "start"]
```

To build the image, enter `docker build -t volume-demo .`

To run the image in a container, enter
`docker run --rm -v $HOME/data:/data volume-demo`.
The `-v` flag says that the file path `/data` inside the container
should be mapped to the file path `$HOME/data` outside the container.

Include the `-v` flag with a mapping once per volume to be shared.

Another way to use a volume is to explicitly define it.

- cd to the local directory to be shared.
- Create a volume by entering `docker volume create data`
  where "data" is the name to assign to the volume.
- List the defined volumes by entering `docker volume ls`.
- Run the image in a container with the `--mount` flag as follows:
  `docker run --rm --mount source=data,target=/data volume-demo`

Include the `--mount` flag with a mapping once per volume to be shared.

## SvelteKit

To run a SvelteKit server in a Docker container:

- Modify `sveltekit.config.js`.

  Instead of importing the adapter `@sveltejs/adapter-auto`,
  import `@sveltejs/adapter-node`.
  Change the line that creates an adapter instance
  from `adapter()` to `adapter({ out: 'build' })`.

- Create the following `Dockerfile`:

  ```text
  FROM node:alpine

  COPY package.json package-lock.json ./
  RUN npm ci

  COPY . /app
  WORKDIR /app
  RUN npm run build

  EXPOSE 3000
  CMD ["node", "./build"]
  ```

- Create a Docker image by entering `docker build -t {image-name} .`
- Run this in a container by entering `docker run -p 4000:3000 {image-name}`
  which exposes port 3000 inside the container
- Browse localhost:4000.
- Find the id of the container by entering `docker container ls`.
- Stop the container by entering `docker stop {container-id}`.
  This can take around 10 seconds to complete.

## Restricting Network Access

TODO: Coming soon.
