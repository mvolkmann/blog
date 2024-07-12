---
eleventyNavigation:
  key: Docker
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

<figure style="width: 30%">
  <img alt="Docker logo" style="border: 0"
    src="/blog/assets/docker.png?v={{pkg.version}}">
</figure>

## Overview

{% aTargetBlank "https://www.docker.com", "Docker" %}
"helps developers build, share, run, and verify applications anywhere —
without tedious environment configuration or management."

The primary appeal of using Docker is that "Dockerized" apps can run anywhere.
This includes Windows, macOS, and Linux.
Each platform must install Docker.
Applications run in a virtual Linux environment.
Many Linux variants are supported.

The most common use of Docker is to run server software.
For example, it can be used to run a web server or API server.

At a high level, the steps involved in using Docker include:

- Create a file named `DockerFile` that describes an "image".
- Build the image.
- Run the image in a "container".

## Installing

The steps to install Docker in Windows are:

- Browse <a href="https://docs.docker.com/docker-for-windows/"
  target="_blank">https://docs.docker.com/docker-for-windows/</a>.
- Scroll to and click "Get Docker for Windows (stable)" to download a `.msi` file.
- Double-click the downloaded .msi file and follow the instructions.

The steps to install Docker in macOS are:

- Browse <a href="https://docs.docker.com/docker-for-mac/"
  target="_blank">https://docs.docker.com/docker-for-mac/</a>.
- Scroll to and click "Get Docker for Mac (stable)" to download a `.dmg` file.
- Double-click the download `.dmg` file.
- In the install dialog, drag `Docker.app` to the `Applications` folder.

It is also possible to install Docker in macOS using Homebrew,
but it requires more steps.
See <a href="https://nickcharlton.net/posts/docker-via-homebrew.html"
target="_blank">https://nickcharlton.net/posts/docker-via-homebrew.html</a>.

The steps to install Docker in Linux
vary based on the Linux distribution being used.
See <a href="https://docs.docker.com/engine/installation/linux/"
target="_blank">https://docs.docker.com/engine/installation/linux/</a>.

## Verifying Installation

The easiest way to verify that the installation was successful
is to download and run the "hello-world" image.
To do this enter: `docker run hello-world`.
This will download the image, and output information related to the download,
the line "`Hello from Docker!`",
and information to help you get started using Docker.
It also results in creation of an image and a container.

To see the new image, enter `docker images`.

To see the new container, enter: `docker ps -a`.

To run the container again, enter `docker start -a {container-id-prefix}`.

To get help on a docker command, enter: `docker help {command}`.
For example, `docker help start` briefly describes the `-a` option
which is used to attach to an existing container.

## Docker Images

A Docker image is a filesystem and a set of parameters to use at runtime.
It doesn't have state and never changes.
Images can run a set of commands and exit,
but more commonly they continue running until stopped.
Examples include web servers, API servers, and database daemons.
An image runs inside a container.

Images are described by and built from files that,
by convention, are named "`Dockerfile`".
When building an image, the `-f` option of the `docker build` commmand
can be specified to use a file with a different name.

### Official Base Images

Some images are labelled as "official". For details on what this means, see
<a href="https://github.com/docker-library/official-images"
target="_blank">https://github.com/docker-library/official-images</a>.
For a list of the official images, sorted on
the number of times they have been "pulled", see
<a href="https://hub.docker.com/explore/"
target="_blank">https://hub.docker.com/explore/</a>.

Official programming language images include
clojure, dart, elixir, erlang, gcc, golang, groovy, haskell, haxe,
ibmjava, java, julia, jruby, mono, node, openjdk, perl,
php, python, ruby, rust, and swift.

Official web server images include
glassfish, httpd (Apache), jetty, nginx, and tomcat.

Official database images include
cassandra, couchbase, couchdb, mariadb, mongo, mongoexpress,
mysql, neo4j, oraclelinux, postgres, redis, and rethinkdb.

Official Linux images include
debian, centos, fedora, opensuse, and ubuntu.

Other official images of note include
bash, drupal, hello-world, jenkins, maven, rails,
redmine, and wordpress.

## Examples

Before diving into details about Docker,
let's walk through some simple examples.

We should only have one image now.
That is the one created while verifying the installation.
Verify this by running `docker images`.

### Bash Example

Let's create a very simple image that just outputs "Hello, World!" using bash.
Here is the `Dockerfile` that describes the image.

```docker
FROM bash
CMD echo Hello, World!
```

To build this image enter: `docker build -t my-demo .`<br>
Because we don't already have the `bash` base image,
that is downloaded and built.

To see all the current images enter: `docker images`<br>
We should have three images now. They are named
`hello-world`, `bash,` and `my-demo`.

To run the `my-demo` image enter: `docker run my-demo`<br>
This runs the image in a newly created container.
The container still exists.

To see this enter: `docker ps -a | grep my-demo`

To run this container again enter: `docker start -a {container-id}`

To delete this container enter: `docker rm {container-id-prefix}`

To delete the `my-demo` image enter: `docker rmi my-demo`<br>
Note that the `bash` image on which it depended was not deleted,
but it can be deleted with `docker rmi bash`.
It is common to create multiple images that use the same base image
and retain these rather than delete them.

### Node Example

Now let's create an image that outputs "`Hello, World!`" using Node.js.
It uses the "chalk" npm package to output colored text.
We are doing this to illustrate using npm packages from a Docker image.

Here is the `package.json` file that describes dependencies.
It can also specify a "script" for running the application.

```json
{
  "name": "chalk-demo",
  "version": "1.0.0",
  "description": "a Hello World app that uses the chalk npm package",
  "main": "index.js",
  "scripts": {
    "start": "node index.js"
  },
  "license": "MIT",
  "dependencies": {
    "chalk": "^1.1.3"
  }
}
```

Here is the JavaScript code from the file `index.js`.

```js
const chalk = require('chalk');

// process.argv holds command-line arguments.
// The first value is the path to the node executable.
// The second value is the path to the file being executed.
// The third value (at index 2) is the first command-line argument.
const name = process.argv[2] || process.env.name || 'World';
console.log('Hello, ' + chalk.red.bold(name) + '!');
```

To run this Node application outside of Docker
with no excess output from npm,
enter: `npm -s start`
Note how "World" is output in red.

Here is the Dockerfile that describes the image to be built.
It specifies a specific version of Node, 7.4.
An alternative it to request the latest long-term support (LTS) version
by replacing the version number with the code name "boron".

```docker
FROM node:7.4-onbuild
ENV name ""
CMD ["npm", "--silent", "start"]
```

Base images with an "`onbuild"` tag
use `ONBUILD` instructions to specify
instructions to be executed when the using image is executed.
These instructions are executed as if were inserted
immediately after the FROM instruction.
In the case of node onbuild images, the following
happens automatically when the image is executed:

- Assume the current directory contains a `package.json` file.
- Run `npm install` which installs the dependencies
  described in `package.json`.
- Assume `package.json` defines
  a `start` script.
- Run `npm start`.

The CMD instruction in the Dockerfile above overrides the one provided
by the `node onbuild` image to use the `--silent` option.
This suppresses informational messages from npm.

To build the Docker image enter: `docker build -t node-demo .`<br>
This will take several minutes because it needs to
download and build the `node` base image.
Once the base image has been built, rebuilding this image,
or other images that use the same base image,
will build much faster.

To run this image, but not retain the container in which it runs,
enter: `docker run -t --rm node-demo`<br>
The `-t` option tells Docker to run the image in a pseudo-TTY
which is able to process the ANSI escape codes
output by the `chalk` package to produce colored text.
The `--rm` option tells Docker not to retain
the container it creates after the image exits.
Note that it is preceded by two dashes, not one.

To override the value of the `name` environment variable
specified in `Dockerfile` and `index.js`,
add "`-e name={value}`" to the `docker run` command.
For example, `docker run -t --rm -e name=Mark node-demo`

### Volume Example

Now let's create an image that writes and reads files on the host.

Here is the `package.json` file.
This example has no dependencies.

```json
{
  "name": "volume-demo",
  "version": "1.0.0",
  "description": "demonstrates reading and writing text files",
  "main": "index.js",
  "scripts": {
    "start": "node index.js"
  },
  "license": "MIT"
}
```

Here is the JavaScript code from the file `index.js`.

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

Here is the Dockerfile that describes the image to be built.

```docker
FROM node:7.4-onbuild
CMD ["npm", "start"]
```

To build the Docker image enter: `docker build -t volume-demo .`
To run this image,
enter: `docker run --rm -v $HOME/data:/data volume-demo`<br>
The `-v` option tells Docker to map the container directory `/data`
to the host directory `data` under your home directory.

After running this, a file named `test.txt` will exist
in `$HOME/data` directory and it will contain
"`Hello, World!`".

### Express Example

Now let's create an image that runs an Express HTTP server using Node.js.

Here is the `package.json` file that describes dependencies.
It also specifies a `script` for running the application.

```json
{
  "name": "express-demo",
  "version": "1.0.0",
  "description": "a Hello World Express app",
  "main": "index.js",
  "scripts": {
    "start": "node index.js"
  },
  "license": "MIT",
  "dependencies": {
    "express": "^4.14.0"
  }
}
```

Here is the JavaScript code from the file `index.js`.

```js
const express = require('express');

const app = express();

app.get('/hello', (req, res) => {
  const name = req.query.name || 'World';
  res.send(`Hello, ${name}!`);
});

const port = 3000;
app.listen(port, () => console.log('listening on port', port));
```

To run this Node application outside of Docker
enter: `npm start`<br>
and browse `localhost:3000/hello?name={your-first-name}`.

Here is the `Dockerfile` that describes the image to be built.

```docker
FROM node:7.4-onbuild
EXPOSE 3000
```

To build this image enter: `docker build -t express-demo .`

When a container exposes ports using the EXPOSE instruction,
these ports are only available to processes running in the container.
To use them outside of the container (in the host),
they must be mapped to a host port.
This is specified with the `--publish` or `-p` option.
For example, `-p 3001:3000` specifies that port 3000
inside the container is mapped to the host port 3001.
The same port number can be used for both if
the container port isn't already in use on the host.

To run this image in a container enter:
`docker run -d -p 3000:3000 express-demo`<br>
The `-d` option causes the container
to run in the background (detached mode).
Do not include the `--rm` option when using this.

To output log messages from container enter:
`docker logs {container-id-prefix}`<br>
This is a great way to diagnose issues.

When a container is not running in detached mode, it cannot be
stopped by pressing ctrl-c in the terminal where it is running.
To stop a running container enter:
`docker stop {container-id-prefix}`

The docker info command outputs an overview of the current Docker environment
including the operating system (ex. Ubuntu), provider (ex. amazonec2),
number of images, number of containers, and how many are running and stopped.

## Amazon EC2

From the Amazon EC2 website,
"Amazon Elastic Compute Cloud (Amazon EC2) is a web service
that provides resizable compute capacity in the cloud.
It is designed to make web-scale cloud computing easier for developers."

In the next section we will walk through the steps to
run a Docker image in a container within an EC2 instance.
To prepare for that, this section provides the steps
to create a free EC2 account.
Skip the rest of this section if you already have an EC2 account.

- Browse <a href="https://amazon.com/ec2" target="_blank">https://amazon.com/ec2</a>
- Click the "Create an AWS Account" button.
- Enter your email address.
- Select the "I am a new user" radio button.
- Press the "Sign in using our secure server" button.
- Enter the requested information.
- Press the "Create account" button.
- Enter contact information and check the "AWS Customer Agreement" checkbox.
- Press the "Create Account and Continue" button.
- Enter credit card information. You will only be charged if
  your usage exceeds certain limits after the trial period ends.
- Press the "Continue" button.
- Enter your telephone number.
- Press the "Call Me Now" button.
- You will receive a phone call. On your phone,
  enter the PIN number that is displayed in the browser.
- Press the "Continue to select your Support Plan" button.
- Select the "Basic" radio button.
- Press the "Continue" button.

The next step is to get access and secret keys.

- From the "My Account" menu in the upper-right, select "Security Credentials".
- Sign in using your new account.
- Press the "Continue to Security Credentials" button.
- Click "Access Keys (Access Key ID and Secret Access Key)".
- Press the "Create New Access Key" button.
- Press the "Download New Key" button.
- This downloads a file named "rootkey.csv" in the Downloads directory.
  It contains your access key and secret key. Move this file to a place
  you'll remember to look for it later, like in a directory named "Amazon-AWS".

## Docker Machine

Docker Machine makes it easy to use Docker in cloud provider instances.
We'll walk through the steps to create an instance on Amazon EC2.
This section assumes you already have an AWS account.

- Install Docker Machine by following the instructions at
  <a href="https://docs.docker.com/machine/install-machine"
  target="_blank">https://docs.docker.com/machine/install-machine</a>.

- Create a Docker-aware Amazon EC2 instance by entering the following commands.
  We will use the instance name "aws-sandbox".
  That should be substituted in place of
  all occurrences of {instance-name} below.

  ```bash
  docker-machine create --driver amazonec2 \<br>
    --amazonec2-access-key={access-key} \<br>
    --amazonec2-secret-key={secret-key} \<br>
    --amazonec2-region={region} \<br>
    {instance-name}
  ```

  The values for `access-key` and `secret-key`
  can be obtained from the `rootkey.csv` file
  that was downloaded in the previous section.
  The value for region will be similar to "us-west-2".</li>
  This command populates the
  `~/.docker/machine/machines/{instance-name}` directory
  with several `.pem` files and more.
  It will take several minutes to complete.

- Enter: `eval $(docker-machine env {instance-name})`<br>
  This sets four Docker-related environment variables that cause
  subsequent "`docker`" commands to run on the AWS instance
  instead of locally.
  The environment variables are `DOCKER_CERT_PATH`,
  `DOCKER_HOST`, `DOCKER_MACHINE_NAME`,
  and `DOCKER_TLS_VERIFY`.
  Unset these environment variables later
  to return to using Docker locally.

To list the current Docker Machine instances enter:
`docker-machine ls`

To get the IP address of an instance enter:
`docker-machine ip {instance-name}`

Use the normal docker commands to build images
and run them in containers within the AWS instance.
For example, to build and run our `express-demo`,
cd to its directory and enter these commands:<br>
`docker build -t express-demo .`<br>
`docker run -d -p 3000:3000 express-demo`<br>

To test this app running in the AWS instance,
browse `http://{instance-ip-address}:3000/hello`.
TODO: Why doesn't the server respond?
You should also be able to test this by running<br>
`docker run -d -p 8000:80 --name webserver kitematic/hello-world-nginx`<br>
and browsing {ip-address}:8000, but that doesn't work either!

To stop a running instance, enter `docker-machine stop {instance-name}`

To restart a running instance, enter `docker-machine restart {instance-name}`

To remove a stopped instance, enter `docker-machine remove {instance-name}`

To ssh into an instance, enter `docker-machine ssh {instance-name}`

TODO: Is there a good reason to create an IAM user and install the aws CLI?

## Dockerfile Contents

Dockerfiles contain instructions that describe an image to be built.
Each instruction is on a separate line.
They start with an instruction name followed by its arguments.
Instruction names are not case sensitive, but
the convention is for them to be all uppercase.
To spread arguments over several lines,
end all but the last line with a backslash like in Bash.
Comments are lines that begin with `#`.
Comments cannot appear at the ends of instructions.

## Dockerfile Instructions

Each instruction runs independently in a new image created just for it,
or ones from previous builds that are in the "build cache".
Previous commands do not affect subsequent ones.
For example, "RUN cd /usr/local" will not change
the working directory used by the next instruction.

### FROM

This specifies the base image.
It is required and MUST be the first instruction.

### ENV

This instruction sets the value of an environment variable
that is used inside the Dockerfile.
It cannot be used to change the value
of a previously set environment variable.
For example, to set ANIMAL to GIRAFFE,

```docker
ENV animal giraffe
```

To refer to this enviroment variable in another instruction,
use `${ANIMAL}` or `$ANIMAL`.

Instructions that support references to environment variables include:
`ADD`, `COPY`, `ENV`, `EXPOSE`, `LABEL`, `ONBUILD`, `STOPSIGNAL`,
`USER`, `VOLUME`, and `WORKDIR`.

### MAINTAINER

TODO: Add this detail.

### WORKDIR

This sets the default working directory from which commands will execute.
It defaults to "/".

### RUN

TODO: Add this detail.

### CMD

TODO: Add this detail.

### PATH

This specifies a local filesystem path that will be in the context of the image.
This should expose only the files needed by the image.
For example, do not use /.
A .dockerignore file can be added to any directory
to specify files that should not be exposed to the image.

### URL

This specifies a Github repository
whose files will be in the context of the image.

### COPY

TODO: Add this detail.

### WORKPATH

## Images

### Creating Images

To build an image from a Dockerfile:

```bash
docker build [-t {tag}] {Dockerfile-path}
```

While the -t option can be omitted, it is preferable include it.
Otherwise the image can only be identified by
the id that is automatically assigned.
A tag consists of a name and
an optional version separated from the name by a colon.
For example, `my-project:1.2`

Typically the "docker build" command is run
from the directory that contains Dockerfile.
When that is the case, the value for Dockerfile-path
can be a period representing the current directory.

If the base image identified in Dockerfile does not already
exist locally, its Dockerfile is downloaded from
<a href="https://hub.docker.com/" target="_blank">Docker Hub</a>.
Docker Hub is a repository of shared Docker images.
Next, the base image built.
The time required to do this depends on the base image,
but it is typically several minutes.
Subsequent builds of your images that use these base images
are much faster because locally installed base images are reused.

Examples of commonly used base images include
bash and node. ADD MORE!

### Listing Images

To get a list of all exising images:

```bash
docker images
```

### Tagging Images

Images have unique ids which are SHA values.
These are long and difficult to type.
Tags are human readable aliases for image ids.
Any number of tags can be added to the same image.
To add a tag to an image:

```bash
docker tag {id-or-existing-name} {new-name}
```

### Deleting Images

To delete an image:

```bash
docker rmi [-f] {image-id-prefix-or-tag}
```

If an image has a tag other than "latest",
deleting it requires specifying both the name and tag like this:

MISSING CONTENT?

If the image is used by an existing container,
an error message explaining this will be output.
The image can be deleted anyway if the -f option is used.
Containers whose images have been deleted can still be rerun
because all images it requires were copied into it.

```bash
docker rmi [-f] {name}:{tag}
```

There are multiple versions of an image, eash with a different tag,
deleting one by its id, requires using the -f option.

Deleting a tag only deletes the corresponding image
if it is the only tag for that image. ???

To delete all images, enter `docker rmi -f $(docker images -q)`

### Dangling Images

During iterative development it is common
to rebuild an image multiple times.
When an image with the same name as an existing one is built,
the previous version remains but is stripped of its name and tag.
These are referred to as "dangling images".
The following command deletes all of them:

```bash
docker rmi $(docker images -qa -f "dangling=true")
```

This uses two docker commands.
First, "docker images" is used to get a list of the ids
of all the dangling images.
Second, "docker rmi" is used to delete those images.
Why isn't there a build option to do this automatically?

## Containers

A container is a running instance of an image.

### Creating/Running Containers

To list the current running containers,
enter: `docker ps`

To list all containers including those that are stopped,
enter: `docker ps -a`.

To see details about an existing container,
enter: `docker inspect {container-id}`.

To restart a container whose image is currently running,
enter `docker restart {container-id}`

To rerun a container whose image has exited,
enter `docker start -a {container-id}`.
This works even if the images used by the container have been deleted
because the images have already been copied into the container.

### Deleting Containers

To delete specific containers, enter `docker rm {container-ids}`.
A space-separated list of container id prefixes can be specified.
Each id prefix must uniquely match a single container.

To delete all containers, enter `docker rm $(docker ps -qa)`.

### Opening a shell inside a running container

To interact with a running container,
open a shell inside it where commands can be entered.

```bash
docker exec -it {container-id} /bin/sh
```

To delete all containers including those that are running,
enter `docker rm -f $(docker ps -qa)`

## Volumes

Containers can also provide read-write access to the read-only image filesystem
a "Union File System".
Changes to files happens in the container, but are not saved in the image.
"Volumes" allow these changes to be persisted to the local filesystem.

To specify a volume to be used by a container,
use the -v option of the docker run command as follows:

```bash
docker run {image-name} -v $(pwd):{directory}
```

It should be possible to edit files under a volume directory
and have the image in the container utilize the changes
without restarted it. TRY THIS!
See https://dzone.com/articles/docker-for-devs-creating-a-developer-image?edition=264895&utm_source=Spotlight&utm_medium=email&utm_campaign=cloud%202017-01-24

### Sharing an Image

The easiest way to share a Docker image is to share its Dockerfile.
The recipient can build this image from that.

Another alternative is to register for a
<a href="https://hub.docker.com/" target="_blank">Docker Hub</a>
account and upload your image there.
From here you can browse for shared, downloadable images.
For example, docker/whalesay.

## Unorganized Content

- Docker Engine: supports sharing Docker images
- docker run docker/whalesay cowsay Hello from the whale!

To start Docker on Mac OS X

- if there is no whale icon in the
  status bar at the top of the screen
  - open Launchpad or Finder/Applications
  - click Docker icon (whale)
  - should see Docker icon in status bar at top
- if there is a whale icon
  - click it a verify that the dropdown says "Docker is running"

To set Docker preferences

- click the whale icon in the status bar
- select "Preferences"
- General options include:
  - "Automatically start Docker when you log in" (on by default)
  - "Automatically check for updates" (on by default)
  - "Exclude VM from Time Machine backups" (off by default)
  - number of CPUs
  - amount of memory allocated
- can configure proxies
- can configure file sharing with the host machine
- can initiate uninstall

Kitematic

- a graphical UI for Docker
- can be installed from the Docker whale menu

To determine if Docker is running and see other info

- docker info

Images

- to define
  - create a new directory and cd to it
  - create a file name "Dockerfile"
  - comments in this file begin with #
  - first instruction must be "FROM {image-name}[:tag]"
    to specify the base image - tag defaults to "latest",
    but can specify a different version - the smallest, simplest base image is "scratch",
    but that doesn't seem to be able to do anything - another good option is "bash" - official Node base images are described at
    https://hub.docker.com/_/node/
    _ specify npm packages to be installed in package.json
    _ run a command to install PostgreSQL?
    IMPORTANT! \* SEE https://hub.docker.com/r/spittet/node-postgresql/ - official PostgreSQL base images are described at
    https://hub.docker.com/_/postgres/ - there doesn't seem to be an offical base image
    that includes Express
  - RUN command
    - executes command(s) in a new layer
      and creates a new image
    - often used to install software packages
    - to install software in the image, add lines like this:
      RUN apt-get -y update && apt-get install -y {software-name}
  - CMD command
    - sets the default command and/or parameters
    - can be overwritten from command line when running
    - only the last CMD command is used,
      so there is no point in having more than one
    - typically used to start a server
    - to run a command, add the line "CMD {command}"
  - ENTRYPOINT command
    - configures a container that will run as an executable
  - RUN, CMD, and ENTRYPOINT can be used in "shell" or "exec" form
    - in shell form the command is
      executed with "/bin/sh -c {command}"
      - ex. CMD echo "Hello, World!"
    - in exec form an executable is
      executed directly with no shell processing
      - ex. CMD ["/bin/echo", "Hello, World!"]
      - exec form is preferred for the CMD and ENTRYPOINT commands
  - save the file
- to SSH into a Docker container
  - run `docker ps` to get the container id
  - run `docker exec -it {container-id} /bin/bash
- to build an image

* docker build [-t {name[:tag]}] {Dockerfile-path}
  - if -t option is omitted,
    the image will have no name (a.k.a. repository)
    or tag, just an id
  - Dockerfile-path can be . for the current directory
  - also downloads and builds image for the base image
    if it doesn't already exist
    - ex. bash and node
    - can take a long time, but subsequent builds are
      much faster because the base image
      is already downloaded and built
  - if you build an image with the same name
    as an existing image, it will not be replaced,
    but it will lose its name so
    the name can be assigned to the new image
    - referred to as a "dangling image"
    - can delete it by its id or with this command:
      docker rmi $(docker images -qa -f "dangling=true")
    - why isn't there a build option to do this automatically?

- to list existing images
  - docker images
- to remove an image
  - docker rmi {image-name-or-id}
    - can use shortest length of image id that is unique
  - not allowed if image is currently running in a container

Containers

- to list
  - only running: docker ps
  - all: docker ps -a
  - only latest created: docker ps -l
- to create, but not start: docker create
- to create and start: docker run {image-name}
  - docker run {image-name}
    - options
      -i : is for interactive (can read from stdin)
      -t : allocates a pseudo TTY
      --rm : removes container after image exits
      --name {container-name} : gives name to container
  - if the image isn't found locally,
    this attempts to download it from "Docker Hub"
    - download images are saved locally for future use
    - can remove when no longer needed
  - every time this is run, a new container is created
  - after this completes, the new container remains
    unless the --rm option is used
    - I think you want this for non-server images
- to stop and restart a running container:
  docker restart {container-id}
- to rename: docker rename {container-id} {new-name}
- to delete: docker rm {container-id}
  - can use shortest length of container id
    that is unique
- containers are process-centric

  - once the image inside it exits, the container is done
  - so why does it still exist?
    - may to allow view its logs

- to stop a running container
  - docker stop {container-id}
- to output logs from a running container
  - docker logs {container-name}
- to show processes running inside a running container
  - docker top {container-name}
- to run a command inside a running container
  - docker exec {container-name} {command}
  - one use is to inspect the contents of a database
- to copy a file into a running container
- docker cp {file-path} {container-name}:/{new-file-name}
- to open a bash shell running inside a running container

  - docker exec -it {container-name} /bin/bash
  - useful to poke around at the state

- to get information about an image or container
  - docker inspect {name-or-id}
    - Monsanto container-name is "capacity-api"

Docker Compose

- a tool for defining multi-container applications
- Does Monsanto use this?
- to start an image
  - How is this different than loading into a container?
  - cd to a directory containing docker-compose.yml
  - docker-compose up -d
    - -d (daemon) avoids taking over the current process
- to stop an image
  - cd to a directory containing docker-compose.yml
  - docker-compose down
