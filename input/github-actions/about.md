---
css: '/blog/assets/github-actions.css'
eleventyNavigation:
  key: About
  parent: GitHub Actions
layout: layout.njk
---

GitHub Actions enable registering executing code on a cloud server
in response to GitHub commands completing.
For example, an action could build an application after every push.
This can include running linters, code formatters, and tests.
Executed actions and their output appear in the
"Actions" tab of the GitHub repository.

## Configuring Actions

Actions for a repository are configured by YAML files
in the `.github/workflows` directory.
Here is a simple example defined in a file named `demo.yml`.
It uses an action defined at
<https://github.com/actions/hello-world-javascript-action>.

```yaml
name: My Demo
on: [push]
jobs:
  build:
    name: DemoJob
    runs-on: ubuntu-latest
    steps:
      - name: Hello
        id: hello
        uses: actions/hello-world-javascript-action@master
        with:
          who-to-greet: 'Mark Volkmann'
      - name: Time
        run: echo 'The time was ${{ steps.hello.outputs.time }}.'
```

This executes on every push to the repository.

TODO: What properties can be in `jobs:` besides `build:`?

For workflows that will run on a GitHub-hosted server,
the operating system of the cloud server can be specified.
Options include `ubuntu-latest`, `windows-latest`, and `macos-latest`.
A specific operating system version can be specified instead of "latest".
The supported versions in include
`ubuntu-18.04`, `ubuntu-16.04`, `windows-2019`, and `macos-10.15`.

Workflows can also be self-hosted.
An example of specifying this is:

```yaml
runs-on: [self-hosted, linux]
```

Each step is defined by a number of properties.

| Property Name | Meaning                                                          |
| ------------- | ---------------------------------------------------------------- |
| `name`        | step name that appears in the web UI that shows workflow results |
| `uses`        | a predefined action to use                                       |
| `with`        | arguments to pass to the action                                  |
| `id`          | name that will be used to refer to action result properties      |

The property with the name specified by `id`
is an object with an `outputs` property
that is an object that holds all the output values.
Actions typically document their outputs.
In this case the only output is a `time` property
which is the time at which the action executed.

## Viewing Action Results

The "Actions" tab of a GitHub repository is shown below.

![GitHub Actions web UI #1](/blog/assets/github-actions-web-ui-1.png)

The workflows that have been executed can be filtered on
the event that triggered them (ex. "push"),
their status (ex. "success" or "failure"),
the targeted branch,
and the GitHub user name that triggered it.

Clicking a workflow row displays details about its execution.

![GitHub Actions web UI #1](/blog/assets/github-actions-web-ui-2.png)

Click a job name (ex. "DemoJob") in the left nav to see details.
This displays information about the steps
"Set up job", "Complete Job", and
each named step in the workflow, "Hello" and "Time" in this example.
Click the disclosure triangle in front of a step name
to see its detail.

Here we see the "Set up job" and "Hello" steps expanded.

![GitHub Actions web UI #1](/blog/assets/github-actions-web-ui-3.png)

The "Set up job" step shows the operating system that was used
and actions that were downloaded (ex. "hello-world-javascript-action").

Here we see the "Time" and "Complete Job" steps expanded.
In this example the "Time" step shows the
time at which the "Hello" step was executed.

![GitHub Actions web UI #2](/blog/assets/github-actions-web-ui-4.png)

## Executing Shell Commands

GitHub Actions do not have to use an existing action
such "hello-world-javascript-action".
They can also execute shell commands.
Here is an example workflow file that does this.

```yaml

```

## Defining Actions
