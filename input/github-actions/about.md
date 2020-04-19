---
css: '/blog/assets/github-actions.css'
eleventyNavigation:
  key: About
  parent: GitHub Actions
layout: layout.njk
---

GitHub Actions enable registering "jobs" to run on a cloud server
in response to GitHub commands completing.
The cloud server must have the GitHub Actions runner application is installed.
GitHub provides these servers for free,
but it is also possible to use your on servers.

For example, an workflow can build an application after every push.
This can include running linters, code formatters, and tests.
Executed workflows and their output appear in the
"Actions" tab of the GitHub repository.

A "workflow" defines a set of jobs using a YAML file.
A job defines a set up steps
to run in a given environment (ex. `ubuntu-latest');
A step is a single task runs a predefined action or a shell command.

## Configuring Workflows

Workflows for a repository are configured by YAML files
in the `.github/workflows` directory.
Here is a simple example defined in a file named `demo.yml`.
It uses an action defined at
<https://github.com/actions/hello-world-javascript-action>.

```yaml
name: My Demo
on: [push]
jobs:
  build: # a job id
    name: DemoJob # a job name
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

The `on` property can be set to one webhook event name or an array of them.
There are many webhook events that can trigger a workflow to run.
These are documented at
<https://help.github.com/en/actions/reference/events-that-trigger-workflows>.
Some events are triggered by more than one kind of activity.
When this is the case, a particular activity type can be specified.

| Webhook Event               | Triggered By                                                                                                                                                                                                       |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| check_run                   | the "check runs" API that can check code in various ways (ex. linting) is invoked                                                                                                                                  |
| check_suite                 | a suite of check runs is executed                                                                                                                                                                                  |
| create                      | a branch or tag is created                                                                                                                                                                                         |
| delete                      | a branch or tag is deleted                                                                                                                                                                                         |
| deployment                  | a request to deploy a branch, SHA, or tag is received                                                                                                                                                              |
| deployment_status           | a deployment status is provided by an HTTP POST request to a GitHub API                                                                                                                                            |
| fork                        | a repository is forked                                                                                                                                                                                             |
| gollum                      | a wiki page is created or updated                                                                                                                                                                                  |
| issue_comment               | an issue comment is created, edited, or deleted                                                                                                                                                                    |
| issues                      | an issue is opened, edited, deleted, transferred, pinned, unpinned, closed, reopened, assigned, unassigned, labeled, unlabeled, locked, unlocked, milestoned, or demilestoned                                      |
| label                       | a label is created, edited, or deleted                                                                                                                                                                             |
| milestone                   | a milestone is created, closed, opened, edited, or deleted                                                                                                                                                         |
| page_build                  | a GitHub Pages-enabled branch is pushed                                                                                                                                                                            |
| project                     | a project within a repo is created, updated, closed, reopened, edited, or deleted (see the "Projects tab in a GitHub repo to manage project tasks)                                                                 |
| project_card                | a project card is created, moved, convered to an issue, edited, or deleted                                                                                                                                         |
| project_column              | a project column is created, updated, moved, or deleted                                                                                                                                                            |
| public                      | a private repo is changed to a pull request is opened, assigned, unassigned, labeled, unlabeled, closed, reopened, synchronize(d), ready_for_review, locked, unlocked, review_requested, or review_request_removed |
| pull_request                | TODO                                                                                                                                                                                                               |
| pull_request_review         | a pull request review is submitted, edited, or dismissed                                                                                                                                                           |
| pull_request_review_comment | a pull request review comment is created, edited, or deleted                                                                                                                                                       |
| push                        | a commit is pushed                                                                                                                                                                                                 |
| registry_package            | a registry package (?) is published or updated                                                                                                                                                                     |
| release                     | a release is created, published, unpublished, edited, prereleased, or deleted                                                                                                                                      |
| status                      | the status of a commit changes                                                                                                                                                                                     |
| watch                       | a user watches or stars the repository?                                                                                                                                                                            |

You can also create a "scheduled event" which
schedules a workflow to run at a certain time interval.

You can also trigger a workflow to run by creating a
`repository-dispatch` event that is created by
sending an HTTP POST request to a GitHub API endpoint.

For workflows that will run on a GitHub-hosted server (a.k.a. runner),
the operating system of the server can be specified.
This can be the latest version of a particular OS or a specific version.
Current options include:

- `ubuntu-latest`
- `windows-latest`
- `macos-latest`.
- `ubuntu-18.04`
- `ubuntu-16.04`
- `windows-2019`
- `macos-10.15`.

Workflows can also be self-hosted.
An example of specifying this is:

```yaml
runs-on: [self-hosted, linux]
```

In the example above, `build` is a job id.
TODO: Where are job ids displayed in the web UI?
This workflow only defines one job.
Defining multiple jobs is useful to allow some steps
to run on a server that uses a different operating system.

Each step (a.k.a. action) is defined by a number of properties.

| Property Name | Meaning                                                          |
| ------------- | ---------------------------------------------------------------- |
| `name`        | step name that appears in the web UI that shows workflow results |
| `run`         | a shell command to run                                           |
| `uses`        | a predefined action to use                                       |
| `with`        | arguments to pass to the action                                  |
| `id`          | name that will be used to refer to action result properties      |

Specify `run` or `uses`, but not both.
If a step does not have a `name` property,
a name is created from the value of `run` or `uses`.

The `id` property specifies a property name
whose value will be an object with an `outputs` property
that is an object that holds all the output values.
Actions typically document their outputs.
In the example above, the action creates an output named `time`
which can be accessed with `steps.hello.outputs.time`
because the value of `id` is `hello`.
The action sets this to the time at which it was executed.

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

## Workflow Templates

A workflow can be created from the GitHub web UI.
Click the "Actions" tab and press the "New workflow" button.
This presents a series of boxes that describe workflow templates.
Click the "Set up this workflow" button inside one of the boxes
to create a workflow based on that template.

Here is an example workflow file that does this.
For example, the "Node.js" template contains the following:

```yaml
name: Node.js CI

on:
  push:
    branches: [master]
  pull_request:
    branches: [master]

jobs:
  build:
    runs-on: ubuntu-latest

    strategy:
      matrix:
        node-version: [10.x, 12.x]

    steps:
      - uses: actions/checkout@v2
      - name: Use Node.js ${{ matrix.node-version }}
        uses: actions/setup-node@v1
        with:
          node-version: ${{ matrix.node-version }}
      - run: npm ci
      - run: npm run build --if-present
      - run: npm test
        env:
          CI: true
```

It offers to save this a `.github/workflows/nodejs.yml` in your repository.
You can customize the file name and the workflow definition if desired.
When ready to save it, press the "Start Commit" button in the upper-right.
A dialog will appear.
Optionally enter a commit comment and press the "Commit new file" button.
Do a "git pull" to get the new workflow file in your local repository.
The new workflow will be scheduled to run immediately.
Click the "Actions" tab to see the results.

Two events trigger this workflow to execute.
The first is a push to the master branch.
The second is a pull request to the master branch.

Setting `strategy.matrix.node-version` to an array of version numbers
causes it to execute the steps in each version of Node.
This is useful to run tests in multiple versions of Node.
To only use the latest version of version 12,
remove `strategy.matrix.node-version` and
change the `node-version` property for the `setup-node` step to just `12.x`.

This workflow runs the following commands:

- `actions/checkout@v2` is a predefined action that checks out
  the most recent commit to a given branch which defaults to "master".
  See <https://github.com/actions/checkout> for options.
- `actions/setup-node@v1` is a predefined action that
  sets the Node environment to be used by actions.
  It uses a version specified in `strategy.matrix.node-version`
  unless the `node-version` property is specified.
  When the matrix property specifies Node versions,
  the version used in a particular run becomes part of the step name.
  See <https://github.com/actions/setup-node> for options.
- `npm ci` does a clean install of all dependencies.
  It is similar to `npm install`, but differs in that
  - if the `node_modules` directory exists, it is deleted
  - `package-lock.json` must exist
  - it is an error if `package.json` specifies different
    dependencies or versions than `package-lock.json`
  - `package-lock.json` will not be updated
- `npm run build --if-present` runs the `build` script if it is defined,
  but it is not treated as an error if it is missing.
- `npm test` runs tests using the commands in the `test` script.
  This sets the `CI` environment variable to `true`
  which means that warnings from tests will be treated as errors
  and cause the build to fail.

TODO: Is there an issue with having more than one workflow file?
TODO: It seems only one gets executed.

## Executing Shell Commands

GitHub Actions do not have to use an existing action
such "hello-world-javascript-action".
They can also execute shell commands.

## Using Secrets

To use secret information like passwords and tokens in a workflow,
create GitHub secrets.
To add a secret to a GitHub repo:

- browse the GitHub repo
- click the "Settings" tab
- click "Secrets" in the left nav
- click "Add a new secret"
- for the name, enter "GH_TOKEN"
- for the value, paste in your GitHub personal access token
- click "Add secret"

Secrets can then be referenced by name in a workflow
with the syntax `${{ secrets.name }}`
where `name` is the secret name.
This is used in the following workflow.

## Building and Publishing an Eleventy Site

Here is a workflow file that builds and deploys an Eleventy site
on every push.

```yaml
name: Eleventy build and deploy
on:
  push:
    branches: [master]
jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - name: git checkout
        uses: actions/checkout@v2
      - name: Node.js setup
        uses: actions/setup-node@v1
        with:
          node-version: 12.x
      - name: npm clean install
        run: npm ci
      - name: site build
        run: npm run build
      - name: site deploy
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GH_TOKEN }}
          publish_dir: ./_site
```

For more details, see <https://github.com/peaceiris/actions-gh-pages>.

## Defining Actions

Content for this topic is coming soon!
