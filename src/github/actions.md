---
css: '/blog/assets/github-actions.css'
eleventyNavigation:
  key: GitHub Actions
  parent: GitHub
  title: Actions
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

### Overview

GitHub Actions enable registering "jobs" to run on a cloud server
in response to GitHub events.
The cloud server must have the GitHub Actions runner application is installed.
GitHub provides these servers for free,
but it is also possible to run jobs on your own servers.

- A "**workflow**" defines a set of jobs using a YAML file.
- A "**job**" defines a set up steps to run
  in given environments (ex. `ubuntu-latest`).
- A "**step**" is a single task that runs a predefined action or a shell command.

For example, a workflow can build an application
after every push to given branches.
This can include running linters, code formatters, and tests.
Executed workflows and their output appear in the
"Actions" tab of each GitHub repository.

A shell command can run a shell script or
a CLI command such as those provided by `npm`.
Examples include `npm install`, `npm run lint`, and `npm test`.

There are over 3,000 predefined actions to choose from,
cataloged at <https://github.com/actions>
and <https://github.com/marketplace?type=actions>.
Many of these are commercial,
but there are over 200 that have a free tier.

### Configuring Workflows

Workflows for a repository are configured by YAML files
in the `.github/workflows` directory.
Any number of workflows can be triggered by a single GitHub event.
For details on the syntax of these files, see
<https://help.github.com/en/actions/reference/workflow-syntax-for-github-actions>.

Here is a simple example defined in a file named `demo.yml`.
It uses an action defined at
<https://github.com/actions/hello-world-javascript-action>.
The code that implements actions is automatically retrieved
from their GitHub repositories.

{% raw %}

```yaml
name: My Demo # workflow name
on: push
jobs:
  build: # job id
    name: DemoJob # job name
    runs-on: ubuntu-latest
    steps:
      - name: Hello
        id: hello # used below to refer to output from this step
        uses: actions/hello-world-javascript-action@master
        with: # specifies arguments to the action
          who-to-greet: 'Mark Volkmann'
      - name: Time
        run: echo 'The time was ${{ steps.hello.outputs.time }}.'
```

{% endraw %}

This executes on every push to the repository on any branch.

The `on` property can be set to one webhook event name or an array of them.
There are many webhook events that can trigger a workflow to run.
These are documented at
<https://help.github.com/en/actions/reference/events-that-trigger-workflows>.
Some events are triggered by more than one kind of activity.
When this is the case, a particular activity type can be specified.

### Linting

To check a YAML file describing GitHub Actions for issues:

- enter `brew install actionlint`
- `cd` to the project root directory
- enter `actionlint`

### Workflow Jobs

In the workflow defined above, `build` is a job id.
This workflow defines a single job.
Defining multiple jobs is useful to allow some steps
to run on a server that uses a different operating system.

### Workflow Steps

Each step (a.k.a. action) within a job is defined by a number of properties.

| Property Name | Meaning                                                                   |
| ------------- | ------------------------------------------------------------------------- |
| `name`        | optional step name that appears in the web UI that shows workflow results |
| `run`         | a shell command to run                                                    |
| `uses`        | a predefined action to use                                                |
| `with`        | arguments to pass to the action                                           |
| `id`          | name that will be used to refer to action result properties               |
| `needs`       | step name (or array of them) that must completed before this step begins  |
| `if`          | for conditional execution                                                 |

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

### Supported Webhook Events

<table>
  <tr>
    <th>Webhook Event</th>
    <th>Triggered By</th>
  </tr>
  <tr>
    <td>check_run</td>
    <td>
      the "check runs" API is invoked;
      can check code in various ways (ex. linting)
    </td>
  </tr>
  <tr>
    <td>check_suite</td>
    <td>a suite of check runs is executed</td>
  </tr>
  <tr>
    <td class="bold">create</td>
    <td>a branch or tag is created</td>
  </tr>
  <tr>
    <td class="bold">delete</td>
    <td>a branch or tag is deleted</td>
  </tr>
  <tr>
    <td>deployment</td>
    <td>a request to deploy a branch, SHA, or tag is received</td>
  </tr>
  <tr>
    <td>deployment_status</td>
    <td>
      a deployment status is provided by an HTTP POST request to a GitHub API
    </td>
  </tr>
  <tr>
    <td class="bold">fork</td>
    <td>a repository is forked</td>
  </tr>
  <tr>
    <td class="bold">gollum</td>
    <td>a wiki page is created or updated</td>
  </tr>
  <tr>
    <td class="bold">issue_comment</td>
    <td>an issue comment is created, edited, or deleted</td>
  </tr>
  <tr>
    <td class="bold">issues</td>
    <td>
      an issue is opened, edited, deleted, transferred, pinned, unpinned,
      closed, reopened, assigned, unassigned, labeled, unlabeled,
      locked, unlocked, milestoned, or demilestoned
    </td>
  </tr>
  <tr>
    <td>label</td>
    <td>a label is created, edited, or deleted</td>
  </tr>
  <tr>
    <td>milestone</td>
    <td>a milestone is created, closed, opened, edited, or deleted</td>
  </tr>
  <tr>
    <td class="bold">page_build</td>
    <td>a GitHub Pages-enabled branch is pushed</td>
  </tr>
  <tr>
    <td class="bold">project</td>
    <td>
      a project within a repo is created, updated,
      closed, reopened, edited, or deleted
      (see the "Projects" tab in a GitHub repo to manage project tasks)
    </td>
  </tr>
  <tr>
    <td class="bold">project_card</td>
    <td>
      a project card is created, moved, converted to an issue,
      edited, or deleted
    </td>
  </tr>
  <tr>
    <td class="bold">project_column</td>
    <td>a project column is created, updated, moved, or deleted</td>
  </tr>
  <tr>
    <td>public</td>
    <td>a private repo is changed to public</td>
  </tr>
  <tr>
    <td class="bold">pull_request</td>
    <td>
      a pull request is opened, assigned, unassigned, labeled, unlabeled,
      edited, closed, reopened, synchronize(d), ready_for_review,
      locked, unlocked, review_requested, or review_request_removed
    </td>
  </tr>
  <tr>
    <td class="bold">pull_request_review</td>
    <td>a pull request review is submitted, edited, or dismissed</td>
  </tr>
  <tr>
    <td class="bold">pull_request_review_comment</td>
    <td>a pull request review comment is created, edited, or deleted</td>
  </tr>
  <tr>
    <td class="bold">push</td>
    <td>a commit is pushed</td>
  </tr>
  <tr>
    <td>registry_package</td>
    <td>a registry package (npm alternative) is published or updated</td>
  </tr>
  <tr>
    <td class="bold">release</td>
    <td>
      a release is created, published, unpublished, edited,
      prereleased, or deleted
    </td>
  </tr>
  <tr>
    <td>status</td>
    <td>the status of a commit changes</td>
  </tr>
  <tr>
    <td class="bold">watch</td>
    <td>a user watches or stars the repository</td>
  </tr>
</table>

### Scheduled Workflows

A workflow can be scheduled to run at a certain time interval
by defining a "scheduled event".
For example, adding the following in a workflow YAML file
causes the jobs that follow it to run every five minutes:

```yaml
on:
  schedule:
    - cron: '*/5 * * * *'
```

The parts of the `cron` value, in order, are
minutes (0-59), hours (0-23), day of month (1-31),
month (1-12), and day of week (0-6).
An asterisk is treated as a wildcard character, allowing any value.
Specifying a minute value with `*/n` means to run at every nth interval,
so `*/5` runs every five minutes.

### Manually Triggered Workflows

A workflow can be triggered to run by dispatching a `repository_dispatch` event.
This is done by sending an HTTP POST request to a GitHub API endpoint.

Define the events that should trigger a workflow
in a workflow YAML file as follows:

```yaml
on:
  repository_dispatch:
    types: [start-my-workflow]
```

For details on sending a POST request for the specified event type
in order to trigger the workflow, see
<https://developer.github.com/v3/repos/#create-a-repository-dispatch-event> and
<https://dev.to/teamhive/triggering-github-actions-using-repository-dispatches-39d1>.
These describe the required request headers
(including an `Authorization` header containing a personal access token)
and body.

### Operating System

For workflows that will run on a GitHub-hosted server (a.k.a. runner),
the operating system of the server can be specified.
This can be the latest version of a particular OS or a specific version.
Current options include:

- `ubuntu-latest`
- `windows-latest`
- `macos-latest`
- `ubuntu-18.04`
- `ubuntu-16.04`
- `windows-2019`
- `macos-10.15`

Workflows can also be self-hosted.
An example of specifying this is:

```yaml
runs-on: [self-hosted, linux]
```

### Viewing Action Results

After a workflow is triggered, click the "Actions" tab of the
GitHub repository to refresh the list of triggered workflows.
This can be done repeatedly until the workflow appears.

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
"Set up job", "Complete job", and
each named step in the workflow, "Hello" and "Time" in this example.
Click the disclosure triangle in front of a step name
to see its detail.

The "Set up job" step sets up
the cloud environment where the workflow will execute and
downloads all predefined actions that will executed.

The "Complete job" step tears down the cloud environment,
stopping any processes that were started.

Here we see the "Set up job" and "Hello" steps expanded.

![GitHub Actions web UI #1](/blog/assets/github-actions-web-ui-3.png)

The "Set up job" step shows the operating system that was used
and actions that were downloaded (ex. "hello-world-javascript-action").

Next, we see the "Time" and "Complete Job" steps expanded.
In this example the "Time" step shows the
time at which the "Hello" step was executed.

![GitHub Actions web UI #2](/blog/assets/github-actions-web-ui-4.png)

### Workflow Templates

A workflow can be created from the GitHub web UI.
Click the "Actions" tab and press the "New workflow" button.
This presents a series of boxes that describe workflow templates.
Click the "Set up this workflow" button inside one of the boxes
to create a workflow based on that template.

Here is an example workflow file created from the "Node.js" template:

{% raw %}

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

{% endraw %}

After clicking the button, it offers to save this workflow file
in `.github/workflows/nodejs.yml` within your repository.
You can customize the file name and the workflow definition if desired.

When ready to save it, press the "Start Commit" button in the upper-right.
A dialog will appear.
Optionally enter a commit comment and press the "Commit new file" button.
Do a "git pull" to get the new workflow file in your local repository.
The new workflow will be scheduled to run immediately.
Click the "Actions" tab to see the results.

Two events trigger this workflow to execute.
The first is a push to the master branch.
The second is creating a pull request on the master branch.

{% raw %}
Setting `strategy.matrix.node-version` to an array of version numbers
causes it to execute the steps in each listed version of Node.
To only use the latest version starting with 12,
remove `strategy.matrix.node-version` and
change the `node-version` property for the `setup-node` step
to `12.x` instead of `${{ matrix.node-version }}`.
{% endraw %}

This workflow runs the following commands:

- `actions/checkout@v2` is a predefined action that checks out
  the most recent commit to a given branch which defaults to "master".
  See {% aTargetBlank 'https://github.com/actions/checkout', 'checkout' %}
  for options.
- `actions/setup-node@v1` is a predefined action that sets the Node environment.
  It uses a version specified in `strategy.matrix.node-version`
  unless the `node-version` property is specified.
  When the matrix property specifies Node versions,
  the version used in a particular run becomes part of the step name.
  See {% aTargetBlank 'https://github.com/actions/setup-node', 'setup-node' %}
  for options.
- `npm ci` does a clean install of all dependencies.
  It is similar to `npm install`, but differs the following ways:
  - If the `node_modules` directory exists, it is deleted.
  - The file `package-lock.json` must exist.
  - It is an error if `package.json` specifies different
    dependencies or versions than `package-lock.json`.
  - The `package-lock.json` file is not be updated.
- `npm run build --if-present` runs the `build` script if it is defined,
  but it is not treated as an error if it is missing.
- `npm test` runs tests using the commands in the `test` script.
  This sets the `CI` environment variable to `true`
  which means that warnings from tests will be treated as errors
  and cause the build to fail.

### Popular Predefined Actions

The following action checks out the repository in the cloud environment.

- `actions/checkout@v2`  
  This is used by many predefined workflows.

The following actions setup the environment
to use a particular programming language.

- `actions/setup-go@v1`  
  This is used by the "Go" workflow.

- `actions/setup-java@v1`  
  This is used by the "Java with Gradle" and "Java with Maven" workflows.

- `actions/setup-node@v1`  
  This is used by the "Node.js" and "Publish Node.js package" workflows.

- `actions/setup-python@v1`  
  This is used by the "Python application", "Python package",
  and "Publish Python package" workflows.

The following actions are used by the "Deploy to Amazon ECS" workflow for AWS.

- `aws-actions/configure-aws-credentials@v1`
- `aws-actions/amazon-ecr-login@v1`
- `aws-actions/amazon-ecs-render-task-definition@v1`
- `aws-actions/amazon-ecs-deploy-task-definition@v1`

The following action is used by the "Build and Deploy to GKE" workflow for GCP.
GKE stands for "Google Kubernetes Engine".
GCP stands for "Google Cloud Platform".

- `GoogleCloudPlatform/github-actions/setup-gcloud@master`

### Multi-line Steps

In this example, there are multiple, consecutive steps
that each run one command.

```yaml
- run: npm ci
- run: npm run lint
- run: npm run format
- run: npm run build
```

Alternatively, this can be written as a single step with multiple commands
as follows:

```yaml
- run: |
    npm ci
    npm run lint
    npm run format
    npm run build
```

### Workflow Errors

If a workflow step results in an error,
subsequent steps will not be executed.
For example, this could happen if there is
a code linting error, a compiler error, or a test failure.

If there are any errors, the repository owner
will receive an email with the subject
"[{username}/{repo-name}] Run failed: {workflow-name} - {branch-name}".
The email will include two links that can be clicked to see the results.
The link containing the failed job name is more informative
than the link containing "View results".

### Using Secrets

To use secret information like passwords and tokens in a workflow,
create GitHub secrets.

To add a secret to a GitHub repo:

- Browse the GitHub repo.
- Click the "Settings" tab.
- Click "Secrets" in the left nav.
- Click "Add a new secret".
- Enter a name (ex. GH_TOKEN).
- Enter a value (ex. paste your GitHub personal access token).
- Click "Add secret".

{% raw %}

Secrets can then be referenced by name in a workflow
with the syntax `${{ secrets.name }}`
where `name` is the secret name.
This is used in the workflow in the next section.

{% endraw %}

### Building and Publishing an Eleventy Site

Here is a workflow file that builds and deploys an Eleventy site
on every push.

{% raw %}

```yaml
name: Eleventy build and deploy
on:
  push:
    branches: [master]
jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions/setup-node@v1
        with:
          node-version: 14.x
      - run: npm ci
      - run: npm run lint
      - run: npm run format
      - run: npm run build
      - name: site deploy
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GH_TOKEN }}
          publish_dir: ./_site
```

{% endraw %}

For more details on the "site deploy" step,
see <https://github.com/peaceiris/actions-gh-pages>.

### Manually Triggering a Workflow

Currently there is no way to manually
trigger a workflow from the GitHub web UI.
It is a frequently requested feature, as indicated at
{% aTargetBlank
  'https://github.community/t5/GitHub-Actions/GitHub-Actions-Manual-Trigger-Approvals/td-p/31504',
  'feature request'
%}.

However, workarounds are possible.
One workaround is to configure a workflow to be triggered
when the repository is starred.
This is done by changing the `on` property
at the top of a workflow file to the following:

```json
on:
  push: # existing event
    branches: [master]
  watch: # newly added event
    types: [started] # triggered by starring the repo
    # Why is the event name "started"?  Looks like a typo.
```

### Output with echo

When running in a Linux environment,
a step can use the `echo` command to produce output.
For example:

```yaml
- name: hello world
  run: echo "Hello, World!"
```

To see the output, browse the GitHub repository,
click the "Actions" tab, select the workflow,
select the job, and click the disclosure triangle for the step.
If the step has no disclosure triangle,
click the ellipsis in the upper-right and select "View raw logs".

### Shell Scripts

A step can execute a shell script that exists in the repository.
For example:

```yaml
- name: run shell script
  run: . ./bin/example.sh
```

### Contexts

A large amount of context information is available in workflows.
It is stored in the following context objects:
`env`, `github`, `job`, `matrix`, `needs`,
`runner`, `secrets`, `steps`, and `strategy`.
To learn more about these, see
{% aTargetBlank
  'https://help.github.com/en/actions/reference/context-and-expression-syntax-for-github-actions#contexts',
  'contexts'
%}.

To output the data in a particular context in a step,
use the `toJson` function to put the JSON representation of the object
in an environment variable
and then use an `echo` shell command to output it.
For example:

{% raw %}

```yaml
- name: Dump GitHub context
  env:
    GITHUB_CONTEXT: ${{ toJson(github) }}
  run: echo "$GITHUB_CONTEXT"
```

{% endraw %}

### Sending Email

A step can send an email.
For example, this workflow sends an email
when a project board in the repository changes.

{% raw %}

```yaml
name: GitHub Project changes
on: [project, project_card, project_column]
jobs:
  project-change:
    runs-on: ubuntu-latest
    steps:
      - name: dump GitHub context to discover available data
        env:
          GITHUB_CONTEXT: ${{ toJson(github) }}
        run: echo "$GITHUB_CONTEXT"
      - name: email summary
        id: projectChange
        uses: dawidd6/action-send-mail@v2
        with:
          server_address: smtp.gmail.com
          server_port: 465
          username: ${{secrets.MAIL_USERNAME}}
          password: ${{secrets.MAIL_PASSWORD}}
          subject: blog project change
          body: >
            A change was detected in the
            <a href="https://github.com/mvolkmann/blog/projects/3">project board</a>
            of the GitHub repository ${{github.repository}}.
            <br>
            updated by ${{ github.actor }}
            at ${{ github.event.project_card.updated_at }}
            <br>
            event name = ${{ github.event_name }}
            <br>
            event action = ${{ github.event.action}}
            <br>
            event changes = ${{ toJson(github.event.changes) }}
            <br>
            card note = ${{ github.event.project_card.note }}
          to: r.mark.volkmann@gmail.com
          from: Mark Volkmann
          content_type: text/html
```

{% endraw %}

This requires adding secrets with the names `MAIL_USERNAME` and `MAIL_PASSWORD`.
These steps assume that Gmail is being used.

1. Browse <https://myaccount.google.com/>.
1. Click "Security" in the left nav.
1. In the "Signing in to Google" panel, click "App passwords".
1. In the "Select app" drop-down at the bottom, select "Mail".
1. In the "Select device" drop-down, select your current device type.
1. Press the "GENERATE" button.
1. Copy the displayed password.
1. Browse the GitHub repository for the workflow being created.
1. Click the "Settings" tab.
1. Click "Secrets" in the left nav.
1. Click "Add a new secret".
1. Enter "MAIL_USERNAME" for the name.
1. Enter your Google username.
1. Press the "Add secret" button.
1. Click "Add a new secret".
1. Enter "MAIL_PASSWORD" for the name.
1. Paste the generated password for the value.
1. Press the "Add secret" button.

For more detail in the `action-send-mail` action,
see <https://github.com/dawidd6/action-send-mail>.

### Conditional Steps

A step can include an `if` property to make its execution conditional.
For a list of available variables that can be used in the condition,
see <https://help.github.com/en/actions/reference/context-and-expression-syntax-for-github-actions#contexts>.

One variable that can be used holds
the name of the event that triggered the workflow.
The following step outputs this:

{% raw %}

```yaml
- name: report event
  run: echo github.event_name = ${{ github.event_name}}
```

{% endraw %}

For example, a step can execute only if the workflow was triggered
by a particular event.

```yaml
name: Demo trigger on watch
on:
  push:
    branches: [master]
  watch:
    types: [started]
jobs:
  demo:
    runs-on: ubuntu-latest
    steps:
      - name: manual trigger
        if: github.event_name == 'watch'
        run: echo "I was manually triggered."
```

To trigger this workflow, star the repo.
To trigger it again, unstar and star the repo again.

The reported action name will be the workflow name rather than
a commit message since it was not triggered by a commit.

### Setting Output

A step can set output that can be used in subsequent steps.
It uses some crazy syntax to do this.
For example:

{% raw %}

```yaml
- name: set some output
  run: echo "::set-output name=foo::bar"
  id: my_id
- name: use some output
  run: echo ${{steps.my_id.outputs.foo}}
```

{% endraw %}

This outputs "bar".

### Multiple Workflows

A repository can define any number of workflows
by creating multiple YAML files in the `.github/workflows` directory.

An event can trigger any number of workflows.
Each triggered workflow appears in the "Actions" tab
of the GitHub web UI for the repository as a separate entry.
When triggered by a push, they will all have
the same title which is a commit comment.
Below each title is the workflow name which is what distinguishes them.
Click them one at a time to see their results.

### Defining Actions

An action is defined by a YAML file
that describes an object with the following properties:

- `name`: the action name
- `description`: a description of the action
- `author`: GitHub username of the action author
- `inputs`: an object where the keys are input names and
  the values are objects with `description` and `default` properties
- `outputs`: an object where the keys are output names and
  the values are objects with a `description` property
- `runs`: an object with `using` and `main` properties
  The value of `main` is the code to execute.
  The value of `using` describes the runtime to use for `main`
  such as `node12`.

Actions can be implemented in a Docker container
using any programming language supported by Docker.
They can also be implemented outside of a Docker container using JavaScript.

Actions that are intended to be used by only a single repository
can be defined in that repository.
Actions that are intended to be shared across projects
should be defined in their own GitHub repository.

When a workflow uses an action, it can specify the version to use with
semantic versioning (major.minor.patch) or just a major version number.

Let's define a JavaScript action that outputs the day of the week
for the current date or a provided date.

Create the directory `src/actions/day-week`.
In this directory, create the file `action.yml`
with the following content to describe the action:

```yaml
name: 'Day of Week'
description: 'outputs the day of the week for today or a given date'
author: 'mvolkmann'
inputs:
  date:
    description: 'a date in the format "yyyy-mm-dd" or the string "today"'
    default: 'today'
outputs:
  dayOfWeek:
    description: 'a day of the week'
runs:
  using: 'node12'
  main: 'index.js'
```

In the same directory, create the file `index.js` that implements the action.

```js
const core = require('@actions/core');

// Can access information about the
// GitHub repository that is using this action.
//const github = require('@actions/github');

const days = [
  'Sunday',
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday'
];

try {
  const dateString = core.getInput('date');
  const date = dateString === 'today' ? new Date() : new Date(dateString);
  const dayOfWeek = days[date.getDay()];
  console.log(dateString, 'is on a', dayOfWeek);
} catch (error) {
  core.setFailed(error.message);
}
```

To use this action, add the following steps to a workflow:

```yaml
- name: get day of week for today
  uses: ./src/actions/day-of-week
- name: get day of week for my birthday
  uses: ./src/actions/day-of-week
  with:
    date: '1961-4-16'
```

For more details on defining JavaScript actions, see
<https://help.github.com/en/actions/building-actions/creating-a-javascript-action>.
