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

For workflows that will run on a GitHub-hosted server,
the operating system of the cloud server can be specified.
Options include `ubuntu-latest`, `windows-latest`, and `macos-latest`.
A specific operating system version can also be specified.
The supported versions in include
`ubuntu-18.04`, `ubuntu-16.04`, `windows-2019`, and `macos-10.15`.

Workflows can also be self-hosted.
An example of specifying this is:

```yaml
runs-on: [self-hosted, linux]
```

Each "step" has a `name` and an `id`.
Step names are displayed in the web UI that displays workflow results.
Step ids are used to refer to result properties that include `outputs`.
Actions typically document their outputs.
In this case the only output is a `time` property
which is the time at which the action executed.

## Viewing Action Results

### "Set up job" and "Hello" expanded

![GitHub Actions web UI #1](/blog/assets/github-actions-web-ui-1.png)

### "Time" and "Complete job" expanded

![GitHub Actions web UI #2](/blog/assets/github-actions-web-ui-2.png)

## Defining Actions
