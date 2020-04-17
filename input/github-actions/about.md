---
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

## Viewing Action Results

## Defining Actions
