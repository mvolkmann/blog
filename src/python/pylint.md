---
eleventyNavigation:
  key: pylint
  order: 8
  parent: Python
layout: topic-layout.njk
---

{% aTargetBlank "https://www.pylint.org/", "pylint" %}
is a Python linting tool.

To install it, enter `pip install pylint`.

To configure it, ensure that the `PYTHONPATH` environment variable
includes the path where `pip` installs globally available modules.
For example, on my Mac the directory is
`/Library/Frameworks/Python.framework/Versions/3.8/lib/python3.8/site-packages`.

To lint a file, enter `pylint name.py`.

To disable a rule, add a comment like the following:

```python
# Disable pylint rule that treats all module-level variables as constants.
# pylint: disable=C0103
```
