---
eleventyNavigation:
  key: VS Code Python Support
  order: 2
  parent: Python
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://github.com/microsoft/pylance-release", "Pylance" %}
is an extension from Microsoft that utilized the Language Server Protocol (LSP)
to enable IDE features such as code completion, parameter suggestions,
auto-imports, code outline, code navigation, syntax checking, type checking,
and syntax highlighting.

Modify `settings.json` to enable this extension and configure options.

```json
  "python.languageServer": "Pylance",
```

For strict type checking, add the following:

```json
  "python.analysis.typeCheckingMode": "strict",
```

To avoid adding two blank lines before and after each function definition:

```json
  "python.formatting.autopep8Args": ["--ignore=E302,E305"],
```

More detail is coming soon!
