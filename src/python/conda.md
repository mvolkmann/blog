---
eleventyNavigation:
  key: conda
  order: 8
  parent: Python
layout: topic-layout.njk
---

To install Anaconda, browse <https://www.anaconda.com/products/individual>,
press the "Download" button, and click the link for the desired version.
Then double-click the downloaded installer and follow the instructions.

In macOS this is installed in ~/opt/anaconda3.
To use commands such as `conda`, add the `~/opt/anaconda3/bin` directory
to the `PATH` environment variable.

## Environments

To create a new environment, enter a command like:

```bash
conda create -n my-env python=3.8 pandas jupyter
```

To initialize conda to work with a specific shell,
enter `conda init {shell-name}`.
For example, `conda init fish`.
In order for this change to take effect,
close this shell and open a new one.

To activate this environment, enter `conda activate my-env`.

To deactivate the active environment, enter `conda deactivate`.

## VS Code

To use a conda environment in VS Code,
click "Python {version}" in the lower-left
and select the interpreter from the drop-down
that includes the environment name followed by ": conda".
This environment will then be used by Jupyter Notebooks
that are opened inside VS Code.
