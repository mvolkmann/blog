---
eleventyNavigation:
  key: Anaconda
  order: 7
  parent: Python
layout: topic-layout.njk
---

Anaconda is a Python distribution and
a tool for managing Python virtual environments.

To install the individual edition of Anaconda, browse the {% aTargetBlank
"https://www.anaconda.com/products/individual", "Individual Edition" %} page,
press the "Download" button, and click the link for the desired version.
Then double-click the downloaded installer and follow the instructions.

In macOS this is installed in ~/opt/anaconda3.
To use commands such as `conda`, add the `~/opt/anaconda3/bin` directory
to the `PATH` environment variable.

The install modifies the startup script for your default shell.
One thing this does is change the terminal prompt to include
the name of the activated environment at the end of the line.
Depending on other prompt customizations you have made, this may be problematic.
To prevent Anaconda from doing this, create the file `.condarc`
in your home directory and add the line `changeps1: False` to it.

Anaconda includes many popular Python packages by default.
Highlights include autopep8, beautifulsoup, bokeh, conda, flask,
jupyter, jupyterlab, matplotlib, notebook, numpy, pandas, pandoc, pillow, pip,
pylint, pyqt, pytest, python, qt, readline, regex, requests,
scikit-image, scikit-learn, scipy, seaborn, sqlite, and tk.

To install additional packages (including their dependencies),
update packages, and remove packages,
Anaconda uses the conda package manager.
This can download packages from
the Anaconda repository (curated by Anaconda),
conda-forge (curated by the community),
and Anaconda Cloud (where developers can upload packages),
and private repositories created with Anaconda Team or Enterprise Edition.

For help, enter `conda --help`.

## Anaconda Navigator

Anaconda Navigator is a GUI tool for launching various Python tools
such as JupyterLab, Jupyter Notebook, VS Code, and more.
It also provides a way to see the packages that are installed in each
virtual environment that has been created.
By default a virtual environment named "base" is defined.
This includes a large number of pre-installed packages.
To see them, click "Environments" in the left nav, and select "base".

To change the packages included in an environment,
click "Environments" in the left nav, select an environment.

To create a new environment,
press "Create" at the bottom of the environments list,
enter a name, select a version of Python, and press "Create".
New environments begin with no packages installed.

To create a new environment that is a clone of an existing one,
select an existing environment,
press "Clone" at the bottom of the environments list,
enter a name, and press "Clone".
The base environment cannot be cloned,
presumably because all of its packages
are included in new environments by default.

To create a YAML file that describes an existing environment,
open a terminal, enter `conda activate {env-name}`,
and enter `conda env export > {name}.yml`.
This file can be sent to another developer
so they can create an identical environment.
To import an environment described in a YAML file,
press "Import" at the bottom of the environments list,
enter a name, and select the YAML file.

To delete an environment, select it and
press "Remove" at the bottom of the environments list.
A confirmation dialog will be displayed.
Press "Remove" to confirm.

To add packages, select "Not installed" from the drop-down.
The search input can be used to filter the list.
Click the checkbox in front of each package to be installed.
To install a specific version, right-click the checkbox,
hover over "Mark for specific version installation",
and select the version.
When finished selecting the packages to be added,
press the "Apply" button at the bottom.
A dialog listing all the packages to be added,
some of which are dependencies, will be displayed.
Press "Apply" to do so.

To enable adding packages from channels other than the default Anaconda channel,
press "Channels" to see a dialog listing the current channels,
press "Add..." to add a channel (such as "conda-forge"),
press return, and press "Update channels".

To update packages, select "Updatable" from the drop-down.
All the packages that have an available update will be displayed
and all will be checked by default.
One at a time, click the checkbox in front of a package to update
and select "Mark for update" from the popup menu that appears.
To update to a specific version, right-click the checkbox,
hover over "Mark for specific version installation",
and select the version.
When finished marking the packages to be updated,
press the "Apply" button at the bottom.
A dialog listing all the packages to be updated,
some of which are dependencies, will be displayed.
Press "Apply" to do so.

To remove packages, select "Installed" from the drop-down.
One at a time, click the checkbox in front of a package to remove
and select "Mark for removal" from the popup menu that appears.
When finished marking the packages to be removed,
press the "Apply" button at the bottom.
A dialog listing all the packages to be removed,
some of which are dependencies, will be displayed.
Press "Apply" to do so.

When an update to Anaconda Navigator becomes available,
an "Upgrade Now" button will appear in the upper-right corner.
Click this to upgrade.

## conda

The `conda` command can be used from a terminal to work with environments.

| Action                                                                                                     | Command                                 |
| ---------------------------------------------------------------------------------------------------------- | --------------------------------------- |
| get detailed information about `conda` installation                                                        | `conda info`                            |
| update version of `conda`                                                                                  | `conda update conda`                    |
| initialize conda to work with a specific shell;<br>for change to take effect, close shell and open new one | `conda init {shell-name}`<br>ex. `fish` |
| list available environments;<br>current will have asterisk after name                                      | `conda env list`                        |
| activate different environment                                                                             | `conda activate {name}`                 |

To create a new environment, enter a command like the following
which specifies a name for the environment (`my-env`),
a version of Python to use (3.8), and
packages that should be initially installed (pandas and jupyter):

```bash
conda create --name my-env python=3.8 pandas jupyter
```

Note that creating a new environment does not activate it.

To clone an existing environment,
enter `conda create --clone {old-env} --name {new-env}`.

To deactivate the active environment, enter `conda deactivate`.
Since there is always some active environment,
this will activate the "base" environment.

To list all packages in the active environment,
enter `conda list`.

To install a package in the active environment,
enter `conda install {package-name}`.
If it is not found, browse
{% aTargetBlank "https://anaconda.org/", "anaconda.org" %}
and search for the package to determine
what non-default "channels" might host it.
Then include the channel name in the install command.
For example, to install the matplotlib package,
enter `conda install -c conda-forge matplotlib`.
To specify a version to install other than the latest,
see "Specifying version numbers" in the {% aTargetBlank
"https://docs.conda.io/projects/conda/en/4.6.0/_downloads/52a95608c49671267e40c689e0bc00ca/conda-cheatsheet.pdf",
"Conda Cheat Sheet" %}.

To update a package in the active environment,
enter `conda update {pkg}`.

To remove packages from the active environment,
enter `conda remove {pkg1} {pkg2} ...`.

To create a YAML file that describes the active environment,
enter `conda env export > {env}.yml`.

To create a new environment from a YAML file,
enter `conda env create -f {env}.yaml`.

To remove an environment,
enter `conda remove --name {env} --all`.

## VS Code

To use a conda environment in VS Code,
click "Python {version}" in the lower-left
and select the interpreter from the drop-down
that includes the environment name followed by ": conda".
This environment will then be used by Jupyter Notebooks
that are opened inside VS Code.
