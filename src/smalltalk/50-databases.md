---
eleventyNavigation:
  key: Databases
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

## ODBC

ODBC is an acronym for Open DataBase Connectivity.
It defines an API for interacting with databases
and is primarily used with relational databases.

ODBC was originally created by Microsoft and Simba Technologies.
Version 1.0 was released in 1992 and
latest version (4.0) was released in 2016.

## ODBC Package for Cuis Smalltalk

To install Cuis Smalltalk support for ODBC:

- Clone the Git repository at https://github.com/Cuis-Smalltalk/DatabaseSupport
  into the same directory where the `Cuis-Smalltalk-Dev` directory resides.
- Open a Workspace.
- Enter `Feature require: #ODBC` and "Do it".

## macOS Installs

The recommended way to install shared libraries in macOS is to
use <a href="https://brew.sh" target="_blank">Homebrew</a>.

If you used Homebrew on an Intel Mac and
used "Migration Assistant" to migrate to an Apple Silicon Mac,
libraries will not be installed in the correct directory.
To verify this, open a terminal and enter `brew --prefix`.
In Apple Silicon Macs this should output `/opt/homebrew`.
If it outputs a different directory, you will need to
uninstall Homebrew and reinstall it.

Uninstalling Homebrew will delete all the libraries you have already installed.
Enter `brew list` before uninstalling Homebrew
to get a list of the currently installed packages
so they can be reinstalled after Homebrew is installed again.

To uninstall Homebrew, enter the following command in a terminal:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)"
```

To reinstall Homebrew, browse https://github.com/Homebrew/install,
click the link "Homebrew's latest GitHub release",
download the file `Homebrew-{version}.pkg`,
and double-click that file to install Homebrew.

## ODBC Shared Library

The Smalltalk ODBC package requires the `libodbc` shared library.
To install this in macOS, enter `brew install unixodbc` in a terminal.
This also installs the `isql` command
which can be used to test data source names (DSNs).
This also installs the `odbcinst` command
which can be used to output information about the ODBC installation.

If the files `isql` and `odbcinst` exist in the `/usr/local/bin` directory,
delete them so the versions installed by Homebrew will be used instead.

## Database-specific Drivers

Download a database-specific driver for each kind of database being used.

In macOS, to access PostgreSQL databases,
enter `brew install psqlodbc` in a terminal.
This creates the files `psqlodbcw.so` and `psqlodbca.so`
in the `/opt/homebrew/lib` directory.
The "w" stands for "wide", meaning that it supports Unicode characters.
The "a" stands for "ASCII", meaning that it only supports 8-bit characters.

In macOS, to access SQLite databases,
enter `brew install sqliteodbc` in a terminal.
This creates the file `/opt/homebrew/lib/libsqlite3odbc.so`.

## ODBC Data Sources

ODBC requires defining data sources in a text file.
To determine the directory where this file should reside
and the expected file name, enter `odbcinst -j` in a terminal.
Look for "User Data Sources" in the output.
This will likely be `.odbc.ini` in your home directory.
Create that file with contents similar to the following,
which defines data sources for a PostgreSQL database and a SQLite database.

```text
[PetsDSN]
Description = Postgres database for pets
Driver = PostgreSQL
Database = pets

[TodosDSN]
Description = SQLite database for a Todo app
Driver = /opt/homebrew/lib/libsqlite3odbc.so
Database = /Users/volkmannm/Documents/dev/lang/smalltalk/Cuis-Smalltalk-Dev-UserFiles/todos.db
```

The file `/usr/local/etc/odbcinst.ini` associates driver names
with paths to their shared libraries.
In the `.odbc.ini` file above, the `Driver` values
can be the absolute path to the driver shared library.
But using driver names specified in the `odbcinst.ini` file
avoids needing to repeat the shared library paths
for each data source that uses the same driver.

The following `odbcinst.ini` file specifies driver shared libraries
for PostgreSQL and SQLite.

```text
[PostgreSQL]
Description = PostgreSQL ODBC Driver
Driver = /opt/homebrew/lib/psqlodbcw.so

[SQLite]
Description = SQLite ODBC Driver
Driver = /opt/homebrew/lib/libsqlite3odbc.so
```

To list all the registered data sources, enter `odbcinst -q -s`.

To view the details of a specific data source,
enter `odbcinst -q -s -n {name}`.

To verify that the data source defined above can be accessed,
enter `isql TodosDSN` and `select * from todos;`.
Press ctrl-d to exit.

## Database Access from Smalltalk

In macOS, start a Cuis Smalltalk image by entering
`./RunCuisOnMacTerminal.sh` in a terminal.
This script includes the command which is necessary to allow
the Smalltalk ODBC package to find ODBC driver shared libraries:

```bash
export DYLD_LIBRARY_PATH="$(brew --prefix)/lib:${DYLD_LIBRARY_PATH}"
```

This script also starts a Smalltalk VM using the base image.
To use another image, copy and modify this script.

If an image is started in a way that does not
set the `DYLD_LIBRARY_PATH` environment variable
and an attempt is made to open an `ODBCConnection`,
the error message "External module not found" will be displayed.
(My "ActiveRecord" package checks for this
in the `establishConnection:dbType:` method and
signals an `ActiveRecordError` if the environment variable is not set.)

TODO: In macOS, you may be able to set that environment variable
in a way that makes it accessible to apps that are started
by double-clicking them in the Finder. Try this:

```bash
launchctl setenv DYLD_LIBRARY_PATH $(brew --prefix)/lib
```

Open an "Installed Packages" window and
verify that the ODBC package is installed.
If not, open a Workspace, enter `Feature require: #ODBC`, and "Do it".

Open a Browser, select the ODBC class category,
select the `ODBCConnection` class, click the "class" button,
and see the sample code in the class method `dsn:user:password:query:`.

Enter code similar to the following in a Workspace
to verify that your database can be queried:

```smalltalk
queryString := 'select * from todos'.
conn := ODBCConnection dsn: 'TodosDSN' user: '' password: ''.
stmt := conn query: queryString.
rs := stmt execute.

'Columns' print.
columns := rs columns.
columns do: [:column | column name print].

rs do: [:row | row print].

conn close.
```

## ODBC Built-in Logging

The `ODBCConnection` class has the instance methods `checkFeatures` and `connect`.
Both of those methods contain expressions that write to the Transcript.
If that information is not useful,
comment out those expressions to reduce Transcript output.
