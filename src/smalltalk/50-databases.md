---
eleventyNavigation:
  key: Databases
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

## ODBC Package for Cuis Smalltalk

To install Cuis Smalltalk support for ODBC:

- Clone the Git repository at https://github.com/Cuis-Smalltalk/DatabaseSupport.
- Open a "File List" window.
- Navigate to the "DatabaseSupport" directory.
- Select the file `ODBC_pck.st`.
- Click the "install package" button.

This adds the class category "ODBC".

TODO: Is the reason that this package cannot be installed by evaluating
`Feature require: 'DatabaseSupport'` in a Workspace
that the package file is not named `DatabaseSupport_pck.st`?

As of October 26, 2024, there is an issue
in the `ODBCResultSet` `fetchRow` method.
Replace the line

```smalltalk
row := ODBCRow new: columns size.
```

with this:

```smalltalk
row := ODBCRow new.
```

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

Unistalling Homebrew will delete all the libraries you have already installed.
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

## Database-specific Drivers

Download a database-specific driver for each kind of database being used.
In macOS, when using SQLite, enter `brew install sqliteodbc` in a terminal.
Add the following line to each SQLite data source definition:

```text
Driver = /usr/local/lib/libsqlite3odbc.dylib
```

TODO: How do you install a driver for PostgreSQL databases?

## ODBC Data Sources

ODBC requires defining data sources in a text file.
To determine the directory where this file should reside
and the expected file name, enter `odbcinst -j` in a terminal.
Look for "User Data Sources" in the output.
This will likely be `.odbc.ini` in your home directory.
Create that file with contents similar to the following:

```text
[TodoDSN]
Description = SQLite database for a Todo app
Driver = /usr/local/lib/libsqlite3odbc.dylib
Database = /Users/volkmannm/Documents/dev/lang/smalltalk/Cuis-Smalltalk-Dev-UserFiles/todos.db
Timeout = 2000
```

Verify that the data source defined above can be accessed
by entering `isql TodoDSN` and `select * from todos;`.
Press ctrl-d to exit.

## Database Access from Smalltalk

Open a Browser, select the ODBC class category,
select the `ODBCConnection` class, click the "class" button,
and see the sample code in the class method `dsn:user:password:query:`.

Enter code similar to the following in a Workspace
to verify that your database can be queried:

```smalltalk
queryString := 'select * from todos'.
conn := ODBCConnection dsn: 'TodoDSN' user: '' password: ''.
stmt := conn query: queryString.
rs := stmt execute.

'Columns' print.
columns := rs columns.
columns do: [:column | column name print].

rs do: [:row | row print].

conn close.
```
