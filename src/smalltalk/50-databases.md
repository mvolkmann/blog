---
eleventyNavigation:
  key: Databases
  order: 50
  parent: Smalltalk
layout: topic-layout.njk
---

To install support for ODBC:

- Clone the Git repository at https://github.com/Cuis-Smalltalk/DatabaseSupport.
- Open a "File List" window.
- Navigate to the "DatabaseSupport" directory.
- Select the file `ODBC_pck.st`.
- Click the "install package" button.

This adds the class category "ODBC".

Open a Browser, select the ODBC class category,
select the ODBCConnection class, click the "class" button,
and see the sample code in the class method `dsn:user:password:query:`.

This fails on the line
`conn := ODBCConnection dsn: dsn user: user password: password.`
The error is "External module not found".

This package requires the `libodbc` external library.
To install this in macOS, install Homebrew and enter `brew install unixodbc`.
This installs the file `libodbc.2.dylib`.

TODO: Hown can I enable the ODCB package to find the dynamic library?

TODO: Try using the ODBC package to access many kinds of databases including
TODO: SQLite, PostgreSQL, and MongoDB.
