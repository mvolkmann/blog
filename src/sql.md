---
eleventyNavigation:
  key: SQL
layout: topic-layout.njk
---

## Overview

Structured Query Language (SQL) is used to interact with relational databases.

## Comments

Single line comments begin with `--`.

Multi-line comments are delimited with `/* ... */`.

## Data Definition Language (DDL)

To create a table:

```sql
create table {table-name} (
  {column-name} {type} {modifiers},
  ...
)
```

To delete a table:

```sql
drop table {table-name}
```

To add a column:

```sql
alter table {table-name}
add column {column-name} {type} {modifiers}
```

To delete a column:

```sql
alter table {table-name}
drop column {column-name}
```

## Data Manipulation Language (DML)

To insert a row:

```sql
insert into {table-name}
({column-name}, ...)
values (value, ...)
```

To modify rows:

```sql
update {table-name}
set {column-name}={value}, ...
where {condition}
```

For example (in SQL Server):

```sql
update some_table
set end_date = cast('10/31/2014' as datetime)
where id = 19
```

An upsert inserts a row if not present and updates a row if present.

To upsert rows:

```sql
insert into {table-name}
({column-name}, ...)
values (value, ...)
on duplicate key update
column-name-1 = v1,
column-name-2 = v2,
...
```

To delete rows:

```sql
delete from {table-name} where {condition}
```

To select rows:

```sql
select [unique] {column-name}, ...
from {table-name}, ...
where {condition}
order by {column-name}, ...
```

The `where` and `order` by clauses are optional.
The `where` clause can use `like` for wildcard matching

- `%` matches any number of characters including zero
- `_` matches any single character
- for example, `select age from people where lastname like 'V_lk%';`

To find records where a given column is or isn't null,
use `{column-name} IS NULL` or `{column-name} IS NOT NULL`.

When selecting from more than one table (a join),
select columns are prefixed by a table alias and
can have their own alias to avoid name conflicts.
For example;

```sql
select n.namespace as namespace
from Element e, Namespace n
where e.sourceFileID = 253 and
e.namespaceID = n.id
```

The number of rows returned can be limited,
but the syntax is database-specific.
In Postgres and MySQL, add `limit {n}` to the end of the `select`.

## Sequences

Sequences are used to assign sequential ids to new rows.
To reset a sequence:

```sql
alter sequence {sequence-name} restart with 1
```

## Joins

A join returns column values from two tables based on
matching a column value in one table with a column value in another.

There are many kinds of joins.

<figure style="width: 75%">
  <img alt="SQL joins" src="/blog/assets/sql-joins.jpg?v={{pkg.version}}">
  <figcaption>SQL joins by @ezekiel_aleke</figcaption>
</figure>

- inner

  This is the most common kind of join and is the default kind.
  It finds the intersection between two tables.
  It only returns rows in the first table that
  have a corresponding row in the second table.
  Rows in the first table will be returned once
  for each matching row in the second table.

  For example:

  ```sql
  select e.name as eName, s.filename as sName
  from Element e, SourceFile s
  where e.sourceFileID = s.id
  order by eName
  ```

  The names `eName` and `sName` can be used to
  retrieve those columns from the rows in the result set.

- left outer

  This returns all rows in the first table
  plus matching rows in the second table
  or null values if there are no matching rows.

- right outer

  This returns all rows in the second table
  plus matching rows in the first table
  or null values if there are no matching rows.

- full outer

  This returns all rows from both tables
  with matching rows from the other table
  or null values if there are no matching rows.
  Some databases don't support this kind of join.

- cross

  This returns the cartesian product (all possible combinations)
  of rows from two tables.

For more detail, see {% aTargetBlank
"http://en.wikipedia.org/wiki/Join_(SQL)", "Wikipedia" %}.

## Common Operations

### Get last auto-increment key

When keys are automatically assigned from a sequence as new rows are added,
the last key assigned in the current process can be obtained using `currval()`.
The steps to do this are:

1. `select currval('sequence_name') as id`
1. Advance to the first row in the result set that is returned.
1. Get the integer value of the column named "id".

### Get number of rows in a table

The steps to do this are:

1. `select COUNT(\*) as count from table-name`
2. Get value of 'count' as an integer.

### Check for duplicate rows

```sql
select col1, col2, col3, count(_) from table-name
group by col1, col2, col3
having count(_) > 1
```

### Get current date/time

The steps to do this are:

1. `select CURRENT_DATE from dual`
1. Retrieve the string value of 'CURRENT_DATE' from the result set.
