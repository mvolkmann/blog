---
eleventyNavigation:
  key: DynamoDB
layout: topic-layout.njk
---

## Overview

<a href="https://aws.amazon.com/dynamodb/" target="_blank">DynamoDB</a>
is cloud-based NoSQL database that is managed by AWS.

DynamoDB provides:

- no cold starts
- no version upgrades
- no maintenance windows
- broad set of security controls and compliance standards
- multi-region
- 99.999% availability SLA
- managed backups with point-in-time recovery
- streams for serverless, event-driven applications
- great integration with other AWS services

## APIs

For Java, see
<a href="https://docs.aws.amazon.com/sdk-for-java/latest/developer-guide/dynamodb-enhanced-client.html"
target="_blank">DynamoDB Enhanced Client API</a>.
This is the successor to the
<a href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBMapper.html"
target="_blank">DynamoDBMapper</a>.

For JavaScript, see
<a href="https://www.npmjs.com/package/@aws-sdk/lib-dynamodb"
target="_blank">@aws-sdk/lib/dyamodb</a>.

Both of these require an AWS accesss keys.
To create them:

- Browse https://aws.amazon.com.
- Click the "Sign In to the Console" button in the upper-right and sign in.
- Click the drop-down menu in the upper-right and select "Security credentials".
- Scroll down the "Access keys" section.
- Click the "Create access key" button.
- Click the "I understand ..." checkbox.
- Click the new "Create access key" button.
- Click the "Show" link under "Secret access key".
- Click the "Download .csv file" button to get a text file that
  contains the values for "Access key" and "Secret access key" value.
- Copy these values in the file `~/.aws/credentials` which should look like the following:

  ```text
  [default]
  aws_access_key_id={value}
  aws_secret_access_key={value}
  ```

## Authentication

Amazon Identify and Access Management (IAM) is used to
manage authentication and authorization.

## Pricing

There are two pricing models,
"on-demand capacity" and "provisioned capactity".
In both cases there is a charge for the amount of storage used.

In "on-demand capacity" pricing you pay for each read and write,
so only for what you use.
This is best when traffic is unpredictable.

In "provisioned capactity" you specify the number of reads and writes
expected per second with automatic scaling to adjust when that changes.
This is best when traffic is predictable.

## Concepts

- Table

  This is like a table in a relational database.
  It is a collection of items.

- Item

  This is like a row in a relational database.
  It is a collection of attributes.

- Attribute

  This is like a column in a relational database.
  Attribute types include booleans, numbers, strings,
  lists, sorted sets, JSON, and more.

- Key

  There are two kinds of keys, partition (row unique id) and sort.
  The primary key of a table can be the combination of
  the partition key and a sort key when partition keys are not unique.

- Indexes

  These are like indexes in a relational database.
  They make queries more efficient.

  A "global secondary index" (GSI) enables
  efficient querying on a given attribute.

## Getting Started

1. Browse <a href="https://aws.amazon.com" target="_blank">AWS</a>.
1. Create an account if you don't already have one.
1. Sign in.
1. Click "Services" in the upper-left.
1. Select "Database".
1. Select "DynamoDB".

## Creating Tables

1. Click the orange "Create table" button.
1. Tables are region-specific.
   Select a nearby region from the dropdown in the upper-right
   (ex. us-east-2 which is in Ohio).
1. Enter a name for the table (ex. Dogs).
1. Enter a name for the "Partition key" (ex. id) and select its type (ex. Number).
1. If the partition keys will not be unique, enter a name for the "Sort key"
   which is an additional attribute that when combined with the parition key
   will uniquely identify each record.
1. Under "Table settings", select the "Custom settings" radio button.
1. Under "Read/Write capacity settings", you will likely want to change
   the "Capacity mode" to "On-demand" instead of "Provisioned" to save money.
1. Click the disclosure triangle for "Maximum table throughput"
   to limit costs.
1. Check both the "Set maximum read request units"
   and "Set maximum write request units" checkboxes.
1. For both, enter maximum number of requests per second that should be allowed.
1. In the "Secondary Indexes" section,
   create indexes to improve search performance.
   For each index to be created:

   - Click the "Create global index" button.
   - Enter the name of an attribute for the primary index.
   - Select the data type of the attribute.
   - Click the orange "Create index" button.

   For example, the "Dogs" table may need indexes
   for the "name" and "breed" attributes.

1. Scroll to the bottom and click the orange "Create table" button.
1. A list of tables will be displayed and
   the new table should have a status of "Active".
1. Click the new table name to see information about it.
1. Global tables are replicas in other regions that improve access times
   for users in other parts of the world.
   Global tables are automatically kept in sync with their original table.
   To create a global table:

   - Click the "Global tables" tab.
   - Click the "Create replica" button.
   - Click the "Choose a Region" dropdown and select a region.
   - Click the orange "Create replica" button.

1. To create backups of your database, click the "Backups" tab.
   There are three kinds of supported backups:
   Point-in-time recovery (PITR), on demand, and scheduled.

## Creating Items

1. Click a table name.
1. Click the orange "Explore table items" button.
1. Click the "Create item" button at the bottom.
1. Enter a value for the partition key.
1. For each attribute:

   - Click the "Add new attribute" button.
   - Select a data type such as "String", "Number", or "Boolean.
   - Enter an attribute name and value.

1. Click the orange "Create item" button.
1. To create more items with the same set of attributes:

   - Select an item by checking its checkbox.
   - Click "Actions" and select "Duplicate item".
   - Modify the values of the attributes, especially the partition key.
   - Click the orange "Create item" button.

## Filtering Items

To filter the set of items displayed:

1. Click the disclosure triangle to expand the "Filters" section.
1. Click the "Add Filter" button.
1. Select an atrribute name, type, and condition (ex. "Contains).
1. Enter a value.
1. Optionally add more filters by repeating the steps above.
1. Click the orange "Run" button.

Only the matching items will be displayed at the bottom of the page.

To remove all the filters, click the "Reset" button
and click the orange "Run" button again.

## SQL Queries

To run SQL queries against the tables:

- Click "PartiQL editor" in the left nav.
- Enter a query like "select \* from Dogs".
- Click the orange "Run" button.

Query results will appear under "Items returned" at the bottom of the page.
