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
<a href="https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBMapper.html"
target="_blank">DynamoDBMapper</a>.

For JavaScript, see
<a href="https://www.npmjs.com/package/@aws-sdk/lib-dynamodb"
target="_blank">@aws-sdk/lib/dyamodb</a>.

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

Browse <a href="https://aws.amazon.com" target="_blank">AWS</a>.
