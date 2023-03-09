---
eleventyNavigation:
  key: jq
layout: topic-layout.njk
---

## Overview

JavaScript Object Notation (JSON) is a data format that is
frequently returned by API services and read from files.
Often this is not formatted for human readability and
all the data is on a single line with no added spaces.
The tool {% aTargetBlank "https://stedolan.github.io/jq/", "jq" %}
helps with this and does much more.

jq is implemented in C.

The basic functionality of jq is to pretty-print JSON data.
But can also filter, sort, and transform JSON data.

jq is practically a programming language.
It has a long list of features including
types, conditionals, regular expressions, math functions,
custom function definitions, streaming, and more.

## Resources

- {% aTargetBlank "https://stedolan.github.io/jq/", "jq Home Page" %}
- {% aTargetBlank "https://earthly.dev/blog/jq-select/", "An Introduction to JQ" %}
  by Adam Gordon Bell
- {% aTargetBlank "https://www.youtube.com/watch?v=FSn_38gDvzM&list=PLKaiHc24qCTSOGkkEpeIMupEmnInqHbbV&index=1",
  "Processing JSON in the command-line made easy - jq tutorial (first steps)" %}
  YouTube video by Szymon Stepniak
- {% aTargetBlank "https://www.youtube.com/watch?v=EIhLl9ebeiA&list=PLKaiHc24qCTSOGkkEpeIMupEmnInqHbbV&index=2",
  "Transforming, sorting, and grouping JSON documents in the command-line - jq tutorial" %}
  YouTube video by Szymon Stepniak

## Installing

There is a {% aTargetBlank "https://jqplay.org/", "web-based UI" %} for jq
that can be used as an alternative to installing it locally
or just to experiment.

To install jq in macOX using Homebrew, enter `brew install jq`.
To install jq in other operating systems or
use a different approach for installing in macOS, see
{% aTargetBlank "https://stedolan.github.io/jq/download/", "Download jq" %}.

## Basic Usage

To pretty-print the contents of a JSON file, enter `cat {file-path} | jq`.
Another way to write this is `jq . {file-path)`
where `.` is a filter that keeps the entire contents.

To pretty-print the response from an API service,
enter `curl {service-url} | jq`.
For example, we can view the JSON response from a public API that returns
an object where the keys are dog breeds
and the values are arrays of variety names.

```bash
curl https://dog.ceo/api/breeds/list/all | jq
```

A portion of the output is shown below.

<img alt="jq dog breeds" style="width: 100%"
  src="/blog/assets/jq-dog-breeds.png?v={{pkg.version}}"
  title="jq dog breeds">

To work repeatedly with the same JSON data returned from an API endpoint,
consider capturing the data in a local file. For example:

```bash
curl https://dog.ceo/api/breeds/list/all > dogs.json
```

## Filtering

jq can filter the JSON data and output a subset.
For example, the filter `.message.hound` assumes that the JSON data
describes an object rather than an array,
finds the top-level property named "message",
assumes its value is an object,
and finds the property "hound" inside that object.
To use this filter we can enter `jq -c .message.hound dogs.json`.
This outputs the following which is a compact (`-c`) JSON array
of all the hound varieties:

```text
["afghan","basset","blood","english","ibizan","plott","walker"]
```

{% aTargetBlank "https://openlibrary.org", "Open Library" %} supports
an API for getting publications whose description contains a given word.
For example, the following command captures JSON describing publications
related to Svelte which is a web framework.

```bash
curl -s "https://openlibrary.org/search.json?q=svelte" > publications.json
```

The structure of the JSON returned is as follows:

```json
{
    "numFound": 25,
    "start": 0,
    "numFoundExact": true,
    "docs": [
        ... publication objects go here ...
    ],
    "num_found": 25,
    "q": "Svelte",
    "offset": null
}
```

Each publication in the `docs` array is described similar to the following:

```json
        {
            "key": "/works/OL21909240W",
            "type": "work",
            "seed": [
                "/books/OL36011260M",
                "/books/OL29882427M",
                "/works/OL21909240W",
                "/subjects/mathematics",
                "/authors/OL8370536A"
            ],
            "title": "Svelte and Sapper in Action",
            "title_suggest": "Svelte and Sapper in Action",
            "edition_count": 2,
            "edition_key": [
                "OL36011260M",
                "OL29882427M"
            ],
            "publish_date": [
                "2020"
            ],
            "publish_year": [
                2020
            ],
            "first_publish_year": 2020,
            "number_of_pages_median": 475,
            "lcc": [
                "QA-0076.76000000.A65"
            ],
            "isbn": [
                "9781638350682",
                "163835068X",
                "9781617297946",
                "1617297941"
            ],
            "last_modified_i": 1677546021,
            "ebook_count_i": 0,
            "ebook_access": "no_ebook",
            "has_fulltext": false,
            "public_scan_b": false,
            "publisher": [
                "Manning Publications Co. LLC",
                "Manning Publications Company",
                "Manning Publications"
            ],
            "language": [
                "eng"
            ],
            "author_key": [
                "OL8370536A"
            ],
            "author_name": [
                "Mark Volkmann"
            ],
            "subject": [
                "Mathematics"
            ],
            "publisher_facet": [
                "Manning Publications",
                "Manning Publications Co. LLC",
                "Manning Publications Company"
            ],
            "subject_facet": [
                "Mathematics"
            ],
            "_version_": 1759034499007512577,
            "lcc_sort": "QA-0076.76000000.A65",
            "author_facet": [
                "OL8370536A Mark Volkmann"
            ],
            "subject_key": [
                "mathematics"
            ]
        },
```

To get only the publications where the author name includes "Volkmann"
we can use the `select` and `contains` functions as follows
where `docs` is a top-level property in the input JSON
whose value is an array of publication objects.

```bash
jq '[.docs[] | select(.author_name[0] | contains("Volkmann"))]' publications.json
```

## Transforming

jq can transform the JSON data given to it into different data
that may or may not use JSON syntax.

We can transform the `publication.json` file above
into a JSON array of objects that only provide
the `date`, `author`, and `title` of each publication.
The publication objects in the input JSON have
`publish_date` and `author_name` properties whose values are arrays.
We only care about the first element and
we want to rename those properties to `date` and `author`.
Wrapping the entire filter expression in square brackets
causes a JSON array to be output.
The expression `.docs[]` get the elements in the `docs` property array.
The part in curly braces after the pipe operator creates
new JSON objects containing the specified properties.

```bash
jq '[.docs[] | {date: .publish_date[0], author: .author_name[0], title}]' publications.json > pub1.json
```

The output will begin as follows:

```json
[
  {
    "date": "2018",
    "author": "Chris Whyatt",
    "title": "Svelt"
  },
  {
    "date": "2021",
    "author": "Alex Libby",
    "title": "Practical Svelte"
  },
```

If any of the `date` properties have a `null` value,
we can filter out those publications using the `select` function as follows
where `.[]` represents the set of elements in the top-level array:

```bash
jq '[.[] | select(.date != null)]' pub1.json > pub2.json
```

## Regular Expressions

jq provides several functions that take regular expressions as arguments.
These include `capture`, `gsub`, `match`,
`scan`, `split`, `splits`, `sub`, and `test`.

Some of the dates in the JSON file above
only specify a year while others also specify a month and day.
To transform all the dates to only include the year
we can use the `scan` function as follows.
This keeps only the last four characters which should be the year.
For example, "February 1, 2002" will be replaced by "2002".

```bash
jq '[.[] | {year: .date | scan("\\d{4}$"), author, title}]' pub2.json > pub3.json
```

## Sorting

To sort the objects on their `date` in reverse order
we can use the `sort_by` function as follows
where `.` represents the top-level array:

```bash
jq '. | sort_by(.year) | reverse' pub3.json > pub4.json
```

## Limiting Output

We can limit the number of array elements to be output.
For example, adding `| [limit(3; .[])]`
only outputs data for the first three publications.
The `.[]` part specifies what we want to limit
which in this case is the top-level array.
The square brackets around the call to the `limit` function
cause it to output a JSON array rather than just a set of objects.

```bash
jq 'limit(3; .[])' pub4.json
```

## Grouping

We can group objects based a common property value.
For example, adding `| group_by(.date)`.

```bash
jq 'group_by(.year)' pub4.json > pub5.json
```

The output will begin as follows:

```json
[
  [
    {
      "year": "1852",
      "author": "Luigi Canina",
      "title": "Particolare genere di architettura proprio degli usi domestici"
    },
    {
      "year": "1852",
      "author": "Luigi Canina",
      "title": "Particolare genere di architettura domestica"
    },
    {
      "year": "1852",
      "author": "Luigi Canina",
      "title": "Particolare genere di architettura domestica"
    }
  ],
  [
    {
      "year": "1959",
      "author": "Ben Weider",
      "title": "Mangez bien et restez svelte"
    }
  ],
```

Now that the publications are grouped by date,
we can output the number of publications in each year.
The date for each group can be obtained from the
`date` property of the first element in each group array
using the syntax `.[0].year`.
The number of elements in each group can be obtained
using the syntax `. | length`.
Each of the objects in the resulting JSON array will have `year` and `count` properties.
We can sort them on `year` in descending order
with `| sort_by(.year) | reverse`.

```bash
jq '[.[] | {year: .[0].year, count: . | length}] | sort_by(.year) | reverse' pub5.json
```

The output will begin as follows:

```json
[
  {
    "year": "2023",
    "count": 2
  },
  {
    "year": "2022",
    "count": 1
  },
```
