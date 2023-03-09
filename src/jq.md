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
helps with this.

jq pretty-prints JSON data.
It can also filter, sort, and transform the data.

jq is practically a programming language.
It has a long list of features including
types, conditionals, regular expressions, math functions,
custom function definitions, streaming, and more

## Installing

To install jq in macOX using Homebrew, enter `brew install jq`.

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

## Transforming

jq can transform the JSON data given to it into different data.

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

We can use `jq` to transform this JSON into a JSON array of objects
that only provide the `date`, `author`, and `title` of each publication.
The publication objects in the input JSON have
`publish_date` and `author_name` properties whose values are arrays.
We only care about the first element
and we want to rename those properties to `date` and `author`.
Wrapping the entire filter expression in square brackets
causes a JSON array to be output.

```bash
jq "[.docs[] | {date: .publish_date[0], author: .author_name[0], title}]" publications.json
```

Some of the `date` properties have a `null` value.
To filter out those publications we can use the `select` function as follows:

```bash
jq "[.docs[] | {date: .publish_date[0], author: .author_name[0], title} | select(.date != null)]" publications.json
```

To sort the objects on their `date` in reverse order
we can use the `sort_by` function as follows:

```bash
jq "[.docs[] | {date: .publish_date[0], author: .author_name[0], title} | select(.date != null)] | sort_by(.date) | reverse" publications.json
```

Some of the dates only specify a year while others also specify a month and day.
This is bad for sorting.
We can transform all dates to only include the year as follows.
