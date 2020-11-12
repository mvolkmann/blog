---
eleventyNavigation:
  key: Neo4j
layout: topic-layout.njk
---

{% aTargetBlank "https://neo4j.com/", "Neo4j" %} is a graph database
implemented in Java.
It was first released in February 2010.
There are free and commercial versions.
The free version is limited to run on a single node
and does not provide hot backups.

Data is stored as collections of "nodes" that are attached by "edges".
Both nodes and edges can have associated labels and attributes.
It supports indexing of attributes for faster searches
and optional schemas.

There are drivers that support accessing Neo4j databases
from many programming languages.
Officially supported languages include
C# (.Net), Go, Java, JavaScript, and Python.
Community supported languages include
C/C++, Clojure, Erlang, PHP, R, and Ruby.

"Cypher" is the query language used to obtain data from a Neo4j database.
A good summary of Cypher syntax can be found at { aTargetBlank
"https://neo4j.com/docs/cypher-refcard/4.1/", "Neo4j Cypher Refcard" %}.

{% aTargetBlank "https://github.com/neo4j-contrib/neovis.js/", "neovis.js" %}
is a JavaScript library for visualizing the result of Cypher queries
in a web browser.
