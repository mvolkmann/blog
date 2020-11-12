---
eleventyNavigation:
  key: Neo4j
layout: topic-layout.njk
---

{% aTargetBlank "https://neo4j.com/", "Neo4j" %} is a graph database.
It was first released in February 2010.
There are free and commercial versions.
The free version is limited to run on a single node
and does not provide hot backups.

Data is stored as collections of "nodes" that are attached by "edges".
Both nodes and edges can have associated labels and properties.
Each node and edge can only have one label which is rendered in diagrams,
but they can have any number of properties.
Properties can be indexed to support faster searches,
but they are not indexed by default.
Schemas can optionally be defined to specify the properties
that an be specified for each node and edge type.

Neo4j is implemented in Java, but there are drivers that
support accessing Neo4j databases from many programming languages.
Officially supported languages include
C# (.Net), Go, Java, JavaScript, and Python.
Community supported languages include
C/C++, Clojure, Erlang, PHP, R, and Ruby.

"Cypher" is the query language used to obtain data from a Neo4j database.
A good summary of Cypher syntax can be found at {% aTargetBlank
"https://neo4j.com/docs/cypher-refcard/4.1/", "Neo4j Cypher Refcard" %}.

{% aTargetBlank "https://github.com/neo4j-contrib/neovis.js/", "neovis.js" %}
is a JavaScript library for visualizing the result of Cypher queries
in a web browser.
It connects to a running Neo4j database instance
and renders live data updates.
See this {% aTargetBlank
"https://github.com/neo4j-contrib/neovis.js/issues/141", "issue" %}.

## Installing

To install Neo4j:

- Browse {% aTargetBlank "https://neo4j.com/", "https://neo4j.com/" %}.
- Click the "GET STARTED" button.
- Click the "Download Neo4j Desktop" button.
- Click the "Download" button.
- Enter the requested information and click the "Download Desktop" button.
- Save the installer file.
  On macOS, the file name is neo4j-desktop-1.3.10.dmg.
- Save the "Neo4j Desktop Activation Key" that is displayed.
- Double-click the installer file and follow the OS-specific steps.

## Neo4j Desktop application

To run the Neo4j Desktop application:

- Locate the folder where it was installed.
- Double-click its icon.
- First time only, you will be prompted
  to paste the "Neo4j Desktop Activation Key".

![Neo4j Desktop app](/blog/assets/neo4j-desktop-app.png)

To create a new database:

- Click the large rectangle labeled "+ Add Database".
- Click "Create a Local Database".
- Enter a name and password for the database.
- Click the "Create" button.

To start a database, click its "Start" button.

## Neo4j Browser application

To open a database in a Neo4j Browser window,
click its "Open" button in the Neo4j Desktop application.
This is somewhat similar to a Jupyter notebook
in that it consists of a vertical series of cells
into which code can be entered.
New code can be entered in the top cell.

![Neo4j Browser](/blog/assets/neo4j-browser.png)

Icons in the upper-right corner of each cell
can be clicked to initiate the following actions:

- save as a favorite (star)
- save as a project file (floppy)
- pin to top (pushpin)
- change to occupy the full screen (opposing arrows)
- collapse to a single line (caret)
- rerun (circle with arrowhead)
- close (X)

<img alt="Neo4j Browser icons" class="keep-size"
  src="/blog/assets/neo4j-browser-icons.png">

To execute the code in a cell,
click the blue play icon (triangle) on the right side of the cell
or press ctrl-enter (cmd-return on macOS).

## Creating and deleting nodes

To create nodes in a Neo4j browser window,
enter a Cypher command like the following:

```text
create (Mark:Person {name:'Mark Volkmann', born:1961}),
  (Tami:Person {name:'Tami Volkmann', born:1961}),
  (Amanda:Person {name:'Amanda Nelson', born:1985}),
  (Jeremy:Person {name:'Jeremy Volkmann', born:1987});
```

This creates four nodes that have the label `Person`
and the properties `name` and `born`.
The semicolon at the end is only necessary
to enter multiple commands in the same cell.

To view these `Person` nodes, enter `match (p:Person) return p`.
By default the selected view type is "Graph"
which renders a circle for each `Person` node.
Hover over a node to see the values for its `id`, `born`, and `name` properties.

To view only the `Person` nodes that are born in 1970 or later,
enter the following Cypher command:

```text
match p=(person:Person)
where person.born >= 1970
return p;
```

To view only the name property of the `Person` nodes,
enter the following Cypher command:

```text
match (person:Person) return person.name
```

To delete nodes, enter a Cypher command like the following:

```text
match (p:Person) where p.born < 1980 detach delete p
```

To re-run a previous query, possibly with modifications:

- Click the query which copies it to the top cell.
- Optionally modify it.
- Execute it.

## Creating and deleting edges

To create edges (relationships) between nodes,
enter a Cypher command like the following:

```text
match (Mark:Person{name: 'Mark Volkmann'})
match (Tami:Person{name: 'Tami Volkmann'})
match (Amanda:Person{name: 'Amanda Nelson'})
match (Jeremy:Person{name: 'Jeremy Volkmann'})
with Mark, Tami, Amanda, Jeremy
create (Mark)-[:married {year:1981}]->(Tami),
  (Mark)-[:father]->(Amanda),
  (Mark)-[:father]->(Jeremy),
  (Tami)-[:mother]->(Amanda),
  (Tami)-[:mother]->(Jeremy);
```

The edges created here have the labels "married", "father", and "mother".
The edge with the "married" label has the property "year".
The edges with the "father" and "mother" labels do not have any properties.

To view all the instances of a specific node type,
along with connecting lines for their edges,
enter a Cypher command like the following:

```text
match (p:Person) return p
```

<img alt="Neo4j Person relationships" class="keep-size"
  src="/blog/assets/neo4j-person-relationships.png">

The nodes in the diagram can be dragged to new positions
in order to manually change their layout.

To export a diagram as CSV, JSON, PNG, or SVG,
click the download icon in the upper-right corner of its cell
and select one of those formats.

<img alt="Neo4j Browser export menu" class="keep-size"
  src="/blog/assets/neo4j-browser-export-menu.png">

In addition to displaying query results as a "Graph",
they can also be viewed as a "Table", "Text", or "Code"
by clicking the corresponding buttons on the left side of the cell.

<img alt="Neo4j Browser views" class="keep-size"
  src="/blog/assets/neo4j-browser-views.png">

To temporarily hide a node, click its circle and then
click the icon containing an eye with a minus sign.

<img alt="Neo4j node selected" class="keep-size"
  src="/blog/assets/neo4j-node-selected.png">

To get only the nodes that have a specific relationship,
enter a Cypher query like the following:

```text
match (p1:Person)-[m:father]->(p2:Person) return p1, m, p2
```

<img alt="Neo4j father edges" class="keep-size"
  src="/blog/assets/neo4j-father-edges.png">

Let's make this more interesting by adding `Dog` nodes
and relationships between them and their `Person` owners.

```text
create (Maisey:Dog {name:'Maisey', breed:'Treeing Walker Coonhound'}),
  (Ramsay:Dog {name:'Ramsay', breed:'Native American Indian Dog'}),
  (Oscar:Dog {name:'Oscar', breed:'German Shorthaired Pointer'}),
  (Comet:Dog {name:'Comet', breed:'Whippet'});

match (Mark:Person{name: 'Mark Volkmann'}),
  (Tami:Person{name: 'Tami Volkmann'}),
  (Amanda:Person{name: 'Amanda Nelson'}),
  (Jeremy:Person{name: 'Jeremy Volkmann'}),
  (Maisey:Dog{name: 'Maisey'}),
  (Ramsay:Dog{name: 'Ramsay'}),
  (Oscar:Dog{name: 'Oscar'}),
  (Comet:Dog{name: 'Comet'})
with Mark, Tami, Amanda, Jeremy, Maisey, Ramsay, Oscar, Comet
create (Mark)-[:owns {role:'secondary'}]->(Comet),
  (Tami)-[:owns {role:'primary'}]->(Comet),
  (Amanda)-[:owns {role:'primary'}]->(Maisey),
  (Amanda)-[:owns {role:'primary'}]->(Oscar),
  (Jeremy)-[:owns {role:'primary'}]->(Ramsay);
```

To delete edges, enter a Cypher command like the following:

```text
match (p:Person)-[o:owns]->(d:Dog) delete o
```

This deletes all the "own" edges between a `Person` and a `Dog`.

## Querying

To view all the nodes we have created,
along with the edges between them,
enter the following Cypher command:

```text
match (n) return n
```

<img alt="Neo4j all nodes" class="keep-size"
  src="/blog/assets/neo4j-all-nodes.png">

Queries can be more specific.
For example, the following Cypher command
only renders ...:

```text
match (p:Person) where p.name starts with 'Amanda' return p
```

This only renders the `Person` node for Amanda Volkmann.
To also render the nodes connect via an edge,
click the node to display a ring around it
and then click the graph icon in the ring.

<img alt="Neo4j Amanda with ring" class="keep-size"
  src="/blog/assets/neo4j-amanda-ring.png">

The resulting displays is as follows:

<img alt="Neo4j Amanda with links" class="keep-size"
  src="/blog/assets/neo4j-amanda-with-links.png">

## Cleaning up

To delete every node and the edges between them,
enter the following Cypher command:

```text
match (n) detach delete n
```

To verify that everything has been deleted,
enter the following Cypher command:

```text
match (n) return n
```

## Creating and restoring backups

TODO: How do you install the `neo4j-admin` command?

To create a backup of a Neo4j database,
execute a command like the following:

```bash
neo4j-admin backup --backup-dir={some-directory} --database={db-name}
```

To restore a Neo4j database from a backup,
execute a command like the following:

```bash
neo4j-admin restore --from={some-directory} --database={db-name}
```

TODO: It is possible that different commands are required when using
TODO: the community version of Neo4j instead of the enterprise version.

For more detail, see {% aTargetBlank
"https://neo4j.com/docs/operations-manual/current/backup/", "Backup" %}.

## Accessing from a web app

The npm package {% aTargetBlank
"https://github.com/neo4j-contrib/neovis.js", "neovis.js" %}
supports rendering diagrams based on Neo4j Cypher queries.
This builds on the npm package {% aTargetBlank
"https://visjs.github.io/vis-network/docs/network/", "vis-network" %}
for the visualization implementation.

For detailed documentation on options, see {% aTargetBlank
"https://visjs.github.io/vis-network/docs/network/", "vis.js" %}.

Here is an example of a simple web page that renders
the `Person` and `Dog` nodes created earlier.

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width,initial-scale=1" />
    <title>neovis Demo</title>

    <style>
      #vis {
        background-color: linen;
        height: 600px;
      }
    </style>

    <script src="neovis.js"></script>

    <script>
      const cyphers = [
        'match (p1:Person)-[r:married]->(p2:Person) return p1, r, p2',
        'match (p1:Person)-[f:father]->(p2:Person) return p1, f, p2',
        'match (p1:Person)-[m:mother]->(p2:Person) return p1, m, p2',
        'match (p:Person)-[o:owns]->(d:Dog) return p,o,d'
      ];
      const thickness = 'weight';
      let vis;

      const config = {
        container_id: 'vis',
        server_url: 'bolt://localhost:7687',
        server_user: 'neo4j',
        server_password: 'neo4j19',
        labels: {
          Dog: {
            caption: 'name',
            font: {
              color: 'blue',
              face: 'fantasy',
              size: 12
            },
            image: './dog-icon.png'
          },
          Person: {
            caption: 'name',
            font: {
              color: 'red',
              face: 'sans-serif',
              size: 14
            },
            image: './person-icon.png'
          }
        },
        relationships: {
          father: {thickness},
          married: {thickness},
          mother: {thickness},
          owns: {thickness}
        }
        //initial_cypher: ''
      };

      window.onload = () => {
        vis = new NeoVis.default(config);
        vis.render();
        for (const cypher of cyphers) {
          vis.updateWithCypher(cypher);
        }
      };
    </script>
  </head>

  <body>
    <div id="vis"></div>
  </body>
</html>
```

Hover over a node or edge to see its properties in a popup.
