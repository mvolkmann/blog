---
eleventyNavigation:
  key: GitHub Projects
  parent: GitHub
layout: layout.njk
---

GitHub Projects provided a free alternative to project tracking solutions
such as Jira, Rally, and Trello.
Each is represented by a board with multiple columns that contain cards.

There are three kinds of project boards:

- Repository-specific: scoped to a single repository
- User-owned: can be linked to any personal repositories (up to 25)
- Organization-wide: can be linked to any repositories that
  belong to an organization (up to 25)

User-owned and organization-wide project boards can see issues in
linked repositories and can drag them onto the board to turn them into cards.
The GitHub page for linked repositories doesn't provide access
to the project boards in which they are linked.

To create a repository project:

1. Browse a repository.
1. Click the "Projects" tab.
1. Press the "Create a project" button.
1. Enter a project board name.
1. Optionally enter a project description.
1. Optionally select a project template such as "Basic kanban".
1. Press the "Create project" button.

To create a user project,
browse github.com/{username} instead of a specific repository
and then follow the same steps as above.

To link a repository to a user project so it is shared by them:

1. Click the "Projects" tab.
1. Click a user project name.
1. Click the "X" in the upper-right of the card search pane to close it.
1. Click "Menu" in the upper-right.
1. Click the ellipsis in the upper-right.
1. Click "Settings" in the drop-down.
1. Click "Linked repositories".
1. Press the "Link a repository" button.
1. Enter part of a repository name to get a filtered list.
1. Click a repository name.
1. Click the "Link" button.
1. Repeat to link additional repositories.

TODO: Why can't you see the user project from the linked repositories?

If no project template is selected, columns can be added manually.

The "Basic kanban" project template provides three columns
named "To do", "In progress", and "Done"
with a few sample cards already in the "To do" column.
Close the card search results panel on the right
by clicking the "X" in the upper-right.

To delete a card, click the ellipsis in its upper-right
and select "Delete note".

To add a card in a column, click the "+" in the upper-right of the column.

To edit a card, click the ellipsis in the upper-right of the card
and select "Edit note". A dialog will open where changes can be made.

To move a card to a different column, drag it.
