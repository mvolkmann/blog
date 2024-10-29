---
eleventyNavigation:
  key: Contributing
  order: 60
  parent: Smalltalk
layout: topic-layout.njk
---

## Cuis-Smalltalk-Dev Changes

Suggested improvements to Cuis Smalltalk should be
discussed on the mailing list at
[Cuis-dev -- Discusion of Cuis Smalltalk](https://lists.cuis.st/mailman/listinfo/cuis-dev).
If interest is shown there then a "Change Set" should be submitted.

To create a Change Set that includes your suggested changes to Cuis:

- Make changes to packages provided by Cuis.
- Open the World menu and select Open ... Change Sorter.
- Select the change set whose name contains "CuisCore".
- Examine all the changes it contains
  to verify they are what should be included.
- Right-click the change set and select "Rename change set".
- Replace "AuthorName" with your name.
- Right-click the change set and select "File out and keep".

The file will be saved in the `Cuis-Smalltalk-Dev-UserFiles/ChangeSets`
directory with an extension of `.cs.st`.
Send this file as an attachment to the mailing list.

To test a change set:

- Start a fresh image.
- Open the World menu and select Open ... File List.
- Select a change set file.
- Click the "install" button.
- Write code in a Workspace to test the changes.

## Changes to Other Repositories

To contribute changes to other GitHub repositories
under the Cuis-Smalltalk account at https://github.com/Cuis-Smalltalk:

1. Fork the repository.
1. Make changes in your fork.
1. Test your changes.
1. Create a pull request to have your fork merged into the original repository.
