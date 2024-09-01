---
eleventyNavigation:
  key: Contributing
  order: 60
  parent: Smalltalk
layout: topic-layout.njk
---

Suggested improvements to Cuis should be discussed on the mailing list at
[Cuis-dev -- Discusion of Cuis Smalltalk](https://lists.cuis.st/mailman/listinfo/cuis-dev).
If interest is shown there then a "Change Set" should be submitted.

To create a Change Set that includes your suggested changes to Cuis:

- Make changes to packages provided by Cuis.
- Open the World menu and select Open ... Change Sorter.
- Select the chnage set whose name contains "CuisCore".
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
