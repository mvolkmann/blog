---
eleventyNavigation:
  key: Optional Packages
  order: 33
  parent: Smalltalk
layout: topic-layout.njk
---

There are many sources of optional packages
that are compatible with Cuis Smalltalk.

One source is packages that are included in the Cuis Smalltalk distribution.
These can be found in the `Packages/Features`
and `Packages/System` subdirectories.

To obtain more that are not included in the distribution,
open a terminal, `cd` into the "Cuis-Smalltalk-Dev" directory,
and enter `./clonePackageRepos.sh`.
As of late 2024, this adds the following packages:

- AMQP
- AnimatedGIF
- Cairo
- Calendars
- CodeExamples
- Cuis-Smalltalk
- Cuis-Smalltalk-Historical
- Cuis-Smalltalk-Regex
- Cuis-Smalltalk-Tools
- Cuis-Smalltalk-UI
- Cuis-Website
- EnhancedText
- Erudite
- firmata
- Games
- GeographicInformationSystems
- Learning-Cuis
- Machine-Learning
- Measures
- Morphic
- Numerics
- OSProcess
- Parsers
- StyledTextEditor
- SVG
- VMMaker

These packages are described in the "GitHub Account" section
at the end of the "Overview" chapter.

Another source is packages found in GitHub repositories.
Search for repositories whose names begin with "Cuis-Smalltalk-".
If you create packages to be shared with others,
follow this naming convention.
Clone these repositories into the same directory that holds your Cuis Smalltalk distribution.

To install any of these packages, open a Workspace
and evaluate `Feature require: '{PackageName}'.
