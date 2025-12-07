---
eleventyNavigation:
  key: Markdown
layout: topic-layout.njk
---

### Overview

Markdown is a syntax for specifying HTML in a simpler way.
It was created by John Gruber and is defined {% aTargetBlank
"https://daringfireball.net/projects/markdown/", "here" %}.

There are many Markdown parsers.
The one that 11ty uses by default is {% aTargetBlank
"https://github.com/markdown-it/markdown-it", "markdown-it" %}.

### Syntax

Let's review the most commonly used parts of Markdown syntax.

- **headers**: `#` (h1) to `######` (h6)
- **italic**: `*text*` or `_text_`
- **bold**: `**text**` or `__text__`
- **bold and italic**: `**_text_**` or `__*text*__`
- **strikethrough**: `~~text~~`
- **block quotes**: precede each line with `>`, text is automatically wrapped
- **code in a line**: surround with single backticks
- **code on multiple lines**: surround with lines containing three backticks;
  follow first line of backticks with a language identifier; ex. `---js`
- **horizontal rule**: three or more hyphens, asterisks, or underscores;
  each produces the same result
- **HTML**: can include most HTML tags
- **image**: `![alt-text](url)`
- **image with tooltip**: `![alt-text](url "tooltip")`
- **link displaying url**: `<url>`
- **link displaying text**: `[text](link-url)`
- **link displaying an image**: `[![alt-text](image-url)](link-url)`
- **link displaying text with tooltip**: `[text](link-url "tooltip")`
- **unordered list**: precede each list item with `*`, `-`, or `+`
- **ordered list**: precede each list item with `1.`
  or use a specific number to begin there

> lists of any type can be nested by indenting two spaces

- **tables**

  ```text
  | Heading 1 | Heading 2 | Heading 3 | Heading 4 |
  | --------- | :-------- | --------: | :-------: |
  | data 1-1  | data 1-2  |  data 1-3 | data 1-4  |
  | data 2-1  | data 2-2  |  data 2-3 | data 2-4  |
  ```

  Colons in second line indicate horizontal alignment of a column.
  Column 1 is left-aligned by default.
  Column 2 is explicitly left-aligned.
  Column 3 is right-aligned.
  Column 4 is centered.

  Even if the column values are not aligned in the Markdown,
  a properly aligned table will be produced.
  Each heading must have at least three dashes below it.
  The outer pipes can be omitted.
  The VS Code markdownlint extension automatically fixes all these issues.

- **escaping**: precede characters with `\`

### GitHub extensions

- @ mention: `@github-username`
- emoji: `:emoji-name:`;
  examples include `:+1:` (thumbs up), `:tada:` (party hat), and `:rocket:`
- issue reference: `#issue-number`
- task list:
  - `- [ ] some uncompleted task`
  - `- [x] some completed task`

### Enabling HTML

Markdownlint reports errors for HTML tags in Markdown files.
It also reports an error if the first line in the file is not an `h1`,
represented by starting with `#` in Markdown.
These can be disabled by creating the file `.markdownlint.json`
containing the following:

```json
{
  "default": true,
  "MD033": false,
  "MD041": false
}
```
