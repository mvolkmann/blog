---
eleventyNavigation:
  key: ESLint
layout: topic-layout.njk
---

ESLint is, as the website describes,
the "pluggable linting utility for JavaScript and JSX."
Linters report many syntax errors and potential run-time errors.
ESLint also reports deviations from specified coding guidelines.
Error messages identify the violated rules, making it easy to
adjust their configuration if you disagree with the defaults.

Languages that require compilers have a way to
give feedback to developers before code is executed.
Since JavaScript doesn't require a compiler,
linters are needed to play that role.

Alternatives to ESLint exist, such as JSLint and JSHint,
but none compare in terms of the number of rules supported,
ability to configure the rules, and ability to add custom rules.

While you can write JavaScript code without using a linter,
that's a bit like playing football without a helmet
or taking food out of an oven without mitts.
Somebody is going to get hurt.

See my Object Computing (OCI) article {% aTargetBlank
"https://objectcomputing.com/resources/publications/sett/january-2017-eslint-dont-write-javascript-without-it",
"ESLint, Don't Write JavaScript Without It!" %}.
