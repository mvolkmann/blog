---
eleventyNavigation:
  key: ESLint and Prettier with Svelte
  parent: Svelte
layout: topic-layout.njk
---

Here is my typically setup for using ESLint and Prettier with Svelte.

```bash
npm install -D eslint eslint-plugin-import eslint-plugin-svelte3
npm install -D prettier prettier-plugin-svelte
```

Create the file `.eslintrc.json` in the project root directory
containing the following:

```json
{
  "env": {
    "browser": true,
    "es6": true,
    "jest": true,
    "node": true
  },
  "extends": ["eslint:recommended", "plugin:import/recommended"],
  "overrides": [
    {
      "files": ["**/*.svelte"],
      "processor": "svelte3/svelte3"
    }
  ],
  "parserOptions": {
    "ecmaVersion": 2019,
    "sourceType": "module"
  },
  "plugins": ["import", "svelte3"]
}
```

Create the file `.prettierrc` in the project root directory
containing the following:

```text
{
  "arrowParens": "avoid",
  "bracketSpacing": false,
  "singleQuote": true,
  "svelteSortOrder": "options-scripts-markup-styles",
  "trailingComma": "none"
}
```

Create the file `.prettierignore` in the project root directory
containing the following:

```text
public/build
```

Add the following scripts in `package.json`:

```json
  "format": "prettier --write '{public,src}/**/*.{css,html,js,svelte}'",
  "lint": "eslint --fix --quiet src --ext .js,.svelte",
```

To run ESLint, enter `npm run lint`.

To run Prettier, enter `npm run format`.
