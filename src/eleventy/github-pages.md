---
eleventyNavigation:
  key: GitHub Pages
  order: 6
  parent: Eleventy
layout: topic-layout.njk
---

GitHub Pages is a great option to consider for hosting an 11ty site
because it is free and simple to use.

Here are the steps to create and deploy an 11ty site to GitHub Pages.

1. Install Node.js if not already installed.
   See [nodejs.org](http://nodejs.org).
1. Install git if not already installed.
   See <https://git-scm.com/book/en/v2/Getting-Started-Installing-Git>.
1. Create a GitHub account if you don't already have one.
   Browse <https://github.com/> and click the "Sign up" button.
1. Create a new GitHub repository.
1. Clone the repository to create a local repository.
1. cd to the directory of the local repository.
1. Enter `npm init -y` to create a `package.json` file.
1. Enter `npm install -D @11ty/eleventy gh-pages npm-run-all`.
   The installed `gh-pages` command creates a Git branch named "gh-pages"
   that contains only a given directory, in this case the
   "\_site" directory created by the `eleventy` command.
1. Create a `.gitignore` file containing the lines `/node_modules` and `/_site`.
1. Edit `package.json` and replace the "test" script with the following
   npm scripts, most of which are only conveniences:

   ```json
   "add": "git add .",
   "all": "npm-run-all build add commit push deploy",
   "build": "eleventy",
   "clean": "rm -rf _site",
   "commit": "git commit -av",
   "deploy": "gh-pages -d _site",
   "push": "git push origin master",
   "start": "eleventy --serve"
   ```

1. Create the directory `_includes`.
1. Add the file `layout.md` in the `_includes`
   directory containing the following:

   {% raw %}

   ```html
   <html lang="en">
     <body>
       {{content | safe}}
     </body>
   </html>
   ```

   {% endraw %}

1. Create the file `index.md` file containing the following:

   ```html
   ---
   layout: layout.md
   ---

   # Welcome to my site!
   ```

1. To build and test the site locally, enter `npm start`.
1. To build and deploy the site, enter `npm run all`.
   This will prompt for a commit message that must be entered using Vim.
   Press the "i" key to go into insert mode.
   Type a commit message.
   Press the esc key to exit insert mode.
   Press ":" to go into command-line mode.
   Type "wq" and press return to write the changes and quit.
1. Browse the web UI for the GitHub repository, not the deployed site.
1. Click "Settings" near the upper-right.
1. Scroll to the "GitHub Pages" section.
1. Under "Source", "gh-pages branch" should already be selected.
1. Click the link after "Your site is ready to be published at"
   to view the site.
1. Considering bookmarking this URL.

To make local changes to the site:

1. Edit site source files.
1. Start the local eleventy server if it isn't already running
   by entering `npm start`.
   This will provide hot reload when changes are detected.
1. Enter `npm run all` to rebuild and redeploy the site.
   This will prompt for a commit message that must be entered using Vim.
1. Browse the site to verify the changes.
   In can take 30 seconds or more for changes to appear on the site.
   Running deployments can be viewed at
   <https://github.com/{username}/{repo-name}/deployments>.

To make changes to the site from the GitHub web UI
and automatically build and deploy the site:

1. Configure GitHub Actions to do the build and deploy on every push.
   See <https://github/mvolkmann/blog/github/actions>.
1. Browse the GitHub repository.
1. Click the "Code" tab.
1. Navigate to a file to edit.
1. Click the pencil icon in te upper-right of the file content.
1. Make changes.
1. Enter a commit message.
1. Optionally enter an extended description.
1. Press the "Commit changes" button.
