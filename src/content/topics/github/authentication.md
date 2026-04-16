---
eleventyNavigation:
  key: GitHub Authentication
  parent: GitHub
  title: Authentication
layout: topic-layout.njk
---

When a repository is cloned using SSH,
the SSH key associated with the account is used for authenication
when operations such as a "push" are performed.

When a repository is cloned using HTTPS,
you will be prompted for a username and personal access token
every time such an operation is performed.
To avoid this, install the GitHub CLI command "gh".
Then enter "gh auth login" on time and enter the information requested.
After this you will no longer be prompted to authenticate.