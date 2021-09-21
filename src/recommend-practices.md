---
eleventyNavigation:
  key: Recommended Practices
layout: topic-layout.njk
---

## Commits

Before committing code, examine diffs for all the modified files.
This is especially easy to do in VS Code.
Look for debugging code that should be removed such as `console.log` calls.
Verify that the code quality is something you can be proud to show others.

## Pull Requests

After creating a pull request and before requesting reviewers,
examine all the code changes.
In GitHub, click the "Files changed" tab and scroll through all the changes.
Ask yourself if reviewers will understand why you made each of the changes.
If you suspect they may not, add comments.
One way is to add comments directly in the PR
by clicking to the right of a line number.
For comments that should live with the code, add them in the source files,
commit the changes, and push to the current branch.
Once you believe reviewers will understand the changes,
perhaps aided by your comments, request reviewers.

## Sprints

Add a story in each sprint to review and fix issues found by testers.
Testers can be enter these as GitHub issues the repository.
An estimate of 1.5 days is typically sufficient.
Focusing on new issues as they are reported can be a distraction.
Don't focus on the issues until other stories in the sprint have been completed.
