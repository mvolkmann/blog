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

Before creating a pull request, verify that linting, code formatting,
unit tests, and end-to-end tests all run cleanly.
Consider creating an npm script that runs all of these.

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
Testers should enter these as GitHub issues in the repository.
An estimate of 1.5 days is typically sufficient.
Focusing on new issues as they are reported can be a distraction.
Don't fix the issues until other stories in the sprint have been completed.

## Fixing Issues

When addressing a GitHub issue, if it cannot be fixed yet
because there are questions that must be answered,
add the questions in issue comments.
Also edit the the issue title to begin with "Q: ".
If you know who needs to answer the question, begin with "Q for {name}: ".
This enables the team to see at a glance all the issues
that are blocked due to questions.

When an issue is fixed, add an issue comment that says the issue is fixed
and include the branch name that contains the fix.
Also edit the issue title to begin with "Fixed: ".
This enables testers to see at a glance which issues have fixes to be tested.
However, they shouldn't test the fix until the branch containing it
is merged to the "develop" branch.
