const core = require('@actions/core');

// Can access information about the
// GitHub repository that is using this action.
//const github = require('@actions/github');

try {
  const date = core.getInput('date');
  console.log('date =', date);
} catch (error) {
  core.setFailed(error.message);
}
