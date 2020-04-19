const core = require('@actions/core');

// Can access information about the
// GitHub repository that is using this action.
//const github = require('@actions/github');

const days = [
  'Sunday',
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday'
];

try {
  const dateString = core.getInput('date');
  const date = dateString === 'today' ? new Date() : new Date(dateString);
  const dayOfWeek = days[date.getDay()];
  console.log(dateString, 'is on a', dayOfWeek);
} catch (error) {
  core.setFailed(error.message);
}
