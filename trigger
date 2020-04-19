# This doesn't currently work.  It gives a "not found" error from the POST.

set token (jq .gitHubAccessToken secrets.json)

curl -X POST https://api.github.com/repos/mvolkmann/blog/dispatches \
  -H 'Accept: application/vnd.github.everest-preview+json' \
  -H 'Authorization: token ${token}' \
  --data '{"event_type": "manual_trigger"}'
