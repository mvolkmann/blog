token=f5443956b3baec7bf1a55f75d74b471fa6084c15
curl -X POST https://api.github.com/repos/mvolkmann/blog/dispatches \
  -H 'Accept: application/vnd.github.everest-preview+json' \
  -H 'Authorization: token ${token}' \
  --data '{"event_type": "manual_trigger"}'
