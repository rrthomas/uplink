Deploying Uplink to Google Cloud
================================

### Prerequisites:

1) Install the latest version of the [Google Cloud
SDK](https://cloud.google.com/sdk/docs/quickstarts)

2) Install `kubectl`:
```$ gcloud components install kubectl```

3) Authorize your gcloud SDK to talk to Adjoint's gcloud account:
```
$ gcloud auth login                      # gives access to compute engine
$ gcloud auth application-default login  # gives access to compute cluster
```

See: [Deployment docs](https://www.adjoint.io/docs/deployment.html)
