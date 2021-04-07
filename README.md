# What is it?
This tool get the Jenkins plugins database and desired plugins list as an input and will try to resolve all dependencies for all plugins

# How to build?
Just use a `cabal build` to build an executable

# How to use?
  1. Prepare plugins json
```sh
$ curl --compressed -L https://updates.jenkins.io/current/plugin-versions.json \
  | jq -r '.plugins | to_entries[] | .value | to_entries | map (.value)' \
  | jq -s '. | flatten' > plugins.json
```
  2. Define your plugins list
```sh
$ cat ./plugins.list
active-directory:2.22
ansible:1.1
ansicolor:0.7.3
authentication-tokens:1.4
authorize-project:1.3.0
branch-api:2.6.2
build-token-root:1.7
command-launcher:1.5
conditional-buildstep:1.4.0
configuration-as-code:1.46
credentials:2.3.14
credentials-binding:1.24
display-url-api:2.3.4
docker-workflow:1.25
echarts-api:4.9.0-2
email-ext:2.82
job-dsl:1.77
git:4.5.0
```
  3. run application with a target version of Jenkins core as a parameter
```sh
$ autojen-plugins-solver --jenkins-version 2.263.1
```
