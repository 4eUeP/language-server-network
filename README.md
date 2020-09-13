lsp-network
===========

A small tool that help you communicate with [language server][lsp]
through network(tcp) instead of stdio.

**N.B. You must keep your server projects synchronously with your local projects,
including directory structure.**

> :star: You can use [docker volumes][docker-volumes] on server to keep the
> same project structure as your locals. And a `rsync` to sync the projects.


## Quickstart

On server:

```sh
lsp-network-server server-config.yaml
```

An example of config file is on `app/server.example.yaml`

On client:

Set your lsp server command as `lsp-network-client` with an extra option
`client-config.yaml`.
Also there is an example of config file is on `app/client.example.yaml`


## TODO

- Enable TLS.
- Enable password proteced connection.
- Logging
- [?] A builtin sync feature.


[lsp]: https://microsoft.github.io/language-server-protocol/
[docker-volumes]: https://docs.docker.com/storage/volumes/
