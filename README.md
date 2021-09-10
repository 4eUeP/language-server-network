# Language-Server-Network

A small tool that help you communicate with [language server][lsp] through
network(tcp) instead of stdio.

## Motivation

Do development on remote machines, but use local file editors.

## Quickstart

**TODO**

**N.B. You must keep your server projects synchronously with your local
projects, including directory structure.**

> :star: You can use [docker volumes][docker-volumes] on server to keep the same
> project structure as your locals. And a SimpleFileSync to sync your projects.

Setup file-sync:

```
simple-file-sync /path/to/your/config.yaml
```

An example config file:
[simple-file-sync.yaml](./FileSystem/config/simple_file_sync.yaml)

On server:

```sh
lsp-network-server --host 0.0.0.0 --port 3001
```

On client, set your lsp-client plugin:

```sh
lsp-network-client --host <your-server-ip> --port 3001 --projects lsp-network-client.yaml
```

## TODO

- Enable TLS.
- Enable password proteced connection.
- [?] A builtin sync feature.

[lsp]: https://microsoft.github.io/language-server-protocol/
[docker-volumes]: https://docs.docker.com/storage/volumes/
