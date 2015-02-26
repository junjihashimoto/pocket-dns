# PocketDNS: Multi-backend (zookeeper and sqlite) DNS Server using persistent-library

[![Hackage version](https://img.shields.io/hackage/v/pocket-dns.svg?style=flat)](https://hackage.haskell.org/package/pocket-dns)  [![Build Status](https://travis-ci.org/junjihashimoto/pocket-dns.png?branch=master)](https://travis-ci.org/junjihashimoto/pocket-dns)

PocketDNS is multi-backend (zookeeper and sqlite) DNS Server using persistent-library.

## Getting started

Install this from Hackage.

    cabal update && cabal install pocket-dns

## Usage

Set conf.yml which is backend settings.
When backend is zookeeper, conf.yml's format is below.

```
backend: zookeeper
coord: localhost:2181/
timeout: 300000
num-stripes: 1
idletime: 300000
max-resource: 30
```

When backend is sqlite, conf.yml's format is below.

```
backend: sqlite
database: pocket-dns.sqlite3
poolsize: 10
```

Then launch dns-server and set domain and ip-address.

```
pocket-dns daemon &
pocket-dns set <domain>. <ip-address>
```

When domain is not found, pocket-dns checks '/etc/resolv.conf'.

## Commands

### Set

Set ip-address of domain

```
pocket-dns set <domain-name>. <ip-address>
```

### get

Show ip-address of domain

```
pocket-dns get <domain-name>.
```

### list

Show all domain and ip-address

```
pocket-dns list
```

### delete

Delete domain

```
pocket-dns delete <domain-name>.
```

### daemon

Launch dns-server

```
pocket-dns daemon
```
