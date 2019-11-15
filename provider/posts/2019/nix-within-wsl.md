---
title: Nix within WSL
Tags: nix, wsl
created: 2019-08-04T19:15:54Z
author: Kyle Ondy
subtitle: The comfort of Nix, even within Windows
---

# Installing Nix in WSL (Ubuntu)

Attempting to install nix while in WSL returns some obtuse errors.

## Missing cgroups support

The Nix sandbox relies on container functionality that Windows has yet to  implement in the WSL abstraction layer.
If you do not disable the sandbox the follow error occurs.

```bash
error: cloning builder process: Invalid argument
error: unable to start build process
```

## Incorrect file locking

The lock handling in Windows NT doesn't perfectly match Linux's.
While SQLite works fine under the Win32 personality, it produces deadlocks when you try to install Nix, producing this infinite warning loop:

```bash
warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)
warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)
warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)
warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)
warning: SQLite database '/nix/var/nix/db/db.sqlite' is busy (SQLITE_PROTOCOL)
```

## Installing Nix

Adding the following to `/etc/nix/nix.conf` before  installing nix allows for workarounds the allow a successful installation.

```bash
mkdir -p /etc/nix/
cat << EOF > /etc/nix/nix.conf
# Work around missing cgroups support
# https://github.com/Microsoft/WSL/issues/994
sandbox = false

# Work around incorrect file locking
# https://github.com/Microsoft/WSL/issues/2395
use-sqlite-wal = false
EOF
```

Now Run the Nix Package Manager Install script and you will have a successful install.
