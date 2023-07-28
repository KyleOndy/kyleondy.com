---
title: Debian
updated: 2018-10-23T21:08:06Z
subtitle: man debian
tags: notes, debian
---

Upgrading Debian from one release to another.

~~~bash
sed -i 's/stretch/buster/g' /etc/apt/sources.list
~~~

Once `/etc/apt/sources.list/etc/apt/sources.list` is updated, update the running system.

~~~bash
apt update
apt upgrade
apt dist-upgrade
~~~
