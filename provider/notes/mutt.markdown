---
title: Mutt
published: 2016-07-25 12:00:00
updated: 2016-07-26 12:00:00
subtitle: Quick notes on setup
tags: notes, mutt
---

Some notes below on compiling and setting up mutt.

Use the [sidebar patch](http://www.lunar-linux.org/mutt-sidebar/).

On ubuntu 16.04:
```bash
cd ~/src
wget -q -O - ftp://ftp.mutt.org/pub/mutt/mutt-1.5.24.tar.gz | tar xvfz -
cd ./mutt-1.5.24
wget -q -O - http://lunar-linux.org/~tchan/mutt/patch-1.5.24.sidebar.20151111.txt | patch -p1
sudo apt install libssl-dev libsasl2-dev libdb-dev
./configure --enable-imap --enable-hcache --with-ssl --with-sasl
make
sudo make install
```
