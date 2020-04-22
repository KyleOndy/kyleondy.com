---
title: Make
updated: 2020-04-22T07:42:44Z
subtitle: Info about make
tags: notes, make
---

Much of this content is from the [GNU Make Book](https://nostarch.com/gnumake).

Variable precedence: `override` > command line > environment overrides (`-e`) > variables defined in the makefile

Variables

use `?=` for flexibility

`BUILD_DEBUG ?= yes`
