---
title: Grep
updated: 2020-12-01T08:54:58Z
subtitle: Collection of all the issues I've ever had
tags: notes, issues
---

## Non $JOB Specific Issues


Problem:
```
zsh compinit: insecure directories, run compaudit for list.
Ignore insecure directories and continue [y] or abort compinit [n]?
```

Cause:

The root cause of "insecure" is these folders are group writable.
[source](https://unix.stackexchange.com/a/412817)

Fix:
```
compaudit | xargs chmod g-w
```
