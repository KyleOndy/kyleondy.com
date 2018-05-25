---
title: Bash
updated: 2018-05-03T20:21:34Z
subtitle: common shell notes
tags: notes, bash
---

# Line editing

Should work in bash and zsh, and in some readline environments.

~~~{.bash}
^u # delete everything before the cursor
^k # delete everything after the cursor
^h # delete one character before the cursor
^d # delete character after the cursor
^w # delete from cursor to the start of previous word
~~~

# Flow Control

If / else if / else

```bash
if [ "$foo" -eq 0 ]; then
  echo "true"
elif [ "$foo -gt 0 ]
  echo "false"
else
  echo "maybe"
fi
```

For loop.

```bash
for f in ${SET}; then
  echo "${f}"
fi
```
