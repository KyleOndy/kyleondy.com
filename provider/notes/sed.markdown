---
title: sed
updated: 2017-10-04T12:00:00Z
subtitle: Commandline subsitution
tags: notes, sed
---

Find and replace a string, with a backup, because mistakes happen.

~~~{.bash}
sed -i.sedbak -- 's/foo/bar/g' foobar.txt
~~~

If working with file paths, use a different separator character.

~~~{.bash}
sed -i.sedbak -- 's|/home/foo/|/home/bar|g' foobar.txt
~~~
