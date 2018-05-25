---
title: Git
updated: 2017-10-09 11:51:00
subtitle: Commands I always seem to forget
tags: notes, git
---

Push to multiple remotes with a single push.

``` bash
git remote set-url --add --push <remote> <newurl>
```

----

Delete branch from remote.

~~~{.bash}
# version >= 1.7.0
git push <remote> --delete <branch>

# version < 1.7.0
git push <remote> :<branch>
~~~

----

Add forgotten files to last commit.


~~~{.bash}
git commit --amend -C HEAD
~~~


----

Decide last *n* commits should be on a different branch. [Stackoverflow](https://stackoverflow.com/questions/1628563/move-the-most-recent-commits-to-a-new-branch-with-git)

```{.bash}
git branch newbranch
# can also git reset --hard <hash>
git reset --hard HEAD~3 # Go back 3 commits. You *will* lose uncommitted work.
git checkout <newbranch>
```

---

Rewrap text to `textwidth`: <kbd>g</kbd><kbd>q</kbd>
