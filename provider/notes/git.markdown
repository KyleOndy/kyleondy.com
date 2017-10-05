---
title: Git
published: 2015-11-20 12:00:00
updated: 2016-06-12 12:00:00
subtitle: Less commonly used commands. Not really.
tags: notes, git
---

# Some notes below on git.

----

Push to multiple remotes with a single push. `git remote set-url --add --push <remote name> <newurl>`{.bash}

----

Add forgotten files to last commit: `git commit --amend -C HEAD`{.bash}

----

Decide last n commits should be on a different branch. [Stackoverflow](https://stackoverflow.com/questions/1628563/move-the-most-recent-commits-to-a-new-branch-with-git)

```{.bash}
git branch newbranch
# can also git reset --hard <hash>
git reset --hard HEAD~3 # Go back 3 commits. You *will* lose uncommitted work.
git checkout <newbranch>
```

---

Rewrap text to `textwidth`: <kbd>g</kbd><kbd>q</kbd>
