---
title: Git
published: November 20, 2015
excerpt: Less commonly used commands
tags: notes, git
---

### Some notes below on git.

Add forgotten files to last commit: `git commit --amend -C HEAD`{.bash}

----

Decide last n commits should be on a different branch. [Stackoverflow](https://stackoverflow.com/questions/1628563/move-the-most-recent-commits-to-a-new-branch-with-git)

```{.bash}
git branch newbranch
# can also git reset --hard HASH
git reset --hard HEAD~3 # Go back 3 commits. You *will* lose uncommitted work.
git checkout newbranch
```
