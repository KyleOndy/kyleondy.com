---
title: A faster way to amend a git commit
Tags: git
created: 2020-05-09T10:43:58Z
author: Kyle Ondy
subtitle: Making life in the shell a bit nicer
---

tldr: `git amend-to <REF>`.

## The problem

The way I've grown to use git is to iteratively build up a collection, typically 2 to 4, commits as I work.
I know the goal I am working towards may be best describes as a few distinct commits.
This work flow requires me to amend commits behind `HEAD` fairly often.
In the past I had two ways of achieving this.

The first method I've used since my early days of git is to use `git stash` and interactive rebase.
This way does work, but involves a fair amount of trying and ruins any _flow_ that you may have had going.

```bash
git add <chagens to be amended>
git stash
git rebase -i <some REF before the commit to be amended>
# < mark commit for editing >
git stash pop
git commit --amend
git rebase --contine
```

This is cumbersome.
This is slow.
It feels like there should be a better way.

A few months ago I decided there must be a better way.
I began to just mark the changes I wanted to amend to a previews commit as a fixup be making a quick `git commit -m "f: <some description that lets me know what is being fixed up>"`.
I could continue to do this, working for a while without ever having my find drift to far.
Then after a while I could do another interactive rebase _and_ if I was good with naming everything, could reorder the commits and set the commits starting with `f:` to be fixedup.

This worked ok, sometimes, until it didn't.
I would forget which commit each fixup was associated with, causing me to end up with commits that were not atomic.

This work flow failed me due to my inability to reconcile conflicts.
Some commit that was being fixed up would have a conflict, and it would always seem to snowball out of control.

## The quest for a better amend

I knew there had to be a better way.
This past weekend I set some time aside to explicitly look for a solution to the problem.

As I alluded to above my work flow is now as follows.

```bash
git add <changes to be amended to a commit>
git amend-to <^y> # hit control + y on the keyboard
# choose a commit from a list
> git amend-to abcde123
# commit has been amended to
```

This magic is a combination of a git alias and a zsh widget.

### the git alias

Here is the alias `amend-to` that I use.

```
amend-to = "!f() { git commit --fixup \"$1\" && GIT_SEQUENCE_EDITOR=true git rebase --interactive --autosquash \"$1^\";}; f"
```

The magic of this alias is the `GIT_SEQUENCE_EDITOR`.
The excerpts of the man pages are duplicated below.
Committing with `--fixup` words the commit to be compatible with `--autosquash`.
Adding `--autosquash` will reorder the rebase todo list and place the fixup's in the proper place.

This alone would be a significant improvement to the work flow.

Setting `GIT_SEQUENCE_EDITOR=true` will suppress the instruction sheet completely by exiting immediately with an exit code of 0.
This provides a zero interaction amend to any commit, assuming no conflicts.
Now if a conflict does arise only dealing with a single change is easer to shepard though.

```bash
> man git-config
-----8<------
sequence.editor
  Text editor used by git rebase -i for editing the rebase instruction file.
  The value is meant to be interpreted by the shell when it is used. It can be
  overridden by the GIT_SEQUENCE_EDITOR environment variable. When not
  configured the default commit message editor is used instead.
-----8<------
```

```bash
> man git-commit
--fixup=<commit>
  Construct a commit message for use with rebase --autosquash. The commit
  message will be the subject line from the specified commit with a prefix of
  "fixup! ". See git-rebase(1) for details.
```

```bash
> man git-rebase
--autosquash, --no-autosquash
  When the commit log message begins with "squash! ..." (or "fixup! ..."), and
  there is already a commit in the todo list that matches the same ...,
  automatically modify the todo list of rebase -i so that the commit marked for
  squashing comes right after the commit to be modified, and change the action
  of the moved commit from pick to squash (or fixup). A commit matches the ...
  if the commit subject matches, or if the ...  refers to the commit’s hash. As
  a fall-back, partial matches of the commit subject work, too. The recommended
  way to create fixup/squash commits is by using the --fixup/--squash options of
  git-commit(1).
```

This alias is fully functionally.
You do need to look up commit hashes manually.
You need to be constancy referencing `git log` and copy and pasting.
With the history being rewritten you are unable to rerun the same command from history to edit the same logical commit.

### The zsh widget

To avoid all this work, I've written a small zsh widget that allows me to interactively select a commit form a list after hitting `<ctrl>y`.
I've been a big fan of [fzf](https://github.com/junegunn/fzf) using it for finding file within vim since at least [Feb 2006](https://github.com/KyleOndy/dotfiles/commit/f849fc3e3287db3a879117f2fad7dc428c49f347#diff-add39464403a266c831c649bca9a5732R35).
By piping in the results of `git log` into `fzf` I can easily select a commit to amend to.

```zsh
fzf_pick_git_commit() {
  # I've already put a lot of time into my `git lg` alias an am very
  # familiar with how it looks. I am just reusing most of the logic here.
  # I force `--color` becuase git will output this without color by
  # default.
  LOG_LINE=$(git log --color --pretty=format:'%Cred%h%Creset -%G?-%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' | fzf --ansi)
  # as the git commit ref is the first item in the line, split at the
  # first space and take the first argument.
  echo -n "$LOG_LINE" | cut -d' ' -f1 | tr -d $'\n'
}
```

This zsh function just echo's out the git REF, which is not quite what I need.
I need that value to be inserted into the ZSH input environment.
This is done by adding the commit value to the end of the cursor position by appending to `LBUFFER`.

```zsh
_zle_pick_git_commit() {
  COMMIT=$(fzf_pick_git_commit)
  LBUFFER=$LBUFFER$COMMIT
}
```

To enable the zsh binding I create a zsh widget.
```zsh
zle -N _zle_pick_git_commit
bindkey '^y' _zle_pick_git_commit
```

## All together

A nicer way to work.

## External resources

Like almost everything I do, its on the shoulders of giants.
Here are some posts I discovered that led me to my final solution, in no specific order.

- [Stack Overflow: Git alias with positional parameters](https://stackoverflow.com/questions/3321492/git-alias-with-positional-parameters)
- [A closer look at the zsh line editor and creating custom widgets](https://sgeb.io/posts/2014/04/zsh-zle-custom-widgets/)
- [[web.archive.org]: how-to: run git interactive rebase non-interactively](https://web.archive.org/web/20210122084813/http://lowlevelmanager.com/2016/02/how-to-run-git-interactive-rebase-non.html)
