---
title: Docker + Stack + Symlinks = Errors
published: 2016-04-16 12:00:00
updated: 2016-04-16 12:00:00
subtitle: Finding the problem that causes an 'openBinaryFile' error when running stack build with docker enabled.
tags: stack, docker
---

## The Problem

Using [stack's](http://docs.haskellstack.org/en/stable/README/) awesome [docker integration](http://docs.haskellstack.org/en/stable/docker_integration) I got an unexpected error.

    > stack build
    /home/kondy/.stack/config.yaml: openBinaryFile: does not exist (No such file or directory)

After poking around for a bit I discovered that it was my symlinked [dotfiles](https://github.com/KyleOndy/dotfiles) causing the error, not docker or stack.

Running the build with the `--verbose` command to gain some further insight

    > stack build --verbose
    2016-04-12 15:25:34.077559: [debug] Run process: /usr/bin/docker start -a -i a401a0b1a6c48774f175acf8c2ff83279f0641f5fdb6df62be586b6c7348de0c @(stack_5xoCB8Vl1plFNNNfk8BYdK:System.Process.Run src/System/Process/Run.hs:105:5)

We see the error is proceeded by a `docker start` command.
The *long* command above this in the verbose build output it the `docker create` command, which contains the following volume mount..

    -v /home/kondy/.stack:/home/kondy/.stack

This maps my stack dotfiles to a same folder in stack's docker build container. I have [stack.yaml](https://github.com/KyleOndy/dotfiles/blob/master/apps/stack/.stack/config.yaml) symlinked to ~/.stack.

    lrwxrwxrwx  1 kondy   56 Apr 12 15:22 config.yaml -> /home/kondy/src/dotfiles/apps/stack/config.yaml

Checking the mounted `/home/kondy/.stack` folder from within docker shows `config.yaml` again symlinked to `/home/kondy/src/dotfiles/apps/stack/config.yaml`, which doesn't not exist in the docker container.

## Work Around

For now I have simply remove the symlink from my dotfiles. The files changes infrequently enough to make this a passable solution for now. This is a known limitation of docker and is not planned to be fixed in the future.
