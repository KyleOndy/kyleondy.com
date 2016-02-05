---
title: Speeding up Haskell builds on CircleCI
tags: haskell, CI
published: 2015-12-14
author: Kyle
excerpt: Substantially faster Haskell build on CircleCI with Stack
---

I've gotten so used to how awesome continuous integration is I don't think I could live without it anymore.
Unfortunately building Haskell projects was becoming a pain since everything had to be rebuilt each commit.

Now using [Circle CI](https://circleci.com/) along with [stack](http://docs.haskellstack.org/en/stable/README.html) allows projects to be built in the fraction of the time.

![CircleCI speedup](/images/circle-ci-speedup.png)

As of commit [60ee51b](https://github.com/KyleOndy/kyleondy.com/blob/60ee51b606a181e584ef905fde8520d9d1c45d59/circle.yml) my circle.yml file looks like this:


This is the configuration file for circleci.com (CI which builds the site)

    # Manually install to avoid install conflicts
    # cf. https://github.com/jaspervdj/hakyll/issues/340#issuecomment-96101869
    dependencies:
      cache_directories:
        - "~/.stack"
      pre:
        - wget https://github.com/commercialhaskell/stack/releases/download/v0.1.10.0/stack-0.1.10.0-linux-x86_64.tar.gz -O /tmp/stack.tar.gz
        - tar -zxvf /tmp/stack.tar.gz --strip 1 -C /tmp/
        - ls -laxo /tmp
        - chmod +x /tmp/stack
        - sudo mv /tmp/stack /usr/bin/stack
      override:
        - stack setup
        - stack build

    general:
      branches:
        ignore:
          - compiled-html # actaul website content

    # Compile pages during the test stage
    test:
      override:
        - make check


## Breaking it down ##

This section tells circleCI to keep this directory around build to build.


    dependencies:
      cache_directories:
        - "~/.stack"

This downloads the latest stack release to /tmp of the build container, extracts the package to /tmp, copies the executable to the bin directory, and sets the executable bit on stack.


    pre:
      - wget https://github.com/commercialhaskell/stack/releases/download/v0.1.10.0/stack-0.1.10.0-linux-x86_64.tar.gz -O /tmp/stack.tar.gz
      - tar -zxvf /tmp/stack.tar.gz --strip 1 -C /tmp/
      - ls -laxo /tmp
      - chmod +x /tmp/stack
      - sudo mv /tmp/stack /usr/bin/stack

Make sure GHC and all packages declared in .cabal are built.


    override:
      - stack setup
      - stack build
