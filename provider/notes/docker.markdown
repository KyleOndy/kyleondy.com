---
title: Docker
published: 2016-04-13
excerpt: Quick notes on common docker commands
tags: notes, docker
---

Some notes below on docker.

# Volume Containers

Copying files from current directory into a volumes container.

```{.bash}
# if volume container is not yet created
docker create -v $DATA_DIR --name $DATA_CONTAINER_NAME $IMAGE /bin/true

# example for my znc contianer
docker create -v /znc-data --name znc-data kyleondy/znc /bin/true

# copy the files in
docker run --rm --volumes-from $VOLUME_CONTAINER -v $(pwd):/move --entrypoint /bin/sh {$IMAGE} -c "cp -av /move/* $DESTINATION"
```

## Example of moving files into my znc container

```{.bash}
> tree .
.
├── configs
│   └── znc.conf
└── modules
    └── push.cpp

# $VOLUME_CONTAINER=znc-data
# $IMAGE=kyleondy/znc
# $DESTINATON=/znc-data
docker run --rm --volumes-from znc-data -v $(pwd):/move --entrypoint /bin/sh kyleondy/znc -c "cp -av /move/* /znc-data"
```
