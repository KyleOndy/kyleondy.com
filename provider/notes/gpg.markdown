---
title: GPG
published: November 28, 2016
updated: 2017-02-03 
subtitle: Staying secure
tags: notes, gpg
---

Some notes below on GPG.

# Cache smartcard pin

While this is a little less secure, I toggle power to my smartcard when I walk alway from my desk, resetting the pin
```
gpg2 --card-edit
> admin
> forcesign
```
