---
title: Kobo Reader
updated: 2019-12-04T12:00:17Z
subtitle: Working with the Kobo Libra H2O
tags: notes, kobo
---

Convert `EPUBs` to `KEPUBs` with [kepubify](https://github.com/geek1011/kepubify).
A `KEPUB` (`.kepub.epub`) allows the Kobo reader to display estimated time remaining among other nice to haves.
More info can be found at the [mobileread wiki](https://wiki.mobileread.com/wiki/Kepub#Kepub_compared_to_ePub).

----

Moving files onto the Kobo.

```bash
sudo pmount /dev/sdx
cp <files> /media/sdx/library
umount /dev/sdx
```
