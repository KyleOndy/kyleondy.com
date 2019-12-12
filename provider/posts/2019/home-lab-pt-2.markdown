---
title: Home Lab - Bootstrapping
Tags: homelab
created: 2019-12-11T22:24:37Z
author: Kyle Ondy
subtitle: Working out the kinks.
---

One of the most common and insidious issues with systems is configuration drift.
In a home lab without the controls in place that an enterprise or real business demands this drift happens before you realize.
One of the best ways to ensure minimal drift is to minimize the effort needed to rebuild the system in question.

Applying your configuration management to a fresh image verifies that you are able to rebuild your environment as needed.
Using a combination of [pixiecore](https://github.com/danderson/netboot/tree/master/pixiecore) and [waitron](https://github.com/ns1/waitron) I am able to choose if each board should have its OS reinstalled when it boots into the PXE environment.

The RaspberryPi setup was fairly straightforward following [the offical documentation](https://www.raspberrypi.org/documentation/hardware/raspberrypi/bcm2711_bootloader_config.md).

The [ODRIOD-H2](https://www.hardkernel.com/shop/odroid-h2/) _should_ be able to follow the same workflow.
However, there seems to be a bug in the H2's BIOS.
I've made a post for further clarification on the [official forums](https://forum.odroid.com/viewtopic.php?f=173&t=37071).
