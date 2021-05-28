---
title: Home Lab - Part 1
Tags: homelab
created: 2019-12-03T16:39:00Z
author: Kyle Ondy
subtitle: 24cores and 108GB of memory
---

My job affords me the opportunity to run complex workloads on some industry standard platforms.
These are production workloads that require the while enterprise architecture with all the high availability and performance replica bells and whistles.
This is all done on their dime and I have pretty deep knowledge on this _specific_ stack.

I want to expand my working knowledge of workload clustering and orchestration beyond what I do at my day job.
It has never been easier to get hand on experience with clustered computing then it is now with a quick search for `learn fizzbuzz the {easy,hard} way`.
A few clicks in the web console or cli commands and you have a cluster up and running in your choice of cloud provider.

This is great as long as you only want these resources around long enough to learn.
If you wanted to run these resources around the clock, it will get *expensive*.
To save some costs you can run these same systems on a local machine via a hypervisor.
You miss out on some provider niceties such as the SDN and security primitives, but they are ancillary to the core technology.

These are all sane and prudent choices.
I choose another option.
I built a physical lab to learn clustering.
Many people do this, throwing together a few Raspberry Pi's and getting a low powered cluster up they can limp along and learn with.
The advent of the [RaspberryPi 4](https://www.raspberrypi.org/products/raspberry-pi-4-model-b/) 4GB model allows for building a cluster than can handle a more realistic workload.

I built myself a cluster of three new RaspberryPi 4GN boards providing me 12 core and 12GB of RAM of ARM power.
This is some decent power, but I did not stop here.
This three node cluster is reserved for running masters and controllers for services.

I also added in three [ODRIOD H2s](https://www.hardkernel.com/shop/odroid-h2/) for 12 more x86 cores and another 96GB of memory. This six node cluster should allow me to expand beyond proof of concepts and "hello worlds" and host some real services.

The long term plan is to self host and expose these services via a DMZ next to my home network.

This post will contain an up to date list of posts that go into more detail

- [Bootstrapping](/posts/home-lab-bootstrapping)
- Configuration Management
- Running Services
