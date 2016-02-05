---
title:  On Being a Digital Nomad
published: 2015-12-14
excerpt: Self-hosting service for freedom
tags: self-hosted
---

I have begun a migration from many of the services I currently use to FOSS alternatives I am able to host myself.
The ability to have more control over my data, and to be able to easily move from service to service as needs change is a goal I've wanted for a while.

Services that I used everyday have been discontinued, changed, or demonstrated that they are untrustworthy.

* Google Reader
* Dropbox's multiple security problems, including access without passwords.
* [LastPass](https://lastpass.com/) being purchased by LogMeIn, who have a history of sudden and substantial price increases.

At this point I am taking a proactive rather than a reactive approach and moving away from as many services that I can't control as possible.
A second outcome of this moves means I am able to contribute back to the FOSS community.

## Sovereign ##

This would be difficult to do on your own, luckily [sovereign](https://github.com/sovereign/sovereign) has done the heavy lifting for you.

* Email (Dovecot, Solr)
* RSS Reader (Selfoss)
* Cloud Storage (ownCloud)
* VPN (OpenVPN)
* IRC Bounder (znc)
* Git hosting (cgit, gitolite)
* Read-it-later (wallabag)
* Backups (Tarsnap)

Sovereign is a set of an [ansible](http://www.ansible.com/) playbooks that provision a machine to host selected services.
