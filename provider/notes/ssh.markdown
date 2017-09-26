---
title: SSH
published: 2016-12-14
updated: 2016-12-14
subtitle: Quick notes on common ssh usage
tags: notes, ssh
---

Some notes below on ssh.

To break out of a stuck ssh session: <kbd>enter</kbd><kbd>~</kbd><kbd>.</kbd>
More options: <kbd>enter</kbd><kbd>~</kbd><kbd>?</kbd>

SSH Tunnel
ex: port 143 is blocked, but can ssh to mailserver
`ssh -L 1430:localhost:143 mailserver`{.bash}

