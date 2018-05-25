---
title:  Automatic installation of vim-plug
subtitle: Bootstrapping your neovim just a little bit easier
created: 2016-10-04 12:00:00
updated: 2016-10-04 12:00:00
tags: vim, neovim
---

<aside>
This is an outdated article and is here for prosperities sake.
The following information is more than likely woefully out of date.
</aside>

Add the following to the very top of your neovim config

``` bash
if empty(glob('~/.config/nvim/autoload/plug.vim'))
v silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif
```
