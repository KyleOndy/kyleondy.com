---
title:  Automatic installation of vim-plug
excerpt: Bootstrapping your neovim just a little bit easier
published: 2016-10-04
tags: vim, neovim
---

Add the following to the very top of your neovim config

```viml
if empty(glob('~/.config/nvim/autoload/plug.vim'))
v silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif
```
