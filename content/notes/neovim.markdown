---
title: nvim
updated: 2018-10-06T14:58:19Z
subtitle: Notes for neovim (which should mostly work in vim)
tags: notes, vim, neovim
---

# Commonly Used Commands

I use `neovim` daily, but most of these commands should work in vanilla `vim` too.

## Diff Operations

* Get **chunck** from other buffer (diff obtain): `do`
* Move **chunk** to other buffer (diff put): `dp`
* Get **range** from other buffer: `diffg[et]`
* Move **range** to other buffer: `diffou[t]`

## Spelling ##

* turn spell check on: `:set spell`
* Move to next/last word: <kbd>]</kbd><kbd>s</kbd> / <kbd>[</kbd><kbd>s</kbd>
* Add word to dictionary: <kbd>z</kbd><kbd>g</kbd>
* Show suggestions: <kbd>z</kbd><kbd>=</kbd>

## Text Insertion ##

When pasting code use `:set paste` to preserve indenting of pasted code.

## Text Wrapping ##

Wrap code to `textwidth`: <kbd>g</kbd><kbd>q</kbd>

## Plugins ##

* Update / Install plugins: `:PlugUpdate`

