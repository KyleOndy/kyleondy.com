# KyleOndy.com

[KyleOndy.com](https://www.kyleondy.com) is my personal website.
If you have any suggestion, I welcome any pull requests.

## Motivation

The site generation's primary goal is to produce HTML that I can in turn hos somewhere.
If that hasn't been accomplished, this is a bad repository.

A close secondary goal is to learn.
This repository could be simplified by using external libraries or frameworks.
I've chosen to reimplement 100% of the functionally within this repository.
Is this a horrible idea?
Probably.
But I only care about this working for me.
This is not intended to be reused by other people.
I have no intention of that.

## Building

This site is generated with a simple cjoure application.
Why?
I started with a Makefile based version, but I was stuck.
Either I cam not good enough at make, or its the wrong tool for handling data transformations.
Trying to get and pass along metadata within markdown files was too hard.

## Requirements

- Keep existing structure with minimal changes
  - Support tags
  - support git history
  - Zero javascript
  - zero external resources
  - minimal CSS (inline it?)


## Simple Site Revamp

- Take markdown / text and convert into metadata + html fragments
  - homegrown | pandoc
  - metadata is values stored in header of file
  - can inject additional metadata
    - git info
- inject html + metadata into templates
  - homegrown | some other templating project
- ability to create synthetic pages
  - pages not backed by a file on disk
    - archives
    - tags list
    - rss feeds
- external assets are copied in separately (make?)
  - images
  - css
  - js (if needed)

### Inspiration

### Thoughts

- whatever content is just inserted into site.html

## External Resources

- [this website is a makefile](https://www.johnhawthorn.com/2018/01/this-website-is-a-makefile/)
  - [example of above](https://github.com/jhawthorn/site-example)
