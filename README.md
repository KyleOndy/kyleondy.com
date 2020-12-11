# KyleOndy.com

[![Build Status](https://travis-ci.org/KyleOndy/kyleondy.com.svg?branch=master)](https://travis-ci.org/KyleOndy/kyleondy.com)

[KyleOndy.com](https://www.kyleondy.com) is my personal website.
If you have any suggestion, I welcome any pull requests.

## Building

This site is generate with [hakyll](https://jaspervdj.be/hakyll/).

For more information on the generation of the site itself, the source is a [literate haskell](https://wiki.haskell.org/Literate_programming) file.

- [site.lhs](https://github.com/KyleOndy/kyleondy.com/blob/master/provider/posts/site/site.lhs)
- [Rendered Post](https://kyleondy.com/posts/generating-this-web/)

## Deployment

The site it deployed as a static site to Amazon S3 and cached with Amazon CloudFront.
Deployment is handled by the [s3_website](https://github.com/laurilehmijoki/s3_website) gem, see my [s3_website.yml config](https://github.com/KyleOndy/kyleondy.com/blob/master/s3_website.yml).

## Structure

Everything in [`./provider/static`](./provider/static) is copied unaltered to the root of the site, preserving any file hierarchy.

## Todo

- I really like [brandur's](https://www.brandur.org/articles) article layout.
  I do not want to rip him off, but like the printed media feel
- Publish RSS feed
- Add a page of project ideas
- Articles / Fragments / Notes
