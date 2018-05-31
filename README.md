# KyleOndy.com

[![Build Status](https://travis-ci.org/KyleOndy/kyleondy.com.svg?branch=master)](https://travis-ci.org/KyleOndy/kyleondy.com)

[KyleOndy.com](https://www.kyleondy.com) is my personal website.
The site it deployed as a static site to S3, cached with CloudFront, generated with [hakyll](https://jaspervdj.be/hakyll/).
Deployment is handled by [s3_website](https://github.com/laurilehmijoki/s3_website), see ,my [s3_website.yml](https://github.com/KyleOndy/kyleondy.com/blob/master/s3_website.yml).

For more information on the generation of the site itself, the source is a [literate haskell](https://wiki.haskell.org/Literate_programming) file.

- [site.lhs](https://github.com/KyleOndy/kyleondy.com/blob/master/provider/posts/site/site.lhs)
- [Rendered Post](https://kyleondy.com/posts/generating-this-web/)
