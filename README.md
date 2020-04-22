# KyleOndy.com

[KyleOndy.com](https://www.kyleondy.com) is my personal website.
If you have any suggestion, I welcome any pull requests.

## Building

This site is generated with a makefile.
Why?
Because its simple and fast, and I want to embrace the simple tools.
Avoid over complicating everything.


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


## External Resources

- [this website is a makefile](https://www.johnhawthorn.com/2018/01/this-website-is-a-makefile/)
  - [example of above](https://github.com/jhawthorn/site-example)
