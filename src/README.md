# src

Pass in the [`content`](../content) directory, and you get a static site ready for deployment in `_site`.

## General Process

- Read all markdown (`*.md`,`*.markdown`) content in [`content`](../content), build a data-structure with content of posts, and metadata about them.
- Take this DS, pass it along to functions that transform it
- Repeat
- Generate site files from DS.

## More specifics

- Useing [end](https://github.com/edn-format/edn) as the interchange format.

- (markdown-to-hiccup)[https://github.com/mpcarolin/markdown-to-hiccup) to get hiccup representation of markdown files
