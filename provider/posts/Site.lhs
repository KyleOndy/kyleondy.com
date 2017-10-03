---
date: 2017-09-25
updated: 2017-10-03
tags: hakyll, haskell, generating this site
title: Generating this website // Part 1
subtitle: Getting acquainted
---

<aside class="sidenote">
This is part one of the "generating this site" series of posts.
You can read the rest of the series [here][gensite].
</aside>

I have recently moved this site to some new hosting and am taking the oppertunity at provide some content.
When tweaking my config file for [hakyll] I had the thought that I could write the configuration in [literate hasekll], providing content and doccumenting my code.o
This post is litteraly the source code that compiles the application that builds this site.
So this post can compile an application which than can take this post as input and produce this post as output.
Need a totem?

<h1>Lets get started!</h1>

And so began the series of [Generating this website][gensite].
This series will assume you have a basic working knowledge of Haskell or another functional language.
Experice with Hakyll isn't needed either, but having used a static site generate before will make some concepts easier to grasp.
If anything isn't clear feel free to [contact] me or submit a [pull request] against this file.

<aside class="sidenote">
From here and below is still in the process of being cleaned up.
I will be breaking out large chunks of functionality into their own modules, and creating a post for each.
</aside>

Here are two lines which will be seen a lot as we go forward:

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
\end{code}

The `OverloadedStrings` LANGUAGE pragma enables many thing I wish were enabled by default within Haskell.
While you can define pragma in the projects `cabal` file, I prefer to explicty declare it for clairty.

Nest we import Hakyll, which gives us access to the DSL Haskell provides.

I'm also going to import `liftA2` from `Control.Applicative` here.  I use this
as a convenience later.

\begin{code}
import Control.Applicative (liftA2)
\end{code}

todo: why encoding?

\begin{code}
import GHC.IO.Encoding
\end{code}

Finally, I'll import the other posts in this series.
These supply much of the actual functionality.

Indexing and Feed are coming soon as new posts.

\begin{code}
import Content
--Import Indexing
--Import Feed
\end{code}

Some simple rules
-----------------
The main entry point to Hakyll takes a set of [`Rules`][hakyllrules] and
returns an `IO` action which generates the site.  `Rules` themselves form a
monad, so assuming we have some simple rules:


\begin{code}
templates, images, css, static :: Rules ()
\end{code}

We can put them together by simply listing them using `do` notation.

\begin{code}
simpleRules :: Rules ()
simpleRules = do
  templates
  images
  static
  pages
  css
\end{code}

The rules themselves govern the compilation and generation of files.  Perhaps
the simplest of these is `templates`, which compiles all files found in the
`templates` directory and any subdirectories, but doesn't actually need to
output those files anywhere -- instead it keeps the compiled versions around for
other pages using that template.

\begin{code}
templates = match "templates/**" $ compile templateCompiler
\end{code}

Hakyll provides a [`Pattern`][hakyllpattern] type which, conveniently,
implements `IsString` so our `OverloadedStrings` pragma takes care of the
conversion for us.  The `**` pattern searches in that directory and all
subdirectories.

Next up come the images.  These are also very simple -- simply take the full
path of the images, and copy them to the same place in the output directory.

\begin{code}
images = match "images/*" $ do
  route   idRoute
  compile copyFileCompiler
\end{code}

The `route` rule defines the output filename.  `idRoute`, as the name implies,
sets the output filename to match the input filename.  Any rule which generates
output requires a `route` -- any rule without a `route` will be run, but won't
generate any output (like the `templates` rule above).
For CSS files, Hakyll provides a compressor to speed download times.

\begin{code}
css = match "css/*" $ do
  route   idRoute
  compile compressCssCompiler
\end{code}

Of course, the `copyFileCompiler` would work just as well, but we might as well
compress the CSS while we're at it.
Occasionally, I just want to put some static files up that don't fit the
structure of the rest of the blog.  This is particularly useful when I want to
upload slides from a talk I've given, for example the [git talk][gits-guts] I
gave a couple of months ago.  The talk itself is maintained in a different
repository, so it's convenient if I can just include that as a submodule and
have its files copied automatically.  I do this by storing all such content in
the `static` directory, and then copying it when generating the site, stripping
the initial `static/` from the output path.

\begin{code}
static = match "static/**" $ do
  route $ gsubRoute "static/" (const "")
  compile copyFileCompiler
\end{code}

`gsubRoute` is actually quite powerful, allowing us to change our substitution
based on the matched input, but in this case we just want to substitute for the
empty string every time, so we use `const` to drop the argument.

Tags, and the `Rules` which require them
----------------------------------------

The remaining rules are complicated by the fact that they need access to the
tags for various reasons -- the tag index pages obviously need to list all posts
matching a certain tag, while the posts themselves and the Atom feed list the
tags for a particular post at the bottom of the post.
In order to do this, we first need to generate the tags for the site, and then
we need to pass these into those `Rules` that make use of them.  Generating the
tags is quite easy:

\begin{code}
generateTags :: Rules Tags
generateTags = buildTags "posts/*" $ fromCapture "tags/*.html"
\end{code}

Here I use `buildTags` to get the tags from all files in the `posts` directory.
The default method of tagging posts is just to include a `tags` field in the
post's metadata, but if I wanted to do it some other way I could use
`buildTagsWith` instead.

`fromCapture` acts sort of like a `Pattern` in reverse; it fills in the capture
(The `*` in `tags/*.html` in this case) with a given string.  We use that to
say, "for every tag read from the posts' metadata, create an index page at
'tags/TAGNAME.html'".

Having generated the tags, we need to pass them into any rules that need them.
We could use `do`-notation as we did for `simpleRules` and simply pass the
`tags` parameter to each entry, but here I'm going to use a little `Applicative`
trick which allows me to keep the function point-free, and I think makes it read
a little more declaratively.

\begin{code}
taggedRules :: Tags -> Rules ()
--taggedRules = posts & outdatedURLs & clonedURLs & index & tagIndex & tagCloud & feed
taggedRules = posts -- & index & tagIndex & tagCloud & feed
  --where (&) = liftA2 (>>)
\end{code}

This trick exploits the fact that `(->)`, the type of functions, implements
`Applicative` (in fact being applicative is rather their *raison d'être* when
you think about it), so if we lift the Monadic `(>>)` operator to act on
*applications of functions returning a Monad* instead of just Monads, we can
pass the parameter to the function in once and it will be distributed to each of
those functions.  In other words:

```haskell
posts tags >> tagIndex tags >> feed tags ≡ (posts & tagIndex & feed) tags
  where (&) = liftA2 (>>)
```

Because of Haskell's function currying and η-reduction, we can then drop the
`tags` parameter and the brackets entirely and we're left with the definition
for `taggedRules` above.

Configuration
-------------

\begin{code}
config :: Configuration
config = defaultConfiguration
    {
      providerDirectory = "provider"
    , destinationDirectory = "_site"
    , storeDirectory = "_cache"
    , tmpDirectory = "_cache/tmp"
    }
\end{code}

Putting it all together
-----------------------

Finally we define the entry point to the application.  This simply calls
Hakyll's own `hakyll` function, passing in the rules defined above.
First we call the simple, self-standing rules, then we generate the tags and
pass them to the tagged rules.

\begin{code}
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyllWith config $ do
    simpleRules
    generateTags >>= taggedRules
\end{code}

This concludes the introduction to Hakyll and the entry point for the generation
code for this website.  Stay tuned for the next entry, where we'll add the
configuration to actually create the posts themselves!

[gensite]:        /tags/generating this site/
[contact]:        /contact
