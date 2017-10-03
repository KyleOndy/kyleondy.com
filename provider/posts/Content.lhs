---
date: 2017-10-03
updated: 2017-10-03
tags: hakyll, haskell, generating this site
title: Generating this website // Part 2
subtitle: Making posts
---

A few kinds of contnet

- Index Page
- Posts Page
- notes Page
- posts
- notes

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Content where
import Hakyll
import Common
import           Data.Monoid         ((<>))
import Data.Maybe          	(fromMaybe)
import Data.Char           	(toLower, isAlphaNum)
import Data.List           	(intercalate, isInfixOf)
import System.FilePath  	(replaceBaseName)

posts :: Tags -> Rules()
posts tags = taggedContent postsPattern tags

notes :: Tags -> Rules()
notes tags = taggedContent notesPattern tags

taggedContent :: Pattern -> Tags -> Rules()
taggedContent pattern tags = match pattern $ do
  route $ subFolderRoute
  compile $ do
    customPandocCompiler
      >>= saveSnapshot "content"
      >>= return . fmap demoteHeaders
      >>= relativizeUrls
      >>= loadAndApplyTemplate "templates/post.html" (taggedCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
\end{code}

\begin{code}

postIndex :: Tags -> Rules()
postIndex tags = create ["posts.html"] $ do
  route subFolderRoute
  compile $ do
    posts <- recentFirst =<< loadAll postsPattern
    let ctx = constField "title" "Posts - Most Recent" <>
                listField "posts" (taggedCtx tags) (return posts) <>
                defaultContext
    makeItem ""
      >>= loadAndApplyTemplate "templates/posts.html" ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
			
noteIndex :: Tags -> Rules()
noteIndex tags = create ["notes.html"] $ do
  route subFolderRoute
  compile $ do
    notes <- lexicographyOrdered =<< loadAll notesPattern
    let ctx = constField "title" "Notes - Alphabetical" <>
              listField "posts" (taggedCtx tags) (return notes) <>
              defaultContext
    makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

\end{code}

\begin{code}
index :: Tags -> Rules()
index tags = match "index.html" $ do
    -- todo: not sure this is the best way to handle index.html
    route $ gsubRoute "index/" (const "")
    compile $ do
        recentPosts <- fmap (take 3) . recentUpdatedFirst =<< loadAll postsPattern
        recentNotes <- fmap (take 3) . recentUpdatedFirst =<< loadAll notesPattern
        let indexContext =
                listField "posts" (taggedCtx tags) (return recentPosts) <>
                listField "notes" (taggedCtx tags) (return recentNotes) <>
                field "tags" (\_ -> renderTagList tags) <>
                defaultContext
        getResourceBody
            >>= applyAsTemplate indexContext
            >>= loadAndApplyTemplate "templates/default.html" indexContext
            >>= relativizeUrls
\end{code}


Need to have these or the tags will not show up.

\begin{code}
tagIndex :: Tags -> Rules()
tagIndex tags = tagsRules tags $ \tag p-> do
      let title = "Tagged: " ++ tag
      route subFolderRoute
      compile $ do
          posts <- recentFirst =<< loadAll p
          let ctx = constField "title" title <>
                      listField "posts" (taggedCtx tags) (return posts) <>
                      defaultContext
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html" ctx
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls
\end{code}


Extracting the date and title from metadata
-------------------------------------------

As mentioned, Hakyll by default expects the date and title to be included in the
filename and as such it can just change the extension and have done with it.
Because we might be naming files after the modules they define (in the case of
Literate Haskell files), a post such as this one would end up with a URL looking
like <http://dpwright.com/Posts.html>, which would be very odd.  Even
without this, it's quite easy to write a post which accidentally has a subtly
different title to its filename, which would be confusing.
Because of this, we'd much rather pull the `date` and `title` fields out of the
post's metadata, which ensures consistency and reduces repetition.
Unfortunately, Hakyll provides no clear way to do this natively, so we have to
make use of its generic functionality for routing based on metadata.  We do this
using the `metadataRoute` function, above, which takes as its parameter a
function taking a posts `Metadata` and returning the `Routes` associated with
that metadata, and returns `Routes` which can be used by the `Rules` to which is
it passed.  Its type signature looks like this:

~~~haskell
metadataRoute :: (Metadata -> Routes) -> Routes
~~~

The function we passed to `metadataRoute` above was `dateAndTitle`, which we'll
define here.

\begin{code}
dateAndTitle :: Metadata -> Routes
dateAndTitle meta = fromMaybe idRoute $
  mkName <$> getField "title" <*> getField "date"
  where   mkName t d   =   setBaseName $ title t
          getField     =   (`lookupString` meta)
          title        =   map toLower . intercalate "-"
                       .   map (filter isAlphaNum) . words
\end{code}

There's a lot going on in this definition so we'll go through it carefully.

- We begin with a call to `fromMaybe` passing `idRoute` as the fallback.  This
  means that what follows might fail, and should it fail we'll just use the
  filename as-is (falling back on Hakyll's default behaviour).
- `mkName` is called in applicative style, passing two calls to
  `getField` (defined locally).  We know that `Maybe` forms an `Applicative`,
  and that we are expecting a `Maybe` here as the second parameter to
  `fromMaybe`.  So we can infer what will happen here: it will try to get the
  `title` and `date` fields, and if either of them fail it will return
  `Nothing`, otherwise it will pass them both to `mkName`.
    - If you are familiar with applicative style this will have been immediately
      obvious.  If not, it is worth reading through the previous bullet-point
      and associated code a few times until you get a feeling for what's
      happening.  We've reduced what would have been a lot of sanity checking
      and nested `if` statements into a single line of code which, when you are
      used to this style, reads extremely clearly.  It's a very powerful
      technique.
- Moving onto the local definitions: `getField` is simply a shortcut for
  calling the `lookupString` function in order to get the respective fields out
  of the passed `Metadata`.
- `mkName` takes the title and the date as parameters, calls the `date`
  and `title` functions in order to turn them into strings, and then sticks them
  together with a `/`.  Finally it calls `setBaseName` (defined below), which
  works similarly to Hakyll's native `setExtension` except that, obviously, it
  sets the basename.
- `date` normalises the `date` field to the simple `YYYY/mm/dd` format we want
  to use for our filenames.  This means you can write the date in any of
  Hakyll's supported date formats and the filename will turn out OK.
- `title` splits the title up into words, filters out any strange symbols using
  `isAlphaNum` (which, thankfully, is Unicode-friendly so that Japanese titles
  aren't considered "strange symbols"), and then joins those words back up with
  `-` dashes so that we don't have to worry about `%20`s appearing all over our
  URLs.  Finally, it makes the whole thing lower-case.

Function definitions like the one above are part of the reason I love Haskell
and others might hate it.  There's so much functionality packed into so little
code there, you do have to slow down a little bit and read it carefully to
follow it, at least until you are used to code written in the style used (in
this case, some applicative style and a little bit of point-free notation thrown
in for good measure).  Add to this the expressivity of Haskell which allows for
a number of different styles, so that even once you've got used to the style
used here you may open another codebase and find that the style employed there
is completely different!  There is a not-unreasonable argument that this is
problematic; that encouraging a very particular style at the language level (as
Python does, for example) makes it a lot easier to read unfamiliar code.

I am sympathetic to this argument up to a point.  It does make sense, especially
if you are dealing with large numbers of programmers relatively inexperienced in
the language (thus not exposed to the various styles of programming available),
who regularly have to jump into different codebases (thus run into these
different styles frequently).  However, I do think the benefits outweight the
disadvantages.  Firstly, inexperienced programmers are likely to be limited to
relatively isolated areas of the code, so they will have time to get used to
whatever style is employed there before moving on and learning some new style
along with the next codebase.  Secondly, *once you have learnt* the style, it is
actually dramatically *faster* to read succinct code like this than trudging
through reams of `if` statements and manual `for`-style loops.  It's also harder
to make mistakes, as the code fits more closely with the thing it's trying to
do.

So there is a learning curve, and learning curves cost time and money when
training programmers.  But when the initial hump is traversed[^1], the increase
in productivity is well worth the effort.



`setBaseName` turns a string into a `FilePath`, which it can then manipulate
using Haskell's native `replaceBaseName` functionality.

\begin{code}
setBaseName :: String -> Routes
setBaseName basename = customRoute $
  (`replaceBaseName` basename) . toFilePath
\end{code}

Conclusion
----------
That's about it for compiling posts!  This is *almost* all you need to get a
Hakyll site up and running -- the only problem is since we still don't have post
indexing you have to know the URL of the post you want to read before you read
it!  This is about as complicated as it gets with Hakyll though -- if you've
followed this post, the rest should be easy!  We'll cover indexing next, after
which we'll go about adding special features one at a time.

[^1]: Anybody who's read code using the `Lens` library will get the joke.

[generating-this-website]: http://www.dpwright.com/tags/generating%20this%20website
[hwtc]: https://hackage.haskell.org/package/hakyll-4.2.2.0/docs/src/Hakyll-Web-Template-Context.html
[yehs]: http://yannesposito.com/Scratch/en/blog/Hakyll-setup/