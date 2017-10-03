---
date: 2017-10-03
updated: 2017-10-03
tags: hakyll, haskell, generating this site
title: Generating this website // Part 2
subtitle: Making posts
---

<aside class= "sidenote">
This is part two of the "generating this site" series of posts.
You can read the rest of the series [here][gensite].
</aside>


todo: write about posting posts

Three kinds of generated content:
- Index Page
- Posts
- Notes
- Pages
========================================
========================================
========================================

Preliminaries
-------------
We start with our usual `OverloadedStrings` definition and `Hakyll` import.
Because we're developing a module now, rather than the main entry point of our
program, we also need a module header.  This was missing from the last post but
will be present in all the remaining modules in this series.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Content where
import Hakyll
import Common (postsPattern, notesPattern)
\end{code}

Note that since the module is called `Posts`, the file itself must also be
called `Posts.hs`, or rather `Posts.lhs` as it is a literate Haskell document.
This has a bit of a knock-on effect, as Hakyll's default behaviour is to expect
that all files will follow a `date-title` convention for their filenames, so
already we will have to do some extra work to get it to draw that information
from the Pandoc header instead of the filename.

The following imports give us some extra helper functions which we'll use
throughout our program.  These functions (particularly the operators) are so
common I almost wish they were part of the standard prelude, but they aren't so
we'll have to live with a longish import list.  I won't go into too much detail
here but as I've used explicit imports you can see exactly which functions I'm
importing from where.

\begin{code}
--import Data.Monoid           ((<>))
import Data.Maybe            (fromMaybe)
import Data.List             (intercalate, isSuffixOf, sortBy)
import           Data.Ord            (comparing)
import Data.Char             (toLower, isAlphaNum)
import Control.Applicative   ((<$>), (<*>))
import           Data.Monoid         ((<>))
import Control.Monad         (msum, forM)
\end{code}

I'm going to be making use of a few system/date related functions to handle the
date specified in the header and rename the file appropriately.

\begin{code}
import System.FilePath    (replaceBaseName, takeDirectory,
                           takeBaseName, (</>))
import Data.Time.Clock    (UTCTime (..))
import Data.Time.Format   (formatTime, parseTimeM, defaultTimeLocale)
\end{code}





After all that, we can actually get on with writing some code!  If you're new to
Haskell, don't worry too much about all these imports -- in general you just add
them as you come across functionality you need to use which is defined in
another module, so you don't really need to think too much about them ahead of
time.  Because Haskell encourages breaking things down into small, reusable
components, import lists can get quite long.  This is a good thing!



Generating posts
----------------
Here begins a pattern that you will see a lot of.  In Hakyll, the way you
generate anything is by defining a `Compiler`.  Usually, that `Compiler` is
paired with a `Context` which provides all the variables you may want to make
use of in your template.  Finally, we tie that `Compiler` to a specific set of
inputs using `Rules`, which we covered in the previous post.  Often, people
write all their `Rules` inline in one big `main` function, but to make breaking
the configuration over a number of blog posts easier, I've elected to define
each set of `Rules` as an independent function which I call from `main` in the
first post.

First, then, the `Context`, which simply extracts data from the metadata header
at the top of the file.

\begin{code}
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    ,  defaultContext
    ]
\end{code}

add niceUrl field

\begin{code}
{-
defaultCtx :: Context String
defaultCtx = mconcat
  [ niceUrlField  "url" ]
-}
\end{code}

As well as the `defaultContext`, which gives us some common fields such as
`title`, we make use of the `date` and `tags`fields.  They ought to be fairly self-explanatory -- the date is displayed at the top of this
page and the tags are listed at the bottom.

The `Compiler` follows standard conventions: run the Pandoc compiler (in this
case our `customPandocCompiler` defined above), apply templates, and fix up the URLs.

\begin{code}
postCompiler :: Tags -> Compiler (Item String)
postCompiler tags = customPandocCompiler
  >>= saveSnapshot "content"
  >>= return . fmap demoteHeaders
  >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
  >>= loadAndApplyTemplate "templates/default.html" defaultContext
  -- >>= relativizeUrls
  >>= cleanIndexUrls
\end{code}

Hang on, what's that `saveSnapshot` in the middle there?  I never mentioned
that!  Well, that allows us to save a snapshot of our page *as it currently
stands* at that point in the compilation.  This is handy, because we'll want to
include the content of the post in RSS feeds and the like, but at that point we
only want the content, not the header, footer, and navigation.  It is useful,
then, to be able to save out a copy at the point where that is all we have and
re-use it later, rather than having to recompile the whole thing again when
generating feeds.

Finally, our `Rules` tell Hakyll where to get posts from, how to compile them,
and where to put them.

\begin{code}
posts :: Tags -> Rules ()
posts tags = match ("posts/*" .||. "pending-posts/*") $ do
  route $   metadataRoute dateAndTitle `composeRoutes`
            customRoute simplifyURL
  compile $ postCompiler tags
\end{code}

This is mostly pretty simple.  You can see we generate posts from both the
`posts` and `pending-posts` directories (the latter are generated, but not
included in the index, so I can preview them because I know the URL but they're
not "published" as such).  We use the `date` and `title` metadata fields to
generate a filename and then from that create a simplified URL.  Finally we
compile it with the `postCompiler` we defined above.

I took the idea (and the code) for the simplified URL route from [Yann
Esposito's Hakyll setup][yehs].  Instead of `post-name.html`, it outputs a
file to `post-name/index.html`, allowing us to drop the `.html` part when
visiting the page in the browser.  It is defined as follows.

\begin{code}
simplifyURL :: Identifier -> FilePath
simplifyURL ident =
  takeDirectory p </> takeBaseName p </> "index.html"
  where p = toFilePath ident
\end{code}

There's just one snag... that
`dateAndTitle` function passed to `metadataRoute` doesn't actually exist!
We're going to have to write it.

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
```haskell
metadataRoute :: (Metadata -> Routes) -> Routes
```
The function we passed to `metadataRoute` above was `dateAndTitle`, which we'll
define here.

\begin{code}
dateAndTitle :: Metadata -> Routes
dateAndTitle meta = fromMaybe idRoute $
  mkName <$> getField "title" <*> getField "date"
  where   mkName t d   =   setBaseName $ title t
          getField     =   (`lookupString` meta)
          date         =   formatTime defaultTimeLocale
                           "%Y/%m/%d" . readTime
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

OK, after that little detour, let's get back to it!  The `dateAndTitle` function
above made use of two helper functions which haven't actually been defined.  The
first is `readTime`, which we use to normalise the date format.  It takes a date
string and converts it to a `UTCTime` which we can manipulate.

\begin{code}
readTime :: String -> UTCTime
readTime t = fromMaybe empty' . msum $ attempts where
  attempts   = [parseTimeM True defaultTimeLocale fmt t | fmt <- formats]
  empty'     = error $ "Could not parse date field: " ++ t
  formats    = [ "%a, %d %b %Y %H:%M:%S %Z"
               , "%Y-%m-%dT%H:%M:%S%Z"
               , "%Y-%m-%d %H:%M:%S%Z"
               , "%Y-%m-%d %H:%M"
               , "%Y-%m-%d"
               , "%B %e, %Y %l:%M %p"
               , "%B %e, %Y"
               , "%b %d, %Y"
               ]
\end{code}

The basic idea for the implementation is taken from Hakyll itself, from its
`getItemUTC` which is defined in [`Hakyll.Web.Template.Context`][hwtc].
Unfortunately, the type signature for that function is quite a lot more
complicated than we need, so I've extracted the parts we need into a simple
`String -> UTCTime` function here.  If the date doesn't match any of the
supported formats `readTime` will simply crash with an error -- not the best
error handling but since we're always going to be running this interactively it
doesn't really matter.

`setBaseName` turns a string into a `FilePath`, which it can then manipulate
using Haskell's native `replaceBaseName` functionality.

\begin{code}
setBaseName :: String -> Routes
setBaseName basename = customRoute $
  (`replaceBaseName` basename) . toFilePath
\end{code}

Pages
-----

Another form of content on this blog is that of "pages", which are basically
posts except that they don't have a date or tags associated with them and they
are not indexed or included in feeds.  As a result they are super-simple -- we
don't need to save a snapshot, or to parse the date or change the filename.
Instead I can just compile it with a template designed for the purpose and set
the extension.  We'll use the same `customCompiler` as posts for consistency,
but we'll just pass the `defaultContext` as we don't need any of the extra
metadata posts use.

\begin{code}
pageCompiler :: Compiler (Item String)
pageCompiler = customPandocCompiler
  >>= loadAndApplyTemplate "templates/page.html"      ctx
  >>= loadAndApplyTemplate "templates/default.html"   ctx
  >>= relativizeUrls
  where ctx = defaultContext
\end{code}

The rules for pages are equally simple -- just grab anything from the `pages`
folder, compile it using the `pageCompiler` and set its extension to `html`.
This is expressed below.

\begin{code}
pages :: Rules ()
pages = match "pages/*" $ do
  route $ customRoute simplifyURL
  compile pageCompiler
\end{code}


todo: wtite about these

\begin{code}
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
\end{code}

Index Page
----------

\begin{code}
index :: Tags -> Rules ()
index tags = match "pages/index.html" $ do
               route $ gsubRoute "pages/" (const "")
               compile $ do
                 posts <- fmap (take 3) . recentUpdatedFirst =<< loadAll postsPattern
                 notes <- fmap (take 3) . recentUpdatedFirst =<< loadAll notesPattern
                 let indexContext =
                        listField "posts" (postCtx tags) (return posts) <>
                        listField "notes" (postCtx tags) (return notes) <>
                        field "tags" (\_ -> renderTagList tags) <>
                        defaultContext

                 getResourceBody
                     >>= applyAsTemplate indexContext
                     >>= loadAndApplyTemplate "templates/default.html" indexContext
                     -- >>= relativizeUrls
                     >>= cleanIndexUrls

{-
indexContext :: Tags -> Context String
indexContext tags = mconcat [
                  listField "posts" (postCtx tags) (return posts)
                , listField "notes" (postCtx tags) (return notes)
                , field "tags" (\_ -> renderTagList tags)
                , defaultContext
                      ]
                      -}
\end{code}

\begin{code}
indexCompiler :: Tags -> Pattern -> Context String -> Compiler (Item String)
indexCompiler tags pattern baseCtx = do
  posts <- fmap (take 3) . recentUpdatedFirst =<< loadAll "posts/*"
  notes <- fmap (take 3) . recentUpdatedFirst =<< loadAll "nots/*"
  let indexContext =
         listField "posts" (postCtx tags) (return posts) <>
         listField "notes" (postCtx tags) (return notes) <>
         field "tags" (\_ -> renderTagList tags) <>
         defaultContext
  getResourceBody
      >>= applyAsTemplate indexContext
      >>= loadAndApplyTemplate "templates/default.html" indexContext
      >>= relativizeUrls
      >>= cleanIndexUrls
\end{code}


\begin{code}
recentUpdatedFirst :: [Item a] -> Compiler [Item a]
recentUpdatedFirst items = do
    itemsWithTime <- forM items $ \item -> do
        updateTime <- getMetadataField (itemIdentifier item) "updated"
        return (updateTime,item)
    return $ reverse (map snd (sortBy (comparing fst) itemsWithTime))
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