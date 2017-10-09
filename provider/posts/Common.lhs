---
date: 2017-10-02 11:39:10
updated: 2017-10-02 21:39:10
tags: hakyll, haskell, generating-this-site, literate-programs
title: Generating this website // Part 3
subtitle: Building the base
---

<aside class= "sidenote">
This is part three of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</aside>


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Common
 (
 customPandocCompiler,
 lexicographyOrdered,
 notesPattern,
 pagesPattern,
 postsPattern,
 readTime,
 removeIndexHtml,
 recentUpdatedFirst,
 staticPattern,
 subFolderRoute,
 taggedCtx
 ) where
\end{code}

The `Set` module exports function names that clash with those from the standard
prelude for working with lists, so I'll import it qualified here.  In fact, I
only make use of one function from it (`union`), so I could have just imported
this function and had done with it, but it's common form to import data
structures like this qualified, so I'm in the habit of it.

\begin{code}
import qualified Data.Set        as S
import           Hakyll
import           System.FilePath (takeBaseName, splitFileName, takeDirectory, (</>))
\end{code}

Finally some more specific imports.  I'll be overriding some of Pandoc's
default options so I'll need to bring those into scope.

\begin{code}
import  Text.Pandoc.Options (Extension (..), ObfuscationMethod (..),
                            ReaderOptions (..), WriterOptions (..),
                            def)
import  Data.List           (sortBy, isInfixOf)
import  Data.Ord            (comparing)
import  Control.Monad       (forM)
import  Data.Time.Clock     (UTCTime (..))
import  Control.Monad       (msum)
import  Data.Maybe          (fromMaybe)
import  Data.Time.Format    (parseTimeM, defaultTimeLocale, formatTime)

postsPattern :: Pattern
postsPattern = "posts/*"

pagesPattern :: Pattern
pagesPattern = "pages/*"

notesPattern :: Pattern
notesPattern = "notes/*"

staticPattern :: Pattern
staticPattern = "static/**"
\end{code}

To begin with, I'm going to define the custom version of the Pandoc compiler
we'll use to generate the posts.  Hakyll comes with some reasonable defaults,
but I'd like to tweak it a little to allow support for features specific to my
needs here -- in particular, I want support for:

- Literate Haskell (or you wouldn't be reading this!)
- MathJax
- Syntax Highlighting
- Smart Parsing (conversion of `--` to --, and so forth)

The compiler itself is just a standard compiler with different reader and writer
options, and some pandoc-level transformations:

\begin{code}
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith readerOptions writerOptions
\end{code}

Those options are defined in terms of Pandoc's defaults, provided by the
`Default` typeclass, which allows you to specify a default definition `def` for
any type.  First we tell the reader to add `readerSmart` to its options

\begin{code}
readerOptions :: ReaderOptions
readerOptions = def { readerSmart = True }
\end{code}

The writer options are manipulated in a similar way, adding MathJax support,
syntax highlighting, and literate Haskell.

\begin{code}
writerOptions :: WriterOptions
writerOptions = def
  { writerHighlight        = True
  , writerExtensions       = extensions
  , writerHtml5            = True
  , writerEmailObfuscation = NoObfuscation
  }
  where extensions= writerExtensions def `S.union` S.fromList
                    [ Ext_literate_haskell
                    , Ext_tex_math_dollars
                    , Ext_tex_math_double_backslash
                    , Ext_latex_macros
                    ]
\end{code}

Defining `extensions` as a union of the default extensions with a single-member
set may seem like overkill, and for only one item it is, but doing it this way
means that if I ever want to add an extension I can just add it to the list.



/about.md -> /about/index.html

\begin{code}
subFolderRoute :: Routes
subFolderRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                           where p = toFilePath ident
\end{code}


\begin{code}
lexicographyOrdered :: [Item a] -> Compiler [Item a]
lexicographyOrdered items = return $
              sortBy (comparing (takeBaseName . toFilePath . itemIdentifier)) items

recentUpdatedFirst :: [Item a] -> Compiler [Item a]
recentUpdatedFirst items = do
    itemsWithTime <- forM items $ \item -> do
        updateTime <- getMetadataField (itemIdentifier item) "updated"
        return (updateTime,item)
    return $ reverse (map snd (sortBy (comparing fst) itemsWithTime))
\end{code}


\begin{code}
removeIndexHtml :: String -> Compiler String
removeIndexHtml body = return $ withUrls removeIndexStr body
  where
    removeIndexStr url = case splitFileName url of
      (dir, "index.html") | isLocal dir   -> init dir
      _                                   -> url
    isLocal uri = not $ "://" `isInfixOf` uri
\end{code}

Contexts
---------

\begin{code}
dateFormat :: String
dateFormat = "%B %e, %Y"

taggedCtx :: Tags -> Context String
taggedCtx tags = mconcat
    [ dateField "date" dateFormat
    , tagsField "tags" tags
    , updatedField "updated" dateFormat
    , defaultContext
    ]
\end{code}

Date functions
--------------

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

\begin{code}
updatedField :: String -> String -> Context a
updatedField key format = field key $ \i -> do
  t <- getMetadataField' (itemIdentifier i) "updated"
  return $ (formatTime defaultTimeLocale format $ readTime t)
\end{code}

[generating-this-website]: /tags/generating-this-site/