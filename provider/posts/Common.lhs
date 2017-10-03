---
date: 2017-10-03
updated: 2017-10-03
tags: hakyll, haskell, generating this site
title: Generating this website // Part ?
subtitle: Resuing the good stuff
---

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Common
 (postsPattern, pagesPattern, notesPattern, staticPattern,customPandocCompiler,
 subFolderRoute, taggedCtx,lexicographyOrdered, recentUpdatedFirst) where
\end{code}

The `Set` module exports function names that clash with those from the standard
prelude for working with lists, so I'll import it qualified here.  In fact, I
only make use of one function from it (`union`), so I could have just imported
this function and had done with it, but it's common form to import data
structures like this qualified, so I'm in the habit of it.

\begin{code}
import qualified Data.Set            as S
import           Hakyll
import           System.FilePath     (takeBaseName, takeDirectory, (</>))
\end{code}

Finally some more specific imports.  I'll be overriding some of Pandoc's
default options so I'll need to bring those into scope.

\begin{code}
import           Text.Pandoc.Options (Extension (..), ObfuscationMethod (..),
                                      ReaderOptions (..), WriterOptions (..),
                                      def)
import           Data.List           (sortBy)
import           Data.Ord            (comparing)
import           Control.Monad       (forM)

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
\enc{code}

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

Contexts
---------

\begin{code}
taggedCtx :: Tags -> Context String
taggedCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]
\end{code}