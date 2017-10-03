---
date: 2017-10-03
updated: 2017-10-03
tags: hakyll, haskell, generating this site
title: Generating this website // Part 2
subtitle: Making posts
---

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Common
 (postsPattern, pagesPattern, notesPattern, staticPattern,customPandocCompiler,
 subFolderRoute, taggedCtx,lexicographyOrdered, recentUpdatedFirst) where

import qualified Data.Set            as S
import           Hakyll
import           System.FilePath     (takeBaseName, takeDirectory, (</>))
import           Text.Pandoc.Options (Extension (..), ObfuscationMethod (..),
                                      ReaderOptions (..), WriterOptions (..),
                                      def)
import           Data.List           (sortBy)
import           Data.Ord            (comparing)
import Control.Monad (forM)

postsPattern :: Pattern
postsPattern = "posts/*"

pagesPattern :: Pattern
pagesPattern = "pages/*"

notesPattern :: Pattern
notesPattern = "notes/*"

staticPattern :: Pattern
staticPattern = "static/**"

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith readerOptions writerOptions

readerOptions :: ReaderOptions
readerOptions = def { readerSmart = True }

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