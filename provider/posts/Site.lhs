---
date: 2017-09-25 12:00:00
updated: 2017-09-30 16:00:00
tags: hakyll, haskell, generating this site, literate-programs
title: Generating this website // Part 1
subtitle: Getting acquainted
---

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import           GHC.IO.Encoding
import           Hakyll
import           Common
import           Content
import           Feed
import           Control.Applicative (liftA2)
\end{code}

Thigns that do not need tags

\begin{code}
templates, static, pages :: Rules ()
simpleRules :: Rules ()
simpleRules = do
  templates
  static
  pages

templates = match "templates/**" $ compile templateCompiler

static = match staticPattern $ do
  route $ gsubRoute "static/" (const "")
  compile copyFileCompiler

pages = match pagesPattern $ do
  route $  subFolderRoute `composeRoutes` gsubRoute "pages/" (const "")
  compile $ customPandocCompiler
    >>= loadAndApplyTemplate "templates/page.html" defaultContext
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls
\end{code}

Thigns that needs tags applied

\begin{code}
generateTags :: Rules Tags
generateTags = buildTags (postsPattern .||. notesPattern) (fromCapture "tags/*.html")

taggedRules :: Tags -> Rules ()
taggedRules = posts & notes & tagIndex & postIndex & noteIndex & index & feed-- & tagIndex & tagCloud
  where (&) = liftA2 (>>)
\end{code}

Here is simple config

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

Main entry point

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
