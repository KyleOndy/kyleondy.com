---
date: 2017-09-25
updated: 2017-10-03
tags: hakyll, haskell, generating this site
title: Generating this website // Part 1
subtitle: Getting acquainted
---

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import           Common
import           Control.Applicative (liftA2)
import           GHC.IO.Encoding
import           Hakyll
import           Content
--import Pages
--import Posts
--import Notes
--import Index
--import Feed
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
taggedRules = posts & notes & tagIndex & postIndex & noteIndex & index -- & tagIndex & tagCloud & feed
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
    --tags <- generateTags
    simpleRules
    --posts tags
    --notes tags
    --tagPages tags
    generateTags >>= taggedRules
\end{code}