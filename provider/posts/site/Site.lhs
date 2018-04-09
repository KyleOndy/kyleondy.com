---
date: 2017-09-25 12:00:00
updated: 2017-09-30 16:00:00
tags: hakyll, haskell, generating-this-site, literate-programs
title: Generating this website // Part 1
subtitle: Getting acquainted
---

<aside class= "sidenote">
This is part one of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</aside>


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import           GHC.IO.Encoding
import           Hakyll
import           Common
import           Content
import          Contexts
import           Compilers
import           Feed
import           Control.Applicative (liftA2)
\end{code}

Thigns that do not need tags

\begin{code}
templates, static, pages, sass, secrets, secretsStatic :: Rules ()
simpleRules :: Rules ()
simpleRules = do
  templates
  static
  pages
  sass
  secrets
  secretsStatic

templates = match "templates/**" $ compile templateCompiler

static = match staticPattern $ do
  route $ gsubRoute "static/" (const "")
  compile copyFileCompiler

pages = match pagesPattern $ do
  route $  subFolderRoute `composeRoutes` gsubRoute "pages/" (const "")
  compile $ customPandocCompiler
    >>= loadAndApplyTemplate "templates/page.html" defaultCtx
    >>= loadAndApplyTemplate "templates/default.html" defaultCtx
    >>= relativizeUrls
\end{code}

\begin{code}
sass = match "css/*.sass" $ do
  route $ setExtension "css"
  compile compressScssCompiler
\end{code}

For secret things

These don't resolve with git
\begin{code}
secrets = match "secrets/**.markdown" $ do
  route $ subFolderRoute `composeRoutes` gsubRoute "secrets/" (const "")
  compile $ customPandocCompiler
    >>= loadAndApplyTemplate "templates/page.html" defaultCtx
    >>= loadAndApplyTemplate "templates/default.html" defaultCtx
    >>= relativizeUrls

secretsStatic = match "secrets/**" $ do
  route $ gsubRoute "secrets/" (const "")
  compile copyFileCompiler
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

[generating-this-website]: /tags/generating-this-site/
