---
date: 2017-10-09 19:00:00
updated: 2017-10-09 19:00:00
tags: hakyll, haskell, generating-this-site, literate-programs
title: Generating this website // Part 5
subtitle: Contexts
---

<aside class= "sidenote">
This is part five? of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</aside>


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Contexts(defaultCtx,taggedCtx) where
import Hakyll
import  Control.Monad       (msum)
import  Data.Maybe          (fromMaybe)
import Data.Monoid (mconcat)
import  Data.Time.Clock     (UTCTime (..))
import System.Process (readProcess)

import  Data.Time.Format    (parseTimeM, defaultTimeLocale, formatTime)

dateFormat :: String
dateFormat = "%B %e, %Y"

updatedField :: String -> String -> Context a
updatedField key format = field key $ \i -> do
  t <- getMetadataField' (itemIdentifier i) "updated"
  return $ (formatTime defaultTimeLocale format $ readTime t)

defaultCtx :: Context String
defaultCtx = mconcat
    [ dateField "date" dateFormat
    , updatedField "updated" dateFormat
    , gitHistoryUrl "gitHistoryUrl"
    , gitCommitUrl "gitCommitUrl"
    , gitSourceUrl "gitSourceUrl"
    , defaultContext
    ]

taggedCtx :: Tags -> Context String
taggedCtx tags = mconcat
    [ tagsField "tags" tags
    , defaultCtx
    ]
\end{code}

todo: break into gitCommit and gitHistory

The url to an items github history

\begin{code}
gitLog :: String -> String -> IO String
gitLog filePath format =
  readProcess "git" [
    "log"
  , "-1"
  , "HEAD"
  , "--pretty=format:" ++ format
  , "--"
  , filePath
  ] ""
\end{code}

\begin{code}
gitHistoryUrl :: String -> Context String
gitHistoryUrl key = field key $ \item -> do
  let fp = "provider/" ++ (toFilePath $ itemIdentifier item)
  unsafeCompiler $ do
    sha     <- gitLog fp "%h"

    let github  =  "https://github.com/kyleondy/kyleondy.com"
        history = github ++ "/commits/master/" ++ fp

    return $ if null sha
               then "Not Committed"
               else history
\end{code}

The url to an items commit in GitHub

\begin{code}
gitCommitUrl :: String -> Context String
gitCommitUrl key = field key $ \item -> do
  let fp = "provider/" ++ (toFilePath $ itemIdentifier item)
  unsafeCompiler $ do
    sha     <- gitLog fp "%h"

    let github  =  "https://github.com/kyleondy/kyleondy.com"
        commit  = github ++ "/commit/" ++ sha

    return $ if null sha
               then "Not Committed"
               else commit
\end{code}

The url to the item at the current time.

\begin{code}
gitSourceUrl :: String -> Context String
gitSourceUrl key = field key $ \item -> do
  let fp = "provider/" ++ (toFilePath $ itemIdentifier item)
  return $ "https://github.com/kyleondy/kyleondy.com/blob/master/" ++ fp
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
