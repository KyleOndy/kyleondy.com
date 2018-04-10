---
date: 2017-10-09 19:00:00
updated: 2017-10-09 19:00:00
tags: hakyll, haskell, generating-this-site, literate-programs
title: Generating this website // Part 6
subtitle: Compliers
---

<aside class= "sidenote">
This is part six of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</aside>


\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Compilers (compressScssCompiler) where
import           Hakyll
-- import           Common
-- import           Content
-- import           Feed
-- import           Control.Applicative (liftA2)

compressScssCompiler :: Compiler (Item String)
compressScssCompiler = do
  fmap (fmap compressCss) $
    getResourceString
    >>= withItemBody (unixFilter "sass" [
\end{code}

Read from standard input instead of input file

\begin{code}
                                          "-s"
\end{code}
Use the CSS-superset SCSS syntax.
\begin{code}
                                        , "--scss"
\end{code}
Compress the css.
\begin{code}
                                        , "--style", "compressed"
                                        ])

\end{code}


[generating-this-website]: /tags/generating-this-site/
