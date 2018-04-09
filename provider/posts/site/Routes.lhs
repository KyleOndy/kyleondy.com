---
date: 2017-10-10 07:32:12
updated: 2017-10-10 07:32:12
tags: hakyll, haskell, generating-this-site, literate-programs
title: Generating this website // Part todo - routes
subtitle: Routes
---

<aside class= "sidenote">
This is part four of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</aside>

\begin{code}
module Routes (
  slugify
) where

import Data.List (intercalate)
import Data.Char (toLower)


slugify :: String -> String
slugify = intercalate "-" . words . map (\x -> if x `elem`  allowedChars then toLower x else ' ')
  where allowedChars = (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
\end{code}
