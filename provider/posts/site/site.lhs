---
created: 2018-05-03T17:44:39Z
updated: 2018-05-17T20:00:53Z
tags: hakyll, haskell, generating-this-site, literate-programs
title: Generating this website
subtitle: Literally, literate
---

<aside>
The code contained here is all correct, such as *this is* the source that built this site.
However, I am still working on the documentation in this file and refactoring some of those more opaque code.
</aside>


This site is generated using [Hakyll][hakyll], a [Haskell][haskell] library for generating static websites.
The raw version of this file (see the source link at the bottom of this post) is compiled into the executable that generates this entire site, and in turn is presented as this post.
This is achieved by writing a [literate][literate] source file.

Assumptions
-----------

This is not intended as a beginner tutorial, so some working working knowledge of Haskell is assumed going forward.
As with all Haskell files, we have to import some modules.

First we import the [`Hakyll`][hakyll] module, which contains the core functionality needed to generate this site.
Next import is [`Hakyll.Web.Sass`][hakyll-sass] which provides a compiler for [scss][scss].

<aside>
Currently `hakyll-sass` is a fork I maintain.
todo: add link
I am working on submitting my PRs upstream and will move upstream once published to hackage.
</aside>

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Hakyll.Web.Sass  (sassCompiler)
\end{code}

Next is the Monad module, which facilitates composition. If not familiar with function programming concepts, it is worth looking into.

\begin{code}
import           Control.Monad    (msum, forM)
import           Data.Monoid      ((<>))
\end{code}

The `Data` and `System` modules provide many of the common functions you would expect in a programming language.

\begin{code}
import           Data.Char        (toLower)
import           Data.List        (sortBy, intercalate, isInfixOf)
import           Data.Maybe       (fromMaybe)
import           Data.Ord         (comparing)
import           Data.Time.Clock  (UTCTime (..))
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           System.FilePath  (replaceBaseName, splitFileName, takeBaseName, takeDirectory, (</>))
import           System.Process   (readProcess)
\end{code}

Finally we import some UTF8 encoding to keep things consistent.

\begin{code}
import           GHC.IO.Encoding  (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding, utf8)

\end{code}

Now that we have everything imported we can set some configuration.
The providerDirectory is where the built executable will look for source files when building the site.
By putting all the files in a sub folder we can keep root folder clean.

\begin{code}

config :: Configuration
config = defaultConfiguration
    {
      providerDirectory = "provider"
    }
\end{code}

Here we define the patterns we use to find content

A 'post' is an article that is ready to be published and included in the sites feed and listing.
Grab anything, no matter how deep in the post directory, regardless of the file extension.
\begin{code}
postsPattern :: Pattern
postsPattern = "posts/**"
\end{code}

A 'draft' is an article I am working on, and want rendered, but to not include in any listings.
It can be accessed by providing the direct URL to the page.
This is useful if I want to provide someone the ability to proof the post before going live.
Grab anything, no matter how deep in the drafts directory, regardless of the file extension.

\begin{code}
draftsPattern :: Pattern
draftsPattern = "drafts/**"
\end{code}

notes
whatever, so I can organize as I see fit.

\begin{code}
notesPattern :: Pattern
notesPattern = "notes/**"
\end{code}

Instantiate hakyll with UTF-8 encoding the above configuration.

\begin{code}
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyllWith config $ do
\end{code}

First we need to render our templates so we can apply our content onto them.

\begin{code}
    match "templates/*" $ compile templateBodyCompiler
\end{code}

Each of the following lines generates a portion of our content.
More detail will follow when we look at the function definition.

\begin{code}
    staticCss
    scss
    staticAssets
    index
    pages
    notesIndex
    postsIndex
    postsAndNotes
    secrets
    secretsStatic
\end{code}

I am using [normalize.css][normalize], along with a syntax.css file, these get copied directly to the output directory.

\begin{code}
staticCss :: Rules ()
staticCss = match "css/*.css" $ do
      route idRoute
      compile copyFileCompiler
\end{code}

todo: hakyll-sass

\begin{code}
scss :: Rules ()
scss = match "css/*.scss" $ do
      route $ setExtension "css"
      let compressCssItem = fmap compressCss
      compile (compressCssItem <$> sassCompiler)
\end{code}

The following files are just copied verbatim due to the `idRoute` and `copyFileCompiler` combination.

\begin{code}
staticAssets :: Rules ()
staticAssets = match "static/**" $ do
      route $ gsubRoute "static/" (const "")
      compile copyFileCompiler
\end{code}

index has some unique things.
Lists of most recent posts, and list of most recently updated notes

todo: context
todo: relativizeUrls
todo: removeIndexHtml

\begin{code}
index :: Rules ()
index = match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentlyCreatedFirst =<< loadAll postsPattern
            notes <- fmap (take 3) . recentlyUpdatedFirst =<< loadAll notesPattern
            let indexCtx =
                    listField "posts" siteContext (return posts) <>
                    listField "notes" siteContext (return notes) <>
                    constField "title" "Home"                <>
                    siteContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/site.html" indexCtx
                >>= relativizeUrls
                >>= withItemBody removeIndexHtml
\end{code}

Next we build the  static pages.
They live in the subfolder `pages`, strip that from the url.
compile the markdown with pandoc.
Drop them  into the site template

\begin{code}
pages :: Rules ()
pages = match "pages/*" $ do
        route $  subFolderRoute `composeRoutes` gsubRoute "pages/" (const "")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" siteContext
            >>= loadAndApplyTemplate "templates/site.html" siteContext
            >>= relativizeUrls
            >>= withItemBody removeIndexHtml
\end{code}

Create the notes index page
lexicography Ordered
needs a title

\begin{code}
notesIndex :: Rules ()
notesIndex = create ["notes.html"] $ do
      route subFolderRoute
      compile $ do
        orderedNotes <- lexicographyOrdered =<< loadAll notesPattern
        let ctx = constField "title" "Notes - Alphabetical" <>
                  listField "notes" siteContext (return orderedNotes) <>
                  siteContext
        makeItem ""
                >>= loadAndApplyTemplate "templates/notes.html" ctx
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= relativizeUrls
                >>= withItemBody removeIndexHtml
\end{code}

The typical 'archive' page is just /posts, a list of all posts.
One day, when I write a lot, I'll need to figure out pagination

\begin{code}
postsIndex :: Rules ()
postsIndex = create ["posts.html"] $ do
        route subFolderRoute
        compile $ do
            posts <- recentlyCreatedFirst =<< loadAll postsPattern
            let ctx = constField "title" "All Posts" <>
                    listField "posts" siteContext (return posts) <>
                    siteContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= relativizeUrls
                >>= withItemBody removeIndexHtml
\end{code}

<aside>
At this point my documentation is just a stream of conciousness to help myself and needs to be revisited.
Proceed at your own risk.
</aside>

todo:
posts and notes are treated the same.
Just a logical separation
`.||.` is logical OR for the match

\begin{code}
postsAndNotes :: Rules ()
postsAndNotes = match (postsPattern .||. notesPattern .||. draftsPattern) $ do
        route $ metadataRoute titleFromMetadata `composeRoutes` myPostsRoute `composeRoutes`  subFolderRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" siteContext
            >>= loadAndApplyTemplate "templates/site.html" siteContext
            >>= relativizeUrls
            >>= withItemBody removeIndexHtml
\end{code}

While I strive to keep everything open and transparent, some things are secret.
These I store in a private git repo and clone it into a folder named `secret` under provider.

First we create the secret pages.

\begin{code}
secrets :: Rules ()
secrets = match "secrets/*"  $ do
  route $ subFolderRoute `composeRoutes` gsubRoute "secrets/" (const "")
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/page.html" siteContext
    >>= loadAndApplyTemplate "templates/site.html" siteContext
    >>= relativizeUrls
    >>= withItemBody removeIndexHtml
\end{code}

As with the static above, just blindly copy everything in the secret folder to the site root.
Allows me to host any files arbitrarily.
todo: explain gsubRoute

\begin{code}
secretsStatic :: Rules ()
secretsStatic = match "secrets/**" $ do
  route $ gsubRoute "secrets/" (const "")
  compile copyFileCompiler
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

todo: contexts, what is it?

\begin{code}
shortDateFormat :: String
shortDateFormat = "%B %e, %Y"

siteContext :: Context String
siteContext = mconcat
    [ dateFromMetadata "created" "createdDateTime" shortDateFormat
    , dateFromMetadata "updated" "updatedDateTime" shortDateFormat
    -- , gitHistoryUrl "gitHistoryUrl"
    -- , gitCommitUrl "gitCommitUrl"
    -- , gitSourceUrl "gitSourceUrl"
    , gitEditUrl "gitEditUrl"
    , defaultContext
    ]
\end{code}

todo: make the below two functions into one

\begin{code}
dateFromMetadata :: String -> String -> String -> Context a
dateFromMetadata key value format = field value $ \i -> do
  t <- getMetadataField' (itemIdentifier i) key
  return $ formatTime defaultTimeLocale format (readTime t)
\end{code}

todo: git things

\begin{code}
-- gitLog :: String -> String -> IO String
-- gitLog filePath format =
--   readProcess "git" [
--     "log"
--   , "-1"
--   , "HEAD"
--   , "--pretty=format:" ++ format
--   , "--"
--   , filePath
--   ] ""
-- 
-- gitBranch :: IO String
-- gitBranch = do
--   branch <-readProcess "git" [
--       "rev-parse"
--     , "--abbrev-ref"
--     , "HEAD"
--     ] ""
--   return $trim branch
-- 
-- 
-- gitHistoryUrl :: String -> Context String
-- gitHistoryUrl key = field key $ \item -> do
--   let fp = "provider/" ++ toFilePath (itemIdentifier item)
--   unsafeCompiler $ do
--     sha     <- gitLog fp "%h"
--     branch  <- gitBranch
--     let github  =  "https://github.com/kyleondy/kyleondy.com"
--         history = github ++ "/commits/" ++ branch ++ "/" ++ fp
--     return $ if null sha
--                then "Not Committed"
--                else history
\end{code}

The url to an items commit in GitHub

\begin{code}
-- gitCommitUrl :: String -> Context String
-- gitCommitUrl key = field key $ \item -> do
--   let fp = "provider/" ++ toFilePath (itemIdentifier item)
--   unsafeCompiler $ do
--     sha     <- gitLog fp "%h"
--     let github  =  "https://github.com/kyleondy/kyleondy.com"
--         commit  = github ++ "/commit/" ++ sha
--     return $ if null sha
--                then "Not Committed"
--                else commit
\end{code}

The url to the item at the current time.

Todo: pull current branch out, so links where when I'm in local dev mode.

todo: combine functions

\begin{code}
-- gitSourceUrl :: String -> Context String
-- gitSourceUrl key = field key $ \item -> do
--   let fp     = "provider/" ++ toFilePath (itemIdentifier item)
--   unsafeCompiler $ do
--       branch <- gitBranch
--       return $ "https://github.com/kyleondy/kyleondy.com/blob/" ++ branch ++"/" ++ fp
-- 
gitEditUrl :: String -> Context String
gitEditUrl key = field key $ \item -> do
  let fp     = "provider/" ++ toFilePath (itemIdentifier item)
      branch = "main"
  do
      return $ "https://github.com/kyleondy/kyleondy.com/edit/" ++ branch ++ "/" ++ fp
\end{code}

-----------------
-- todo: below --
-----------------

\begin{code}
subFolderRoute :: Routes
subFolderRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                           where p = toFilePath ident
\end{code}

\begin{code}
dropDateRoute :: Routes
dropDateRoute = gsubRoute "/20[0-9]{2}" $ const ""

dropSiteRoute :: Routes
dropSiteRoute = gsubRoute "site" $ const ""

myPostsRoute :: Routes
myPostsRoute = dropDateRoute `composeRoutes` dropSiteRoute
\end{code}

todo: consolidate the below functions

\begin{code}
lexicographyOrdered :: [Item a] -> Compiler [Item a]
lexicographyOrdered items = return $
              sortBy (comparing (takeBaseName . toFilePath . itemIdentifier)) items

recentlyUpdatedFirst :: [Item a] -> Compiler [Item a]
recentlyUpdatedFirst items = do
    itemsWithTime <- forM items $ \item -> do
        updateTime <- getMetadataField (itemIdentifier item) "updated"
        return (updateTime,item)
    return $ reverse (map snd (sortBy (comparing fst) itemsWithTime))

recentlyCreatedFirst :: [Item a] -> Compiler [Item a]
recentlyCreatedFirst items = do
    itemsWithTime <- forM items $ \item -> do
        updateTime <- getMetadataField (itemIdentifier item) "created"
        return (updateTime,item)
    return $ reverse (map snd (sortBy (comparing fst) itemsWithTime))
\end{code}


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

todo:

\begin{code}
titleFromMetadata :: Metadata -> Routes
titleFromMetadata meta = maybe idRoute mkName (getField "title")
  where mkName t    = setBaseName $ slugify t
        getField    = (`lookupString` meta)
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

\begin{code}
slugify :: String -> String
slugify = intercalate "-" . words . map (\x -> if x `elem`  allowedChars then toLower x else ' ')
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
\end{code}

[hakyll]:         http://jaspervdj.be/hakyll
[haskell]:        https://www.haskell.org/
[literate]:       https://en.wikipedia.org/wiki/Literate_programming
[hakyll]:         https://jaspervdj.be/hakyll/
[hakyll-sass]:    https://github.com/KyleOndy/hakyll-sass
[scss]:           http://sass-lang.com/
[normalize]:      https://github.com/necolas/normalize.css
