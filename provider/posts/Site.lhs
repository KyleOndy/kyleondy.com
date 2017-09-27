---
date: 2017-09-25
updated: 2017-09-26
tags: hakyll, haskell, generating this site
title: Generating this website // Part 1
subtitle: Getting acquainted
---

<aside class="sidenote">
This is part one of the "generating this site" series of posts.
You can read the rest of the series [here][gensite].
</aside>

I have recently moved this site to some new hosting and am taking the oppertunity at provide some content.
When tweaking my config file for [hakyll] I had the thought that I could write the configuration in [literate hasekll], providing content and doccumenting my code.o
This post is litteraly the source code that compiles the application that builds this site.
So this post can compile an application which than can take this post as input and produce this post as output.
Need a totem?

<h1>Lets get started!</h1>

And so began the series of [Generating this website][gensite].
This series will assume you have a basic working knowledge of Haskell or another functional language.
Experice with Hakyll isn't needed either, but having used a static site generate before will make some concepts easier to grasp.
If anything isn't clear feel free to [contact] me or submit a [pull request] against this file.

<aside class="sidenote">
From here and below is still in the process of being cleaned up.
I will be breaking out large chunks of functionality into their own modules, and creating a post for each.
</aside>

Here are two lines which will be seen a lot as we go forward:

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
\end{code}

The `OverloadedStrings` LANGUAGE pragma enables many thing I wish were enabled by default within Haskell.
While you can define pragma in the projects `cabal` file, I prefer to explicty declare it for clairty.

Nest we import Hakyll, which gives us access to the DSL Haskell provides.

\begin{code}
import           Data.List           (isSuffixOf, sortBy)
import           Data.Monoid         ((<>))
import           Data.Ord            (comparing)
import qualified Data.Set            as S (insert)
import           Prelude             hiding (id)
import           System.FilePath
import           Text.Pandoc.Options
import           GHC.IO.Encoding
import Control.Monad (forM)


postsPattern :: Pattern
postsPattern = "posts/*"

pagesPattern :: Pattern
pagesPattern = "pages/*"

notesPattern :: Pattern
notesPattern = "notes/*"

staticPattern :: Pattern
staticPattern = "static/**"

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  hakyllWith config $ do
\end{code}

The contents in the `static` folder get copied to the root of the deployed site

\begin{code}
    match staticPattern $ do
      route $ gsubRoute "static/" (const "")
      compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    tags <- buildTags (postsPattern .||. notesPattern) (fromCapture "tags/*.html")

    match postsPattern $ do
        route   $ postCleanRoute
        compile $ do
            customPandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
                >>= cleanIndexUrls

    create ["posts.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls
\end{code}

To avoid having more templates than we need, we return a field called `posts` from notes so we can reuse that tempalte

\begin{code}
    create ["notes.html"] $ do
      route cleanRoute
      compile $ do
        notes <- lexicographyOrdered =<< loadAll notesPattern
        let ctx = constField "title" "Notes" <>
                  listField "posts" (postCtx tags) (return notes) <>
                  defaultContext
        makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    tagsRules tags $ \tag p-> do
        let title = "Tagged: " ++ tag
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll p
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match notesPattern $ do
      route   $ cleanRoute
      compile $ customPandocCompiler
             >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
             >>= loadAndApplyTemplate "templates/default.html" defaultContext
             >>= relativizeUrls
             >>= cleanIndexUrls

    match "pages/index.html" $ do
        route $ gsubRoute "pages/" (const "")
        compile $ do
            posts <- fmap (take 3) . recentUpdatedFirst =<< loadAll postsPattern
            notes <- fmap (take 3) . recentUpdatedFirst =<< loadAll notesPattern
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    listField "notes" (postCtx tags) (return notes) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler

    match pagesPattern $ do
        route   $ cleanRoute `composeRoutes` (gsubRoute "pages/" (const ""))
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "404.html" $ do
        route cleanRoute
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    create ["feed.xml"] $ do
      route   idRoute
      compile $ do
        let feedCtx = (postCtx tags) `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots postsPattern "content"
        renderAtom feedConfiguration feedCtx posts
          >>= cleanIndexHtmls

    match "cv.markdown" $ do
        route   $ cleanRoute
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/cv.html"      defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls


customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
    let customExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions customExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerEmailObfuscation = NoObfuscation
                        , writerExtensions = newExtensions
                        , writerHtml5            = True
                        , writerHighlight        = True
                        , writerHighlightStyle   = pygments
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

lexicographyOrdered :: [Item a] -> Compiler [Item a]
lexicographyOrdered items = return $
              sortBy (comparing (takeBaseName . toFilePath . itemIdentifier)) items
\end{code}

Sort items by their `updated` tag.
There is propably a better way to do this.
Taken from [here](https://groups.google.com/forum/#!topic/hakyll/pa2YqSmnbEA).

\begin{code}
recentUpdatedFirst :: [Item a] -> Compiler [Item a]
recentUpdatedFirst items = do
    itemsWithTime <- forM items $ \item -> do
        updateTime <- getMetadataField (itemIdentifier item) "updated"
        return (updateTime,item)
    return $ reverse (map snd (sortBy (comparing fst) itemsWithTime))


defaultCtx :: Context String
defaultCtx = mconcat
  [ niceUrlField  "url" ]
\end{code}

todo: add formatting to make `updated` field be pretty date

\begin{code}
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

niceUrlField :: String -> Context a
niceUrlField key = field key niceItemUrl

niceItemUrl :: Item a -> Compiler String
niceItemUrl =
  fmap (maybe "" (removeIndexStr . toUrl)) . getRoute . setVersion Nothing . itemIdentifier
  where removeIndexStr url = case splitFileName url of
            (dir, "index.html") -> dir
            _ -> url

config :: Configuration
config = defaultConfiguration
    {
      providerDirectory = "provider"
    , destinationDirectory = "_site"
    , storeDirectory = "_cache"
    , tmpDirectory = "_cache/tmp"
    }


feedConfiguration ::  FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Kyle Ondy"
    , feedDescription = "Personal blog of Kyle Ondy"
    , feedAuthorName  = "Kyle Ondy"
    , feedAuthorEmail = "kyle@ondy.me"
    , feedRoot        = "https://kyleondy.com"
    }



postCleanRoute :: Routes
postCleanRoute = cleanRoute
  `composeRoutes` (gsubRoute "(posts|drafts)/" (const "posts/"))

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                           where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll patn replacement)
    where
      patn = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

\end{code}

[gensite]:        /tags/generating this site/
[contact]:        /contact
