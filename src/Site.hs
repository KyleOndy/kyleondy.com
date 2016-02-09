--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
module Main (main) where


--------------------------------------------------------------------------------
import           Control.Monad   (forM_)
import           Data.Monoid     ((<>))
import           Data.List       (isSuffixOf, sortBy)
import           Data.Ord        (comparing)
import           Prelude         hiding (id)
import           System.FilePath
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
-- | patters
postsPattern :: Pattern
postsPattern = "posts/*"

newsPattern :: Pattern
newsPattern = "news/*"

pagesPattern :: Pattern
pagesPattern = "pages/*"

notesPattern :: Pattern
notesPattern = "notes/*"

staticFiles :: [Pattern]
staticFiles = [ "images/**"
               , "static/**"
               , "files/**"
               , "robots.txt"
               , "keybase.txt"
               , "favion*"
               , "pgp.txt"
               ]

--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = hakyllWith config $ do

    -- Static files
    forM_ staticFiles $ \pattern ->
      match pattern $ do
        route idRoute
        compile copyFileCompiler


    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Build tags
    tags <- buildTags (postsPattern .||. newsPattern .||. notesPattern) (fromCapture "tags/*.html")

    -- Render each and every post
    match postsPattern $ do
        route   $ postCleanRoute
        compile $ do
            customPandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- Post list
    create ["posts.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- notes page
    create ["notes.html"] $ do
      route cleanRoute
      compile $ do
        notes <- lexicographyOrdered =<< loadAll notesPattern
        let ctx = constField "title" "Notes" <>
                  listField "notes" (postCtx tags) (return notes) <>
                  defaultContext
        makeItem ""
                >>= loadAndApplyTemplate "templates/notes.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- Post tags
    tagsRules tags $ \tag p-> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll p
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- News items
    match newsPattern $ do
      route   $ cleanRoute
      compile $ customPandocCompiler
             >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
             >>= loadAndApplyTemplate "templates/content.html" defaultContext
             >>= loadAndApplyTemplate "templates/default.html" defaultContext
             >>= relativizeUrls
             >>= cleanIndexUrls

    -- Notes
    match notesPattern $ do
      route   $ cleanRoute
      compile $ customPandocCompiler
             >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
             >>= loadAndApplyTemplate "templates/content.html" defaultContext
             >>= loadAndApplyTemplate "templates/default.html" defaultContext
             >>= relativizeUrls
             >>= cleanIndexUrls

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll postsPattern
            news  <- recentFirst =<< loadAll newsPattern
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    listField "news"  (postCtx tags) (return news) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render some static pages
    match pagesPattern $ do
        route   $ cleanRoute `composeRoutes` (gsubRoute "pages/" (const ""))
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route cleanRoute
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= cleanIndexUrls

    -- feed
    create ["feed.xml"] $ do
      route   idRoute
      compile $ do
        let feedCtx = (postCtx tags) `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots postsPattern "content"
        renderAtom feedConfiguration feedCtx posts
          >>= cleanIndexHtmls

    -- CV as HTML
    match "cv.markdown" $ do
        route   $ cleanRoute
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/cv.html"      defaultContext
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls


--need to turn off email obfuscation in pandoc
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith defaultHakyllReaderOptions writeOptions
        where
          writeOptions = defaultHakyllWriterOptions {writerEmailObfuscation = NoObfuscation}

lexicographyOrdered :: [Item a] -> Compiler [Item a]
lexicographyOrdered items = return $
              sortBy (comparing (takeBaseName . toFilePath . itemIdentifier)) items


--------------------------------------------------------------------------------

defaultCtx :: Context String
defaultCtx = mconcat
  [ niceUrlField  "url" ]


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]


--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


-- url field without trailing /index.html
niceUrlField :: String -> Context a
niceUrlField key = field key niceItemUrl

niceItemUrl :: Item a -> Compiler String
niceItemUrl =
  fmap (maybe "" (removeIndexStr . toUrl)) . getRoute . setVersion Nothing . itemIdentifier
  where removeIndexStr url = case splitFileName url of
            (dir, "index.html") -> dir
            _ -> url

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    {
      providerDirectory = "_build"
    , destinationDirectory = "_site"
    , storeDirectory = "_cache"
    , tmpDirectory = "_cache/tmp"
    }



--------------------------------------------------------------------------------
feedConfiguration ::  FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Kyle Ondy"
    , feedDescription = "Personal blog of Kyle Ondy"
    , feedAuthorName  = "Kyle Ondy"
    , feedAuthorEmail = "kyle@ondy.me"
    , feedRoot        = "https://kyleondy.com"
    }



--------------------------------------------------------------------------------
-- | custom routes
postCleanRoute :: Routes
postCleanRoute = cleanRoute
  -- The regex removes the year so all posts are /posts/{title}/index.html
  `composeRoutes` (gsubRoute "(posts|drafts)/" (const "posts/"))

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
