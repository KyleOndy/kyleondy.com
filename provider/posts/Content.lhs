---
date: 2017-10-03
updated: 2017-10-03
tags: hakyll, haskell, generating this site
title: Generating this website // Part 2
subtitle: Making posts
---

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Content where
import Hakyll
import Common
import           Data.Monoid         ((<>))

posts :: Tags -> Rules()
posts tags = taggedContent postsPattern tags

notes :: Tags -> Rules()
notes tags = taggedContent notesPattern tags

taggedContent :: Pattern -> Tags -> Rules()
taggedContent pattern tags = match pattern $ do
  route $ subFolderRoute
  compile $ do
    customPandocCompiler
      >>= saveSnapshot "content"
      >>= return . fmap demoteHeaders
      >>= relativizeUrls
      >>= loadAndApplyTemplate "templates/post.html" (taggedCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
\end{code}

\begin{code}

postIndex :: Tags -> Rules()
postIndex tags = create ["posts.html"] $ do
  route subFolderRoute
  compile $ do
    posts <- recentFirst =<< loadAll postsPattern
    let ctx = constField "title" "Posts - Most Recent" <>
                listField "posts" (taggedCtx tags) (return posts) <>
                defaultContext
    makeItem ""
      >>= loadAndApplyTemplate "templates/posts.html" ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
			
noteIndex :: Tags -> Rules()
noteIndex tags = create ["notes.html"] $ do
  route subFolderRoute
  compile $ do
    notes <- lexicographyOrdered =<< loadAll notesPattern
    let ctx = constField "title" "Notes - Alphabetical" <>
              listField "posts" (taggedCtx tags) (return notes) <>
              defaultContext
    makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

\end{code}

\begin{code}
index :: Tags -> Rules()
index tags = match "index.html" $ do
    -- todo: not sure this is the best way to handle index.html
    route $ gsubRoute "index/" (const "")
    compile $ do
        recentPosts <- fmap (take 3) . recentUpdatedFirst =<< loadAll postsPattern
        recentNotes <- fmap (take 3) . recentUpdatedFirst =<< loadAll notesPattern
        let indexContext =
                listField "posts" (taggedCtx tags) (return recentPosts) <>
                listField "notes" (taggedCtx tags) (return recentNotes) <>
                field "tags" (\_ -> renderTagList tags) <>
                defaultContext
        getResourceBody
            >>= applyAsTemplate indexContext
            >>= loadAndApplyTemplate "templates/default.html" indexContext
            >>= relativizeUrls
\end{code}


Need to have these or the tags will not show up.

\begin{code}
tagIndex :: Tags -> Rules()
tagIndex tags = tagsRules tags $ \tag p-> do
      let title = "Tagged: " ++ tag
      route subFolderRoute
      compile $ do
          posts <- recentFirst =<< loadAll p
          let ctx = constField "title" title <>
                      listField "posts" (taggedCtx tags) (return posts) <>
                      defaultContext
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html" ctx
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls
\end{code}