---
date: 2017-10-04 07:32:12
updated: 2017-10-04 07:32:12
tags: hakyll, haskell, generating-this-site, literate-programs
title: Generating this website // Part 4
subtitle: Making posts
---

<aside class= "sidenote">
This is part four of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</aside>

By this point we have the basic features up and running, and everything we do
from here on are more or less "nice to have"s.  First of which is an Atom
feed, so people using feed readers can easily keep track of new posts.
Feeds seem to have become less popular recently, which I think is a shame
since I find them to be an incredibly easy way to keep on top of new content
across the various communities I'm interested in.  Perhaps I am part of the
problem, however, as I recently removed the Atom feed link from the header
during the redesign of this website over the new year holidays.  It may come
as a surprise, then, that I have a feed at all!  I do, and you can access it
at <https://www.kyleondy.com/feed.xml>, which I had thought was a standard location
which browsers and feed readers would pick up automatically.  It turns out I
was wrong about that, so I guess I'll have to add the link back in.
Anyway, here's how I generate the above feed.  There's not a lot new in this
post that isn't already covered in the [official documentation on
feeds][hakyll-feeds], but here it is.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Feed where
import Hakyll
import Common (postsPattern, taggedCtx)
\end{code}


Hakyll supports feeds natively and provides the *`FeedConfiguration`* type to
configure the feed's properties.  Here's mine.
\begin{code}
feedConfiguration ::  FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Kyle Ondy"
    , feedDescription = "Personal blog of Kyle Ondy"
    , feedAuthorName  = "Kyle Ondy"
    , feedAuthorEmail = "kyle@ondy.me"
    , feedRoot        = "https://kyleondy.com"
    }
\end{code}

Now we can generate the feed itself.  In this series so far, you've already
seen many times that to generate anything we need a *`Compiler`* and a
*`Context`*, which we can then refer to in a set of *`Rules`* which will
determine where and from what source files the content is made.  In this case,
all of these things are so simple we just define them inline.

\begin{code}
feed :: Tags -> Rules()
feed tags = create ["feed.xml"] $ do
  route idRoute
  compile $ do
    allContent <- loadAllSnapshots postsPattern "content"
    posts <- take 10 <$> recentFirst allContent
    renderAtom feedConfiguration feedCtx posts
  where feedCtx = taggedCtx tags `mappend` bodyField "description"
\end{code}

The feed context is just the post context augmented with the `description`
field.  You may remember when we were [generating the posts][generating-posts]
we used a function called `saveSnapshot` to save a copy of the content before
adding headers and other global design elements.  Here we load that content
and put it in the `description` field for the post, resulting in an Atom feed
which contains the entire post as its description, so that it can be read from
within the feed reader of your choice.

[hakyll-feeds]: http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html