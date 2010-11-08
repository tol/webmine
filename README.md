# webmine

A web mining toolkit for Clojure.

## link extraction and url fu

Get a seq of urls, or remove all urls, from an arbitrary string containing urls.

    (url-seq raw-text)

    (remove-urls raw-text)

Expand a shortened url

    (expand short-url)


## html parsing

Webmine wraps tagsoup for parsing.

You get a dom from a raw html string with:

    (def d (dom source-string))

From there, you can do all sorts of things:

    (text-from-dom d)

    (strip-non-content d)

    (divs d)

## feeds (rss/atom)

Get the current entries for a feed.

    (entries feed-url)

Get the canonical rss/atom feeds from a given seq of urls.

    (canonical-feeds feed-urls)

Given the url of a blog's homepage or rss feed, find the outlinks to feeds from both the homepage, and all the entries currently in this blog's feed.

    (feed-outlinks feed-url)

Get the blogroll someone is following from their opml.

    (blogroll opml-file)


## images

Get the most relevant image at a particular url.

    (best-img-at some-url)

Get all the images and their sizes out of a dom.

    (def some-imgs (imgs d))

Size fu checks attrs and style tags.  We can also fetch the images to get their dimesions if the attrs and style tags both fail.

    (fetch-sizes some-imgs)

## text and dom processing

We have some dom scrubbing fu in webmine.readability based on readability.js.  We use it in webmine.images to find the best div to pick the most relevant image.

   (readability-div d)


## Installation

For leiningen:
    ["webmine" "0.1.1"]

