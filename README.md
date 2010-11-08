# webmine

A web mining toolkit for Clojure. A swiss army knife for processing text, images, and feeds from HTML.

The library gives you the most common tools you need out of the box, but give is fine grained enough to build your custom processing tools.

## link extraction and url fu

Get a seq of urls, or remove all urls, from an arbitrary string containing urls.

    (url-seq raw-text)

    (remove-urls raw-text)

Expand a shortened url (e.g. from bitly)

    (expand short-url)


## HTML parsing

Webmine wraps [tagsoup](http://home.ccil.org/~cowan/XML/tagsoup/) for parsing; handles malformed HTML.

You get a dom tree from a raw html string with:

    (def d (dom source-string))

From there, you can do all sorts of things:

    (text-from-dom d)

    (strip-non-content d)

	(attr-map d)
	
	(links-from-dom d)

    (divs d)

If you don't find what you need, you can write arbitrary transformation on the DOM tree using walk-dom

	(walk-dom dom visit-node-fn accum-res-fn)

## feeds (rss/atom)

Get the current entries for a feed.

    (entries feed-url)

Identify the canonical rss/atom feeds from a given seq of urls. 

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

Size fu checks attrs and style tags for the most likely main image.  We can also fetch the images to get their dimensions if the attrs and style tags both fail.

    (fetch-sizes some-imgs)

## text and dom processing

We have dom scrubbing fu in webmine.readability based on readability.js. A nice feature of our readability port is that it's easy to change how a div is scored for readability to suit the data you're working with. webmine.readability is used in webmine.images to find the div most likely to contain the main page image:

	(readability-div d)


## Installation

For leiningen:
    [webmine "0.1.1"]

## Features In The Pipeline

- Lucene integration for search 
- Better readability fu
- Structured data extraction (e.g., HTML tables, lists mapped to clojure data structures)
- Splog detection

## Authors

- Copyright (c) Bradford Cross, Matt Revelle, and Aria Haghighi released under the MIT License (http://www.opensource.org/licenses/mit-license.php).

 

