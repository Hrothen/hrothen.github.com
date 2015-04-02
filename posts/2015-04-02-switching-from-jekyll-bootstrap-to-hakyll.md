---
title: Switching from Jekyll Bootstrap to Hakyll
description: Building a better blog
---

I recently decided to switch from using [Jekyll Bootstrap](http://jekyllbootstrap.com/) to build my blog, to using [Hakyll.](http://jaspervdj.be/hakyll/index.html) Mostly just because I was rewriting all the html and css anyway and I wanted to give Hakyll a try, but also because I wanted to have a better idea of what was going on with each part of the site.

Getting started with Hakyll is pretty easy, the package comes with a program `hakyll-init` which creates a basic site for you to build on, and there are lots of sites using it that open source their code. Some of the stuff I wanted to do took some work to figure out or find though, so I thought I'd collect a list of it here.

##Adding Next and Previous Buttons to Posts
There are a number of ways to add next and previous buttons to the posts, I liked the method I found on [Richard Goulter's blog](https://github.com/rgoulter/my-hakyll-blog/blob/master/site.hs), but hakyll has added some new functions since that was written and we can cut it down a bit now.

```haskell
postList <- sortRecentFirst =<< getMatches "posts/*"

    match "posts/*" $ do

        -- strip date from filename when producing route
        route $ gsubRoute postDateRegex (const "posts/") `composeRoutes`
                setExtension "html"

        compile $ do
            let postLocationContext =
                    field "nextPost" (nextPostURL postList) `mappend`
                    field "prevPost" (prevPostURL postList) `mappend`
                    postCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postLocationContext
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" postLocationContext
                >>= relativizeUrls
```

With `sortRecentFirst` from Hakyll, I now only need to define `nextPostURL` and `prevPostURL`, which are almost identical.

```haskell
findPostUrl :: ([Identifier] -> Identifier -> Maybe Identifier)
             -> [Identifier] -> Item String
             -> Compiler String
findPostUrl p posts post = do
    let id = itemIdentifier post
    case p posts id of
        Just m  -> maybe empty toUrl <$> getRoute m
        Nothing -> empty


prevPostURL :: [Identifier] -> Item String -> Compiler String
prevPostURL = findPostUrl lookupPrev


nextPostURL :: [Identifier] -> Item String -> Compiler String
nextPostURL = findPostUrl lookupNext


lookupPrev :: Eq a => [a] -> a -> Maybe a
lookupPrev ids id = case elemIndex id ids of
                        Just i -> if i >= (length ids - 1) then Nothing else Just $ ids!!(i+1)
                        Nothing -> Nothing

lookupNext :: Eq a => [a] -> a -> Maybe a
lookupNext ids id = case elemIndex id ids of
                        Just i -> if i <= 0 then Nothing else Just $ ids!!(i-1)
                        Nothing -> Nothing
```

I also changed `lookupNext` and `lookupPrev` to use indicies because I managed to get them subtly wrong like, five times while setting up the site (in fact at the time of this writing they are wrong, and will hopefully be finally fixed for real when I post this).

##Setting up Redirects on Github Pages
Github doesn't let me directly tell the server to issue redirects, but fortunately HTML already has a way to do this. I'll create a template `redirect.html` that contains the following lines:

```html
<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8" />
        <meta http-equiv="refresh" content="0; url=/$postName$" />
    </head>
</html>
```

So then I just need to get a list of all the pages I want to redirect from and their targets, and then produce a tiny page for each one with `postName` replaced. This turns out to be fairly simple once you spend some time looking at the Hakyll docs. For extensibility, I'll use post metadata to decide what redirect pages to generate, this way I don't need to artificially separate my old posts and new posts, and if I want to I can create multiple aliases to a page.


```haskell
aliasList <- getAliases <$> getAllMetadata "posts/*"
let aliases = map fromFilePath $ snd $ unzip aliasList

create aliases $ do
        route idRoute
        compile $ do
            let aliasCtx = field "postName" (getRealName aliasList) `mappend`
                           defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/redirect.html" aliasCtx
                >>= relativizeUrls
```

I grab all the metadata for each file in `posts/`, and convert them into `(target,alias)` pairs. The list `aliases` contains only the alias names, which I use to generate the redirect pages, looking up the target page name in `aliasList` with `getRealName`. The actual work is done in `getAliases`, which filters out pages that don't define the `aliases` keyword and reformats titles to match my url scheme.

```haskell
stripPostDate :: FilePath -> FilePath
stripPostDate = replaceAll postDateRegex (const "posts/")

-- convert a list of (Identifier,Metadata) into a list of (target,alias)
getAliases :: [(Identifier,Metadata)] -> [(FilePath,FilePath)]
getAliases ids =
    let pairs = filter (not . null . snd) $ map expand ids
        paths = map (second (map addIndex) . first idToPath) pairs
    in concatMap unzipSecond paths

  where
    expand :: (Identifier,Metadata) -> (Identifier,[FilePath])
    expand = second (maybe [] read . M.lookup "aliases")

    idToPath :: Identifier -> FilePath
    idToPath = stripPostDate . toFilePath

    addIndex :: FilePath -> FilePath
    addIndex f = dropWhile (=='/')  $ dropExtension f </> "index.html"

-- get the path of the page an alias is pointing to
getRealName :: [(FilePath,FilePath)] -> Item String -> Compiler String
getRealName as i = do
    let id   = toFilePath $ itemIdentifier i
        path = fst $ fromJust $ find ((==id) . snd) as
    return $ replaceExtension path "html"

unzipSecond :: (a,[b]) -> [(a,b)]
unzipSecond (x,ys) = map (\a->(x,a)) ys
```

Some of this is specific to my site layout and title format, but most of it is general-purpose. If you haven't seen them before, `first` and `second` from `Control.Arrow` have kind of scary types, but the way most people use them is just to apply a function inside a tuple: `first foo (a,b) == (foo a,b)` and `second bar (a,b) == (a, bar b)`.

##Actually Deploying the Thing
So, if you're not using Jekyll, github wants you to just upload the static site directly, but this is a problem because I've got all this stuff that isn't part of the site sitting in the repo. The solution is to keep all the source in a separate branch, which I call `hakyll`, and then deploy by pushing only the built site in `_site/` to `master`. For this purpose I modified the `deploy.sh` script used by [Jorge Israel Peña](https://github.com/blaenk/blaenk.github.io/blob/source/src/deploy.sh) for his blog. Hakyll has a deploy command you can set in the site configuration, so I add

```haskell
siteConfig :: Configuration
siteConfig = defaultConfiguration{ deployCommand = "bash deploy.sh deploy" }
```

And replace the call to `hakyll` with `hakyllWith siteConfig` in `main`. Then I can just deploy the site by calling `./site deploy`.

Overall switching to Hakyll was pretty simple, and I didn't really have any issues figuring out how to do anything I wanted, so I'd call this a win.