--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (Alternative(..))
import           Control.Arrow (first, second)
import           Data.Functor ((<$>))
import           Data.Function (on)
import           Data.List (intercalate, sortBy, stripPrefix, find)
import           Data.Maybe (maybe, fromJust)
import           Data.Monoid (mappend)
import           System.FilePath (takeFileName, (</>), dropExtension,
                                  replaceExtension)

import           Data.Time.Format (parseTime)
import           System.Locale (defaultTimeLocale)
import           Data.Time.Clock (UTCTime)

import qualified Data.Map.Lazy as M


import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


    match "about.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


    postList <- sortOnDate <$> getMatches "posts/*"

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
                >>= loadAndApplyTemplate "templates/default.html" postLocationContext
                >>= relativizeUrls

    -- go through the posts and generate redirect pages for the old URL style
    --match "posts/*" $ version "redirects" $ do
    --    route $ gsubRoute postDateRegex formatOldPost `composeRoutes`
    --            setExtension "html"

    --        --generate minimal files to redirect old URLs to the current ones
    --    compile $ do
    --        let postRouteContext =
    --                field "postName" redirectTarget `mappend`
    --                postCtx
    --        pandocCompiler
    --            >>= loadAndApplyTemplate "templates/redirect.html" postRouteContext

    aliasList <- getAliases <$> getAllMetadata "posts/*"
    let aliases = map fromFilePath $ snd $ unzip aliasList

    create aliases $ do
        route idRoute
        compile $ do
            let aliasCtx = field "postName" (target aliasList) `mappend`
                           defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/redirect.html" aliasCtx
                >>= relativizeUrls


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- match the post/date part of the post filename
postDateRegex :: String
postDateRegex = "posts/[0-9]{4}-[0-9]{2}-[0-9]{2}-"

----------------------------------------------------------------------------
-- | Implement "next" and "previous" fields for posts
-- Mostly taken from Richard Goulter's blog
-- https://github.com/rgoulter/my-hakyll-blog
--

prevPostURL :: [Identifier] -> Item String -> Compiler String
prevPostURL posts post = do
    let id   = itemIdentifier post
        prev = lookupPrev posts id
    case prev of Just p  -> (fmap (maybe empty $ toUrl) . getRoute) p
                 Nothing -> empty


nextPostURL :: [Identifier] -> Item String -> Compiler String
nextPostURL posts post = do
    let id   = itemIdentifier post
        next = lookupNext posts id
    case next of Just n  -> maybe empty toUrl <$> getRoute n
                 Nothing -> empty


lookupPrev :: Eq a => [a] -> a -> Maybe a
lookupPrev ids id = lookup id $ zip (tail ids) ids

lookupNext :: Eq a => [a] -> a -> Maybe a
lookupNext ids id = lookup id $ zip ids (tail ids)


sortOnDate :: [Identifier] -> [Identifier]
sortOnDate = sortBy (compare `on` asDate)
  where
    asDate :: Identifier -> Maybe UTCTime
    asDate =  parseTime' . takeFileName . toFilePath

    parseTime' = parseTime defaultTimeLocale "%Y-%m-%d"
               . intercalate "-" . take 3 . splitAll "-"

----------------------------------------------------------------------------
-- | utils
--

redirectTarget :: Item a -> Compiler String
redirectTarget item = do
    let path       = toFilePath $ itemIdentifier item
        stripDate  = replaceAll postDateRegex (const "posts/")
        dropSuffix = replaceAll ".md" (const ".html")
    return $ dropSuffix $ stripDate path


-- take a string that looks like "posts/yyyy-mm-dd-"
-- and turn it into yyyy/mm/dd/
formatOldPost :: String -> String
formatOldPost = replaceAll "-" (const "/") . fromJust . stripPrefix "posts/"

getAliases :: [(Identifier,Metadata)] -> [(String,String)]
getAliases ids =
    let pairs = filter (not . null . snd) $ map expand ids
        paths = map (second (map addIndex) . first idToPath) pairs
    in concatMap unzipSecond paths

  where
    expand :: (Identifier,Metadata) -> (Identifier,[String])
    expand (a,x) = let x' = maybe [] read $ M.lookup "aliases" x
                   in (a,x')

    idToPath :: Identifier -> FilePath
    idToPath = replaceAll postDateRegex (const "posts/") . toFilePath

    addIndex :: FilePath -> FilePath
    addIndex f = dropWhile (=='/')  $ (dropExtension f) </> "index.html"

target :: [(String,String)] -> Item String -> Compiler String
target as i = do
    let id   = toFilePath $ itemIdentifier i
        path = fst $ fromJust $ find ((==id) . snd) as
    return $ replaceExtension path "html"

unzipSecond :: (a,[b]) -> [(a,b)]
unzipSecond (x,ys) = map (\a->(x,a)) ys