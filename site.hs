--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (Alternative(..))
import           Control.Arrow (first, second)
import           Data.Functor ((<$>))
import           Data.List (stripPrefix, find)
import           Data.Maybe (fromJust)
import           Data.Monoid (mappend)

import           System.FilePath ((</>), dropExtension, replaceExtension)

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
                >>= loadAndApplyTemplate "templates/default.html" postLocationContext
                >>= relativizeUrls

    
    -- produce redirect pages for every page that defines the "aliases" metadata

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
lookupPrev ids id = lookup id $ zip (tail ids) ids

lookupNext :: Eq a => [a] -> a -> Maybe a
lookupNext ids id = lookup id $ zip ids (tail ids)


----------------------------------------------------------------------------
-- | Implement "aliases" metadata field for producing page redirects
--

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
    expand (a,x) = let x' = maybe [] read $ M.lookup "aliases" x
                   in (a,x')

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