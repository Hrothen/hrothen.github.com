--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (Alternative(..))
import           Data.Functor ((<$>))
import           Data.Function (on)
import           Data.List (intercalate, sortBy)
import           Data.Maybe (maybe)
import           Data.Monoid (mappend)
import           System.FilePath (takeFileName)

import           Data.Time.Format (parseTime)
import           System.Locale (defaultTimeLocale)
import           Data.Time.Clock (UTCTime)


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

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


    postList <- sortOnDate <$> getMatches "posts/*"

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            let postLocationContext =
                    field "nextPost" (nextPostURL postList) `mappend`
                    field "prevPost" (prevPostURL postList) `mappend`
                    postCtx
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postLocationContext
                >>= loadAndApplyTemplate "templates/default.html" postLocationContext
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


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

-- prevPostURL posts post = let p = lookupPrev posts $ itemIdentifier post
--                          in maybe empty mkURL p


mkURL :: Identifier -> Compiler String
mkURL p = (fmap (maybe empty $ toUrl) . getRoute) p

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
sortOnDate = reverse . sortBy (compare `on` asDate)
  where
    asDate :: Identifier -> Maybe UTCTime
    asDate =  parseTime' . takeFileName . toFilePath

    parseTime' = parseTime defaultTimeLocale "%Y-%m-%d"
               . intercalate "-" . take 3 . splitAll "-"