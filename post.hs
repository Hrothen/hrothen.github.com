{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Data.List (find, intercalate)
import System.Environment (getArgs)
import System.IO (IOMode(..), openFile, hClose)
import Control.Arrow (second)
import Data.Char (isAlphaNum)

import qualified Data.Text as T
import Data.Text (Text(..), pack, unpack)
import Data.Text.IO (hPutStrLn)

import Data.Time

import System.FilePath ((</>), (<.>))

main :: IO ()
main = do
    
    metaData       <- map (second T.tail . T.span (/='=') . pack) <$> getArgs
    date <- formatDate . toGregorian . utctDay <$> getCurrentTime
    
    let -- date      = intercalate "-" $ map show [year, toInteger mon, toInteger day]
        title     = maybe "" snd $ find ((=="title") . fst) metaData
        fileTitle = unpack $ removeIllegalChars $ replaceSpaces $ T.toLower title
        filename  = "posts" </> date ++ '-':fileTitle <.> "md"
    
    file <- openFile filename WriteMode
    
    hPutStrLn file "---"
    mapM_ (hPutStrLn file . formatMetadata) metaData
    hPutStrLn file "---"
    
    hClose file

formatDate :: (Integer,Int,Int) -> String
formatDate (y,m,d) = intercalate "-" [show y,pad m, pad d]
  where pad = pad' . show
        pad' i | length i == 1 = '0':i
               | otherwise = i

removeIllegalChars :: Text -> Text
removeIllegalChars = T.filter (\c-> isAlphaNum c || c == '-')

replaceSpaces :: Text -> Text
replaceSpaces = T.replace " " "-"

formatMetadata :: (Text,Text) -> Text
formatMetadata (name,md) = name <> ": " <> md