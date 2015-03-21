---
title: Let's Build a Browser Engine in Haskell: Part 2
description: In which we parse HTML
category: 
tags: []
---

Ok, so in [part 1](lets-build-a-browser-engine-in-haskell.html) we built a set of types to use for a really basic DOM. In part 2, we'll write a simple html parser (actually we'll write two) that only supports Balanced Tags, Attributes with quoted values, and Text nodes.

#Why two parsers?#

First, we're going to write a straight port of Matt's Rust parser, pretty much just to see how it goes. Then we're gonna turn around write a more Haskell flavored parser, since writing parsers is something we can do really really well in Haskell. In this way we explore the similarities and differences between languages, as well as the design process that led to today's modern applicative parsing libraries.

Ok no that's not true, I wrote the whole first parser at two in the morning before I remembered that Parsec is part of the Haskell Platform, so now you get to experience it too (to be fair, it's a pretty decent example of using monad transformers, I feel).

Parser 1: port from Rust
========================

First things first, we're switching from using `String`s, which are just lists of `Char`s to `Text`s which are 1) designed to deal with all the different utf formats and 2) way faster. A nice side effect of this is that we can treat everything like regular `Char`s and the compiler will figure it out for us. If you're following along, go back and update your types in Dom.hs.

First we'll create a Parser type, in Robinson the Parser consists of a vector and an index, but for our purposes it'll be better to consume the `Text` as we read it. We don't have to worry about accidentally destroying it, and in theory the compiler should fuse everything together into a single traversal.

```haskell
-- here's all the imports for the file
{-# LANGUAGE OverloadedStrings #-} -- this lets the compiler turn string literals into Texts

import Data.Char (isAlphaNum)
import Control.Monad (liftM)

import Control.Monad.State.Lazy (StateT(..), evalState, get, put)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Identity

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Dom


data Parser = Parser T.Text

type ParserS = ExceptT T.Text (StateT Parser Identity)
```

We've defined a `Parser` as just a wrapper around a `Text`, and a `ParserS` monad transformer stack where all our parsing code will live. If you're not familiar with monad transformers you should go read [Monad Transformers Step by Step](http://www.grabmueller.de/martin/www/pub/Transformers.pdf) which, despite being a .pdf file, is actually a short introductory tutorial. To read this post though, all you need to know is that `ParserS` basically functions as a `State a` and an `Either T.Text a` at the same time, so we can carry our `Parser` along and also toss up an error if something goes wrong.

We'll also define a few convenience functions for working with the `Parser`.

```haskell
runParserS p s = evalState (runExceptT p) s

nextchr :: Parser -> Char
nextchr (Parser s) = T.head s -- errors if called when string is empty

startsWith :: Parser -> T.Text -> Bool
startsWith (Parser input) s = s `T.isPrefixOf` input

eof :: Parser -> Bool
eof (Parser input) = T.null input
```

`runParserS` extracts our results from the monad transformer stack, the other three functions are just the same as the equivalent Rust versions, except that we're always looking at the head of our `Text`.

The worker functions

```haskell
consumeChar :: ParserS Char
consumeChar = do
    (Parser inp) <- get
    case T.uncons inp of
      Nothing -> throwError "ERROR: unexpectedly reached end of file"
      Just (c,inp') -> do
        put (Parser inp')
        return c

consumeWhile :: (Char -> Bool) -> ParserS T.Text
consumeWhile f = do
    Parser input <- get
    let (s,input') = T.span f input
    put $ Parser input'
    return s

consumeWhitespace :: ParserS T.Text
consumeWhitespace = consumeWhile (==' ')
```

`get` and `put` access the `ParserS`'s state, and `throwError` will short circuit the computation with a `Left` value. We lose a bit over Rust here in needing to explicitly handle trying to read an empty `Text` where Rust would have simply tossed up an error. Speaking of errors, Robinson doesn't really have any error handling here, but it *is* liberally sprinkled with `assert!`s which, unlike every other language I've seen that has them, also run in non-debug code (apparently this is being changed soon). It's convenient to define our own equivalent, which we'll call `assert` as well.

```haskell
assert :: T.Text -> Bool -> ParserS ()
assert s b = if b then return () else throwError s
```

Now we can nicely write checks on one line.

We gain a lot of brevity for short functions:

```haskell
parseTagName :: ParserS T.Text
parseTagName = consumeWhile isAlphaNum


parseNode :: ParserS Node
parseNode = do
    p <- get
    if nextchr p == '<' then parseElement else parseText

parseText :: ParserS Node
parseText = liftM Dom.text $ consumeWhile (/='<')
```

Longer functions look fairly similar to their Rust counterparts, albeit a little easier on the eyes.
I'm not totally happy with how the asserts look, but it's not incredibly hard to follow the flow of the function.

```haskell
parseElement :: ParserS Node
parseElement = do
    -- open tag
    consumeChar >>= assert "missing < in open tag" . (=='<')
    tag <- parseTagName
    attrs <- parseAttributes
    consumeChar >>= assert "missing > in open tag" . (=='>')
    -- contents
    children <- parseNodes
    --end tag
    consumeChar  >>= assert "missing < in close tag" . (=='<')
    consumeChar  >>= assert "missing / in close tag" . (=='/')
    parseTagName >>= assert "end tag doesn't match start tag" . (==tag)
    consumeChar  >>= assert "missing > in close tag" . (=='>')

    return $ Dom.elem tag attrs children


parseAttr :: ParserS (T.Text, T.Text)
parseAttr = do
    name <- parseTagName
    consumeChar >>= assert "missing =" . (=='=')
    value <- parseAttrValue
    return (name,value)

parseAttrValue :: ParserS T.Text
parseAttrValue = do
    open <- consumeChar
    assert "invalid open" (open == '\"' || open == '\'')
    val <- consumeWhile (/=open)
    consumeChar >>= assert "invalid close" . (==open)
    return val

parseAttributes :: ParserS AttrMap
parseAttributes = parseAttributes' HM.empty
  where
    parseAttributes' attrs = do
        consumeWhitespace
        p <- get
        if nextchr p == '>' then return attrs
        else do
            (name,val) <- parseAttr
            parseAttributes' $ HM.insert name val attrs


parseNodes :: ParserS [Node]
parseNodes = parseNodes' []
  where
    parseNodes' nodes = do
        consumeWhitespace
        p <- get
        if eof p || p `startsWith` "</"
        then return nodes
        else parseNode >>= parseNodes' . (nodes++) . (:[])  --slow for big DOM
```

Finally, we'll write the function that actually parses an HTML string.

```haskell
parseHtml :: T.Text -> Either T.Text Node
parseHtml s = case runParserS parseNodes (Parser s) of
              Left err -> Left err
              Right nodes -> Right $
                if length nodes == 1
                then head nodes
                else Dom.elem "html" HM.empty nodes
```

So, that's the whole parser in only about 135 lines of code, which isn't bad. Of course, this implementation is fragile, overly verbose, and I suspect that the wrapper types might prevent proper stream fusion on the `Text` functions (I'm not super familiar with the details of Haskell stream fusion).

Part 2: The Combinator Parser
=============================

Fortunately we have the [Parsec](http://hackage.haskell.org/package/parsec-3.1.5) library, which will allow us to write a much nicer parser in about half the space (and yes I know, brevity tends to lead to the unreadable gibberish Haskell is famous for, but truest me). Parsec is not the fastest parser library, (that would be [attoparsec](http://hackage.haskell.org/package/attoparsec), because if there is one thing all programmers love, it is puns) but it is the most robust, and it's really easy to use. Most importantly, attoparsec produces truly confusing error messages, whereas Parsec will actually allow us to add our own if we want (though today I won't).

Our imports list is slightly shorter:

```haskell
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module HTML.Parsec
    ( parseHtml
    ) where

import Control.Monad (liftM)

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import qualified Data.HashMap.Strict as HM

import Dom
```

And our top level function is all but identical:

```haskell
parseHtml :: T.Text -> Either ParseError Node
parseHtml s = case parse parseNodes "" s of
              Left err -> Left err
              Right nodes -> Right $
                if length nodes == 1
                then head nodes
                else Dom.elem "html" HM.empty nodes
```

The only difference here is our call to `parse` instead of `runParserS`. The underlying monad in Parsec is actually pretty much the same as our `ParserS` but with a bit more. It's also a monad transformer, so although we're not going to use it today, we could add even more monadic features if we needed to.

When working with Parsec it's convenient to build the parser from the top down, our first function is simple enough:

```haskell
parseNodes = manyTill parseNode eof
```

That'll just keep parsing Nodes until the parser fails or we hit the end of the file.

`parseNode` is easy too, just parse a `Text` or an `Element`:

```haskell
parseNode = parseElement <|> parseText
```

How do we parse a `Text`? Just keep taking characters until we hit a '<'

```haskell
parseText = liftM (Dom.text . T.pack) $ many (noneOf "<")
```

What about an `Element`? That's a bit longer, but still pretty readable.

```haskell
parseElement = do
    -- opening tag
    (tag, attrs) <- between (char '<') (char '>') tagData
    -- contents
    children <- parseChildren
    -- closing tag
    string $ tag ++ ">" -- "</" is consumed by parseChildren, maybe bad form?
    return $ Dom.elem (T.pack tag) attrs children


-- the try combinator won't consume input if it fails, so the next parser will get that input
-- otherwise if string "</" matched '<' but not '/' the next parser would start at '/'

parseChildren = spaces >> manyTill parseChild end
  where
    end = eof <|> (try (string "</") >> return ())

    parseChild = spacesAfter parseNode


tagData = do
    t <- tagName
    attrs <- attributes
    return (t,attrs)

tagName = many1 alphaNum

--this is safe because attribute will fail without consuming on '>''
attributes = liftM HM.fromList $ spaces >> many (spacesAfter attribute)

attribute = do
    name <- tagName
    char '='
    open <- char '\"' <|> char '\''
    value <- manyTill anyChar (try $ char open)
    return (T.pack name, T.pack value)


-- run parser p and then strip the trailing spaces, returning the result of p.
spacesAfter p = p <* spaces
```

That's the whole thing.

One thing I like about Parsec is that it's very easy to write short functions with readable names, and then compose them into larger parsers that are still really easy to read. Moving forward, I'm just going to use Parsec for any other parsing in this project, without messing around with the `ParserS` (I've left it in the repo though).

That's all for today, I was planning to write the CSS parser next time, but now I'm thinking I might take a break and set up a test framework first. We'll see where that goes.

The source for this post is [here](https://github.com/Hrothen/Hubert/tree/master/src/Html). As per usual, the source for Robinson is [here](https://github.com/mbrubeck/robinson).