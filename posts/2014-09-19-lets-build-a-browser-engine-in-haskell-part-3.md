---
title: Let's Build a Browser Engine in Haskell: part 3
description: In which we parse CSS
category: 
tags: []
---

Now that we've got some [tests set up](lets-build-a-browser-engine-in-haskell-setting-up-tests.html), we're ready to write a CSS parser. But first, I'd like to take a minute to talk about `Control.Applicative` and some of the combinators it contains. Applicative combinators are really useful for building parsers, but tend to look a lot like voodoo when presented without explanation. We've already used `Parsec`'s `<|>` combinator, which tries the parser on its left, and then the one on its right if the first fails. In writing our CSS parser, we'll also use the following operators:

* `<$>` is an operator synonym for `fmap`.
* `*>` performs the action on it's left, and then the action on its right, ignoring the result of the first action. It is equivalent to `>>` on monads.
* `<*` performs the action on its left, then the action on its right, and returns the result of the first action. It is equivalent to the monadic expression `do{ f <- foo; bar; return f}`.
* `<*>` takes a binary function inside an `Applicative` on its left, and an argument to that function, also inside an `Applicative` on the right.

We mostly use `*>` and `<*` for parsers where we need to parse something but don't care about holding on to it, like trailing whitespace, or separators. `<$>` and `<*>` are useful for collecting the results of multiple parsers, for some type `Foo` we can write `Foo <$> bar <*> baz <*> quux` instead of

```haskell
a <- bar
b <- baz
c <- quux
return $ Foo a b c
```

We could also write that as `LiftA3 Foo bar baz quux`.

Everyone has trouble with `Applicative` stuff for a while (it doesn't help that they make it *really* easy to write illegible code), so don't feel bad if you don't really get it right now.

We're now ready to start on the parser. First, the module definition and imports:

```haskell
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module CSS
    ( Stylesheet(..)
    , Rule(..)
    , Selector(..)
    , Declaration(..)
    , Value(..)
    , Unit(..)
    , parseCSS
    , selectors
    , declarations
    ) where

import Prelude hiding (id)

import Data.Word (Word(..), Word8(..))
import Data.List (sortBy)
import Data.Maybe (maybe)
import Numeric (readFloat, readHex)
import Control.Applicative ((<*), (*>), (<$>), (<*>))

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T
```

We'll create some simple types to represent our CSS:

```haskell
data Stylesheet = Stylesheet [Rule]
  deriving (Show, Eq)

data Rule = Rule [Selector] [Declaration]
  deriving (Show, Eq)

-- only handle simple selectors for now
data Selector = Simple (Maybe T.Text) (Maybe T.Text) [T.Text]
  deriving (Show, Eq)

data Declaration = Declaration T.Text Value
  deriving (Show, Eq)

data Value = Keyword T.Text
           | Color Word8 Word8 Word8 Word8
           | Length Float Unit
  deriving (Show, Eq)

data Unit = Px --only Px for now
  deriving (Show, Eq)

-- an empty selector
nilS = Simple Nothing Nothing []
```

Note that because we don't want to move too far away from how Robinson works, we're actually going to be parsing only CSS2 simple selectors instead of following the more complicated CSS3 spec, for now.

The top level parser looks remarkably familiar

```haskell
-- parse an entire CSS document into a Stylesheet
parseCSS :: T.Text -> Either ParseError Stylesheet
parseCSS css = case runParser rules nilS "" css of
    Left err -> Left err
    Right rs -> Right (Stylesheet rs)

rules = spaces >> manyTill (rule <* spaces) eof
```

A `Rule` is just a list of `Selector`s, and a list of `Declaration`s

```haskell
rule = Rule <$> selectors <*> declarations
```

Selectors are in a comma separated list

```haskell
selectors = (sortBy comp) <$> sepBy1 (selector <* spaces) comma
  where comma = char ',' <* spaces
        comp a b = spec a `compare` spec b
```

We sort the parsed selectors by their specificity

```haskell
type Specificity = (Word,Word,Word)

-- compute the specificity of a Selector
spec :: Selector -> Specificity
spec (Simple name id cls) = (maybeLen id, fromIntegral $ length cls, maybeLen name)
  where maybeLen = fromIntegral . maybe 0 T.length
```

In order to actually build the `Selector`, we'll take advantage of the fact that the `Parsec` monad transformer includes a `StateT` for user state.

```haskell
-- manyTill, but the terminal parser is optional
manyUnless p end = many ((notFollowedBy end) *> p)

-- parse a simple selector
selector = do
    putState nilS
    manyUnless (id <|> cls <|> univ <|> name) eof
    getState


-- selector id
id = do
    char '#'
    i <- identifier
    modifyState (\(Simple n _ cs) -> Simple n (Just i) cs)

-- selector class
cls = do
    char '.'
    c <- identifier
    modifyState (\(Simple n i cs) -> Simple n i (cs++[c]))

-- universal selector
univ = char '*' >> return ()

-- selector name
name = do
    n' <- validId
    n  <- identifier
    let nm = n' `T.cons` n
    modifyState (\(Simple _ i cs) -> Simple (Just nm) i cs)
```

Declarations are parsed from a semicolon separated list bracketed by curly braces.

```haskell
declarations = do
    char '{'
    spaces *> manyTill (declaration <* spaces) (char '}')


declaration = do
    n <- identifier
    spaces >> char ':' >> spaces
    v <- value
    spaces >> char ';'
    return $ Declaration n v

value = len <|> color <|> keyword

len = Length <$> float <*> unit

-- parse a floating point number
float :: Stream s m Char => ParsecT s u m Float
float = (fst . head . readFloat) <$> many (digit <|> (char '.'))

-- parse the unit type in a Value
-- currently only Px is supported
unit = do
    char 'p' <|> char 'P'
    char 'x' <|> char 'X'
    return Px


color = do
    char '#'
    cs <- count 3 (count 2 hexDigit)
    let [r,g,b] = map (fst . head . readHex) cs
    return $ Color r g b 255

keyword = Keyword <$> identifier

identifier = T.pack <$> many validId

validId = alphaNum <|> char '-' <|> char '_'
```

For some reason Parsec doesn't have built in number parsers, so we have to fall back to the read functions from `Numeric`

That's the whole CSS parser, let's add a test for it

```haskell
css = "h1, h2, h3 { margin: auto; color: #cc0000; }\n\
      \div.note { margin-bottom: 20px; padding: 10px; }\n\
      \#answer { display: none; }"

sheet = Stylesheet [ Rule [ Simple (Just "h1") Nothing []
                          , Simple (Just "h2") Nothing []
                          , Simple (Just "h3") Nothing [] ]
                          [ Declaration "margin" (Keyword "auto")
                          , Declaration "color"  (Color 204 0 0 255) ]
                   , Rule [ Simple (Just "div") Nothing ["note"] ]
                          [ Declaration "margin-bottom" (Length 20 Px)
                          , Declaration "padding" (Length 10 Px) ]
                   , Rule [ Simple Nothing (Just "answer") [] ]
                          [ Declaration "display" (Keyword "none") ] ]

testCss = parseTest "for valid css" sheet $ parseCSS css
```

And if I've copied everything here correctly, that test will pass.

That's all for CSS parsing, next time we'll work on combining the Dom and Stylesheet into a single Layout.

As usual, you can find the source for this post [here](https://github.com/Hrothen/Hubert/blob/master/src/Css.hs) and the source for Robinson [here](https://github.com/mbrubeck/robinson).