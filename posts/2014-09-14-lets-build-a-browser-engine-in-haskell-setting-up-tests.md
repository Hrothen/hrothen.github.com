---
title: Let's Build a Browser Engine in Haskell: setting up tests
description: In which we remember that tests are a thing we should have
category: 
tags: []
---

Welcome back. In [part 2](lets-build-a-browser-engine-in-haskell-part-2.html) we wrote an html parser two different ways, and decided to implement some tests before going any further. Let's do that now.

Setting up some simple tests
============================

We'll create a file `tests.hs` in a new folder `/tests`, to keep our test code separate. We'll be using the [HUnit](http://hackage.haskell.org/package/HUnit) test framework, which is included in the Haskell Platform. As usual, we'll start off with the imports:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (elem)
import Data.Either (either)

import Test.HUnit

import Text.Parsec hiding (parseTest)
import Text.Parsec.Text


import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import qualified HTML.Parser as PR
import qualified HTML.Parsec as PS
import Dom
```

We'll be running the same tests for both the `ParserS` and `Parsec` html parsers, but after this we're pretty much going to ignore the `ParserS` one, the `Parsec` parser will be much easier to extend if we want to add features.

We'll start with some simple data to test against:

```haskell
testText = text "candygram"
testElem = elem "p" (HM.singleton "ham" "doctor") [text "sup"]

-- a small test html page
-- yeah, multi-line strings in haskell kind of suck
html = "<html>\n\
       \    <head>\n\
       \        <title>Test</title>\n\
       \    </head>\n\
       \    <p class=\"inner\">\n\
       \        Hello, <span id=\"name\">world!</span>\n\
       \    </p>\n\
       \    <p class=\"inner\">\n\
       \        Goodbye!\n\
       \    </p>\n\
       \</html>"

-- the expected result of parsing the test page
dom = elem "html" HM.empty [head,p1,p2]
  where
    head  = elem "head"  HM.empty [title]
    title = elem "title" HM.empty [text "Test"]
    p1    = elem "p"    (HM.singleton "class" "inner") [hello, span]
    hello = text "Hello, "
    span  = elem "span" (HM.singleton "id" "name") [text "world"]
    p2    = elem "p"    (HM.singleton "class" "inner") [text "Goodbye!"]
``` 

For each test, we want to check both that the parser succeeded, and that it returned the correct value, so we'll define a little helper function for that:

```haskell
-- generic test: given an expected value and an actual value, check that the actual
-- value is not an error message, then compare it to the expected value
parseTest msg e = TestCase . either (assertFailure . show) (assertEqual msg e)
```

The first argument to `parseTest` is a message that will be associated with that test by the runner, if we don't get a successful parse the runner will instead show the parser's error message.

The tests for `ParserS` and `Parsec` are identical aside from the function used to run the parser, and needing to pack the strings for `Parsec`:

```haskell
--------------------------- PARSER_S TESTS ------------------------------

parsePR p i = PR.runParserS p (PR.Parser i)

htmlPR = parseTest "for valid html" dom $ PR.parseHtml html


textPR = parseTest "for valid text" testText $ parsePR PR.parseText "candygram"


elemPR = parseTest "for valid elem" testElem $
                   parsePR PR.parseElement "<p ham=\"doctor\">sup</p>"


---------------------------- PARSEC TESTS ------------------------------


htmlPS = parseTest "for valid html" dom $ PS.parseHtml html


textPS = parseTest "for valid text" testText $
                    parse PS.parseText "" $ T.pack "candygram"


elemPS = parseTest "for valid elem" testElem $
                    parse PS.parseElement "" $ T.pack "<p ham=\"doctor\">sup</p>"
```

Finally we'll group the tests up, and run them:

```haskell
main = runTestTT tests

tests = TestList [TestLabel "ParserS html" htmlPR,
                  TestLabel "ParserS text" textPR,
                  TestLabel "ParserS elem" elemPR,
                  TestLabel "Parsec html"  htmlPS,
                  TestLabel "Parsec text"  textPS,
                  TestLabel "Parsec elem"  elemPS]
```

You can run these tests in GHCi with the command `runTestTT tests` (or just compile tests.hs outright) but if you're being responsible and using a .cabal file + sandbox like I am, you might want to add a test suite to the .cabal file.

```
Test-Suite hunit-tests
  type:             exitcode-stdio-1.0
  main-is:          tests.hs
  other-modules:    Dom,
                    HTML.Parser,
                    HTML.Parsec
  build-depends:    base >= 4.7 && < 5,
                    unordered-containers >=0.2 && <0.3,
                    mtl >= 2.2.1,
                    text >= 1.1.0.0,
                    HUnit >= 1.2.5.0,
                    parsec == 3.1.*
  hs-source-dirs:   tests,
                    src
  default-language: Haskell2010
```

You can now compile your tests with

```
cabal configure --enable-tests
cabal build
cabal test
```

However, HUnit does not actually conform to the format expected by `exitcode-stdio-1.0` (which is amusing, since it's supposed to be a backwards compatibility setting) so `cabal test` will *always* claim to have run all tests successfully. The real results will be printed to the logfile located in dist/test.

```
Test suite hunit-tests: RUNNING...

Cases: 6  Tried: 0  Errors: 0  Failures: 0
                                          
### Failure in: 0:ParserS html

for valid html

expected: NTree (Element (ElementData "html" fromList [])) [NTree (Element (ElementData "head" fromList [])) [NTree (Element (ElementData "title" fromList [])) [NTree (Text "Test") []]],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "Hello, ") [],NTree (Element (ElementData "span" fromList [("id","name")])) [NTree (Text "world") []]],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "Goodbye!") []]]

 but got: NTree (Element (ElementData "html" fromList [])) [NTree (Text "\n    ") [],NTree (Element (ElementData "head" fromList [])) [NTree (Text "\n        ") [],NTree (Element (ElementData "title" fromList [])) [NTree (Text "Test") []],NTree (Text "\n    ") []],NTree (Text "\n    ") [],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "\n        Hello, ") [],NTree (Element (ElementData "span" fromList [("id","name")])) [NTree (Text "world!") []],NTree (Text "\n    ") []],NTree (Text "\n    ") [],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "\n        Goodbye!\n    ") []],NTree (Text "\n") []]


Cases: 6  Tried: 1  Errors: 0  Failures: 1
Cases: 6  Tried: 2  Errors: 0  Failures: 1
Cases: 6  Tried: 3  Errors: 0  Failures: 1
                                          
### Failure in: 3:Parsec html

for valid html

expected: NTree (Element (ElementData "html" fromList [])) [NTree (Element (ElementData "head" fromList [])) [NTree (Element (ElementData "title" fromList [])) [NTree (Text "Test") []]],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "Hello, ") [],NTree (Element (ElementData "span" fromList [("id","name")])) [NTree (Text "world") []]],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "Goodbye!") []]]

 but got: NTree (Element (ElementData "html" fromList [])) [NTree (Element (ElementData "head" fromList [])) [NTree (Element (ElementData "title" fromList [])) [NTree (Text "Test") []]],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "Hello, ") [],NTree (Element (ElementData "span" fromList [("id","name")])) [NTree (Text "world!") []]],NTree (Element (ElementData "p" fromList [("class","inner")])) [NTree (Text "Goodbye!\n    ") []]]


Cases: 6  Tried: 4  Errors: 0  Failures: 2
Cases: 6  Tried: 5  Errors: 0  Failures: 2
                                          
Cases: 6  Tried: 6  Errors: 0  Failures: 2

Test suite hunit-tests: PASS
Test suite logged to: dist\test\hubert-0.1.0.0-hunit-tests.log
```

Oh hey, looks like our tests have found a problem: `parseHtml` has failed for both parsers. Since our other tests have succeeded, we can guess that the issue is at the top level of the parser, and inspecting the output, we can see that indeed, the `ParserS` is parsing whitespace between nodes as text nodes. That should be easy to fix. Since `consumeWhitespace = consumeWhile (==' ')` is only consuming spaces, we'll rewrite it as:

```haskell
consumeWhitespace :: ParserS T.Text
consumeWhitespace = consumeWhile isSpace
```

While the `Parsec` parser is not having this problem, it actually will exhibit the same behavior for improper html files where there is no root node; because for reasons I can't remember, `parseNodes` and `parseChildren` do different things. `parseChildren` is the proper implementation though, so we'll just rename it to `parseNodes` and change every call to `parseChildren` to call `parseNodes`.

We have a second issue, which is affecting both parsers: the parser is returning a text element of `"Goodbye!\n    "` instead of the expected `"Goodbye!"`. This is actually a mistake writing the test, the correct behavior is for the html parser to include the whitespace in the text element, and later algorithms can ignore it or not as they wish. I've also accidentally left off the '!' at the end of `"world"`.

With those changes, all tests now pass:

```
Test suite hunit-tests: RUNNING...

Cases: 6  Tried: 0  Errors: 0  Failures: 0
Cases: 6  Tried: 1  Errors: 0  Failures: 0
Cases: 6  Tried: 2  Errors: 0  Failures: 0
Cases: 6  Tried: 3  Errors: 0  Failures: 0
Cases: 6  Tried: 4  Errors: 0  Failures: 0
Cases: 6  Tried: 5  Errors: 0  Failures: 0
                                          
Cases: 6  Tried: 6  Errors: 0  Failures: 0

Test suite hunit-tests: PASS
Test suite logged to: dist\test\hubert-0.1.0.0-hunit-tests.log
```

With that, we can move on to parsing CSS. I had originally planned to cover CSS in this post, but I'd like to avoid huge posts like the previous one so we'll end this here. Expect the next post fairly soon.

As usual, you can find the source for this post [here](https://github.com/Hrothen/Hubert/blob/master/tests/tests.hs) and the source for Robinson [here](https://github.com/mbrubeck/robinson).