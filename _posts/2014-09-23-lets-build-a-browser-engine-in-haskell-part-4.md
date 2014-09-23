---
layout: post
title: "Let's Build a Browser Engine in Haskell: part 4"
description: ""
category: 
tags: []
---
{% include JB/setup %}

Hey guys, since I'm trying to stick to the same content per post as Matt's blog, this post will be pretty short.

Today we're going to implement styling of the DOM, wherein we combine a Stylesheet and a DOM tree to create a new DOM, with `Rule`s attached to its nodes.

Our imports list is nice and small:

```haskell
module Style where

import Data.Maybe (mapMaybe)
import Data.List (sortBy,find)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Dom
import CSS
```

We'll some types, first a map from property names to values

```haskell
type PropertyMap = HM.HashMap T.Text Value
```

Second, the styled tree itself, consisting of a `NodeType` and a `PropertyMap` for each node.

```haskell
-- instead of building a tree with references to the DOM, we'll
-- just augment the DOM tree with PropertyMaps
type StyledNode = NTree (NodeType,PropertyMap)
```

Actually styling the tree is just a map over the elements of our DOM (Later, when we add more CSS features like inheritance, it'll become a fold instead). We're using our own custom tree type, so we'll need to add an instance of `Functor` back in `Dom.hs`. We'll also add some accessors while we're in there.

```haskell
instance Functor NTree where
    fmap f (NTree n ns) = NTree (f n) $ fmap (fmap f) ns

findAttr :: ElementData -> T.Text -> Maybe T.Text
findAttr (ElementData _ m) k = HM.lookup k m

findID :: ElementData -> Maybe T.Text
findID = flip findAttr "id"

classes :: ElementData -> HashSet T.Text
classes = maybe empty (fromList . T.split (==' ')) . flip findAttr "class"
```

And the styling function:

```haskell
-- traverse the DOM, attaching PropertyMaps to each Node to
-- create a styled tree
styleTree :: Node -> Stylesheet -> StyledNode
styleTree root stylesheet = fmap style root
  where
    style e@(Element e') = (e, specifiedValues e' stylesheet)
    style t@(Text _)     = (t, HM.empty)
```

`SpecifiedValues` finds all the properties attached to an `Element` and collects them into a `PropertyMap`

```haskell
-- Build a map of all the properties attached to an Element
specifiedValues :: ElementData -> Stylesheet -> PropertyMap
specifiedValues e s = HM.fromList $ concatMap expand rules
  where
    rules = sortBy (compare `on` fst) $ matchingRules e s
    expand (_,Rule _ ds) = map (\(Declaration n v) -> (n,v)) ds


type MatchedRule = (Specificity, Rule)

-- get all of the rules from a stylesheet that match the given element
matchingRules :: ElementData -> Stylesheet -> [MatchedRule]
matchingRules e (Stylesheet rules) = mapMaybe (matchRule e) rules

-- find the first rule that matches the given element
matchRule :: ElementData -> Rule -> Maybe MatchedRule
matchRule e r@(Rule selectors _) = do
    s <- find (matches e) selectors
    return (spec s, r)

-- check if a selector matches an element
matches :: ElementData -> Selector -> Bool
matches e sl@(Simple _ _ _) = matchSimple e sl
```

That's all pretty simple stuff, the remaining function to implement, `matchSimple` is more annoying. Here's Matt's code for this function in Robinson:

```rust
fn matches_simple_selector(elem: &ElementData, selector: &SimpleSelector) -> bool {
    // Check type selector
    if selector.tag_name.iter().any(|name| elem.tag_name != *name) {
        return false;
    }

    // Check ID selector
    if selector.id.iter().any(|id| elem.id() != Some(id)) {
        return false;
    }

    // Check class selectors
    let elem_classes = elem.classes();
    if selector.class.iter().any(|class| !elem_classes.contains(&class.as_slice())) {
        return false;
    }

    // We didn't find any non-matching selector components.
    return true;
}
```

It's non-obvious from reading this code what exactly constitutes an acceptable match, primarily because `iter().any()` silently drops out of the `Optional` type, it always returns False for `None`. It looks like `matches_simple_selector` is looking to check that everything matches, but what it's actually doing is checking that a simple selector doesn't have any fields that *don't* match, values of `None` are skipped instead of counted as match failures. The recommended way to deal with optionals in Rust is to explicitly pattern match against them, which would have made this behavior a little more obvious.

Our Haskell equivalent is *a teeny bit ugly*

```haskell
-- matchSimple returns False if any selector field that exists
-- does not match the given element
matchSimple :: ElementData -> Selector -> Bool
matchSimple e@(ElementData nm _) (Simple n i c) = 
  let x = fmap (==nm) n
      y = if i == Nothing then Nothing else Just $ i == (findID e)
      z = if not $ null c then all (flip HS.member (classes e)) c else True
  in case (x,y,z) of
      (Nothing, Nothing, b3) -> b3
      (Nothing, Just b2, b3) -> b2 && b3
      (Just b1, Nothing, b3) -> b1 && b3
      (Just b1, Just b2, b3) -> b1 && b2 && b3
```

That's not totally illegible, but it's not great. We could be more explicit by pattern matching on the function arguments like this:

```haskell
matchSimple e (Simple Nothing  Nothing  c) =  matchClasses e c
matchSimple e (Simple (Just n) Nothing  c) =  matchNames e n
                                           && matchClasses e c
matchSimple e (Simple Nothing (Just i)  c) =  matchId e i
                                           && matchClasses e c
matchSimple e (Simple (Just n) (Just i) c) =  matchNames e n
                                           && matchId e i
                                           && matchClasses e c

matchNames (ElementData nm _) n = n == nm

matchId e i = findID e == Just i

matchClasses e [] = True
matchClasses e c = all (flip HS.member (classes e)) c
```

I personally don't really feel like this version is much easier to read, but it does feel a little better.

Finally we'll add another really simple test.

```haskell

testStyle = TestCase $ assertEqual "styletree" styletree $ styleTree dom css2 

css2 = Stylesheet [ Rule [ Simple (Just "head") Nothing [] ]
                         [ Declaration "margin" (Keyword "auto")
                         , Declaration "color"  (Color 0 0 0 255) ]
                  , Rule [ Simple (Just "p") Nothing ["inner"] ]
                         [ Declaration "padding" (Length 17 Px) ] ]

styletree = NTree (Element (ElementData "html" empt),empt) [head,p1,p2]
  where
    head    = NTree (Element (ElementData "head" empt),rule1) [title]
    title   = NTree (Element (ElementData "title" empt),empt) [test']
    test'   = NTree (Text "Test",empt) []
    p1      = NTree (Element (ElementData "p" (HM.singleton "class" "inner")),rule2) [hello,span]
    hello   = NTree (Text "Hello, ",empt) []
    span    = NTree (Element (ElementData "span" (HM.singleton "id" "name")),empt) [world]
    world   = NTree (Text "world!",empt) []
    p2      = NTree (Element (ElementData "p" (HM.singleton "class" "inner")),rule2) [goodbye]
    goodbye = NTree (Text "Goodbye!\n    ",empt) []
    empt    = HM.empty
    rule1   = HM.fromList [("margin",Keyword "auto"),("color",Color 0 0 0 255)]
    rule2   = HM.singleton "padding" (Length 17 Px)
```

The covers the entire Styling module, next time we'll start building the Layout tree as we close in on actually getting something we can render.

Addendum: Linting
=================

I often forget that I have a linter installed (the Haskell Platform comes with Hlint), so I haven't been linting the code for hubert up until this point. It's generally a good idea to do so though, so let's run Hlint real quick.

```bash
hlint src --report
```

Hlint will write all its complaints to your terminal, but it also generates a file `report.html`. For me Hlint found 9 things to complain about, let's go through and fix them.

```
src\Css.hs:81:13: Warning: Redundant bracket
Found
(sortBy comp) <$> sepBy1 (selector <* spaces) comma
Why not
sortBy comp <$> sepBy1 (selector <* spaces) comma

 src\Css.hs:106:8: Warning: Use void
Found
char '*' >> return ()
Why not
Control.Monad.void (char '*')

src\Css.hs:139:44: Warning: Redundant bracket
Found
digit <|> (char '.')
Why not
digit <|> char '.'

src\Css.hs:162:26: Warning: Redundant bracket
Found
(notFollowedBy end) *> p
Why not
notFollowedBy end *> p

src\Dom.hs:36:1: Error: Eta reduce
Found
elem name atts cs = NTree (Element (ElementData name atts)) cs
Why not
elem name atts = NTree (Element (ElementData name atts))

src\Style.hs:47:24: Warning: Use section
Found
(flip HS.member (classes e))
Why not
(`HS.member` (classes e))

src\Html\Parsec.hs:30:20: Warning: Use void
Found
try (string "</") >> return ()
Why not
Control.Monad.void (try (string "</"))

src\Html\Parser.hs:27:1: Error: Eta reduce
Found
runParserS p s = evalState (runExceptT p) s
Why not
runParserS p = evalState (runExceptT p)

src\Html\Parser.hs:62:14: Error: Use unless
Found
if b then return () else throwError s
Why not
Control.Monad.unless b $ throwError s
```

I'll make all these changes except for one: I'm not going to change `(flip HS.member (classes e))` to `(`HS.member` (classes e))` because it doesn't really seem any better to me (possibly because Sublime refuses to color it). You can go ahead and make the change if you want. I'll try to remember to lint as I go from now on, so we can avoid these interludes in the future.

As usual, you can find the source for this post [here](https://github.com/Hrothen/Hubert/blob/master/src/Style.hs) and the source for Robinson [here](https://github.com/mbrubeck/robinson).