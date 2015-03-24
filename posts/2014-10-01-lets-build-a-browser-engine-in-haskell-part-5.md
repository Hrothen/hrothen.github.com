---
title: Let's Build a Browser Engine in Haskell: part 5
description: In which everything is terrible forever
category: 
aliases: ["2014/10/01/lets-build-a-browser-engine-in-haskell-part-5/"]
tags: []
---

Welcome back, today we're going to implement Boxes and also the Block Layout, because Matt's post on Boxes was mostly talking about how they're set up and I don't have to do that since he already has. To start with we're going to want to set up our imports and define some new types.

```haskell
{-#LANGUAGE BangPatterns, OverloadedStrings#-}
module Layout where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.List (foldl', groupBy)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T

import Dom
import CSS
import Style

data Dimensions = Dimensions { x       :: Float
                             , y       :: Float
                             , width   :: Float
                             , height  :: Float
                             , padding :: EdgeSize
                             , border  :: EdgeSize
                             , margin  :: EdgeSize }

data EdgeSize = EdgeSize { left   :: Float
                         , right  :: Float
                         , top    :: Float
                         , bottom :: Float }

type LayoutBox = NTree (Dimensions,BoxType)

type StyledElement = (NodeType,PropertyMap)

data BoxType = BlockNode StyledElement | InlineNode StyledElement | AnonymousBlock

emptyEdge = EdgeSize 0 0 0 0

defaultDim = Dimensions 0 0 0 0 emptyEdge emptyEdge emptyEdge
```

We'll also want to add some code to the style module:

```haskell
data Display = Inline | Block | DisplayNone
  deriving (Eq)

-- if name exists, return its specified value
value :: StyledNode -> T.Text -> Maybe Value
value (NTree node _) name = HM.lookup name (snd node)

-- look up the display value of a node
display :: StyledNode -> Display
display n = case value n "display" of
    Just (Keyword "block") -> Block
    Just (Keyword "none")  -> DisplayNone
    _ -> Inline

-- this lookup is different than the robinson one, it supports
-- an arbitrary number of possible keywords

-- return the specified value of the first property in ks to exist
-- or def if no properties match
lookup :: StyledNode -> [T.Text] -> Value -> Value
lookup s ks def = maybe def (fromJust . value s) (find (isJust . value s) ks)
```

And a single helper function to CSS

```haskell
toPx :: Value -> Float
toPx (Length len Px) = len
toPx _               = 0
```


Now we can start to work on the layout tree. Our first function traverses the style tree and builds a box for each node. Nodes with `display="none"` (and their children) are filtered from the tree, and adjacent Inline boxes that are children of a Block node are grouped together beneath Anonymous boxes. We also give every node a default initial set of dimensions.

```haskell
buildLayoutTree :: StyledNode -> Either T.Text LayoutBox
buildLayoutTree root = case display root of
    Block       -> Right $ addDim <$> blt root
    Inline      -> Right $ addDim <$> blt root
    DisplayNone -> Left "error: root node has display:none"
  where
    addDim x = (defaultDim,x)

    blt rt@(NTree nd cs) = NTree n ns
      where 
        (!n, !ns) = case display rt of
            Block  -> (BlockNode  nd, anonify ns')
            Inline -> (InlineNode nd, ns')
            -- won't ever hit DisplayNone, it's filtered out
        
        anonify = concatMap mergeInlines . groupBy (\x y -> isInline x && isInline y)
        
        mergeInlines x = if isInline $ head x then [NTree AnonymousBlock x] else x

        isInline (NTree InlineNode{} _) = True
        isInline _                      = False
        
        ns' = map blt $ filter ((/=DisplayNone) . display) cs
```

I'm a little worried about the space performance of this function, for now I've used the `BangPatterns` pragma which allows me to mark `n` and `ns` as "strict" (really just evaluated to [weak head normal form](http://www.haskell.org/haskellwiki/Weak_head_normal_form)) by prefixing them with an exclamation point. This should hopefully prevent the program from building up a huge set of thunks while traversing the tree (reasoning about strictness/laziness and space usage are areas I need to improve on).

We now add some code to set the dimensions on the layout tree, at the moment we'll only support Block nodes, so anything without `display="block"` set will give us an error (I won't be adding a test for this part yet).

```haskell
-- walk a layout tree, setting the dimensions of each node
layout :: LayoutBox -> Dimensions -> Either T.Text LayoutBox
layout l@(NTree (_,box)_) contBlock = case box of
    BlockNode  _   -> layoutBlock contBlock l
    InlineNode _   -> undefined
    AnonymousBlock -> undefined


layoutBlock dim root = calcWidth dim root >>=
                       calcPosition dim   >>=
                       layoutChildren     >>= -- you know what? this might leak
                       calcHeight 
```

`layoutBlock` starts at the root, computes the width and position of the node, then does the same for all its children. Once the whole tree has been set out, we walk back up it from the leaves calculating the height of each node. It's possible that recursing in the middle of the function could cause problems for large pages, although that is also how the Rust implementation is written. This will probably get refactored later.

Calculating the width is fairly ugly, nothing is complicated, but there are a bunch of boundary checks to make.

```haskell
calcWidth :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcWidth contBlock root@(NTree (dim,x) y) = do
    style <- getStyledElem root
    let
      auto = Keyword "auto"
      zero = Length 0 Px
      w = fromMaybe auto $ value style "width"
      vals = map (\a -> lookup style a zero) [
                    ["margin-left"       , "margin"]
                  , ["margin-right"      , "margin"]
                  , ["border-left-width" , "border-width"]
                  , ["border-right-width", "border-width"]
                  , ["padding-left"      , "padding"]
                  , ["padding-right"     , "padding"] ]
      total = sum $ map toPx (w:vals)
      underflow = width contBlock - total

      ([ml'',mr''],vals') = splitAt 2 vals
      (w',ml',mr') = checkUnderflow w $ checkAutoMargins (ml'',mr'')

      checkAutoMargins (x,y)
          | w /= auto && total > width contBlock = (check x,check y)
          | otherwise = (x,y)
        where check a = if a == auto then zero else a

      checkUnderflow w (mlf,mrt) = case (w == auto, mlf == auto, mrt == auto) of
          (False,False,False) -> (w , mlf, Length (toPx mrt + underflow) Px)
          (False,False,True)  -> (w , mlf, Length underflow Px)
          (False,True,False)  -> (w , Length underflow Px    , mrt)
          (False,True,True)   -> (w , Length (underflow/2) Px, Length (underflow/2) Px)
          (True,_,_)          ->
              let l = if mlf == auto then zero else mlf
                  r = if mrt == auto then zero else mrt
               in if underflow >= 0  then (Length underflow Px,l,r)
                                     else (zero,l,Length (toPx r + underflow) Px)

      [w'',ml,mr,blw,brw,plf,prt] = map toPx (w':ml':mr':vals')


      updateDim d = let pad = padding d
                        mar = margin d
                        bor = border d
                     in d{ width = w''
                         , padding = pad{ left = plf, right = prt}
                         , border  = bor{ left = blw, right = brw}
                         , margin  = mar{ left = ml,  right = mr} }

    return $ NTree (updateDim dim,x) y


getStyledElem :: LayoutBox -> Either T.Text StyledNode
getStyledElem (NTree (_,box) _) = case box of
    BlockNode  s   -> Right $ NTree s []
    InlineNode s   -> Right $ NTree s []
    AnonymousBlock -> Left "Error: attempted to access the nonexistant\
                           \ StyleNode of an AnonymousBlock"
```

This will probably be refactored to look a bit nicer, something like

```haskell
calcWidth :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcWidth contBlock root@(NTree (dim,x) y) = checkBounds <$> getStyledElem rt
  where
    checkBounds = updateDim . checkUnderflow . checkAutoMargins . computeVals
```

The resulting function there won't be any nicer, but at least you get a quick idea of what it's doing.

The remaining functions aren't as bad.

```haskell
calcPosition :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcPosition contBlock root@(NTree (dim,a)b) = do
    style <- getStyledElem root
    
    let
      zero = Length 0 Px

      vals = map (toPx .  (\a -> lookup style a zero)) [
                    ["margin-top"         , "margin"]
                  , ["margin-bottom"      , "margin"]
                  , ["border-top-width"   , "border-width"]
                  , ["border-bottom-width", "border-width"]
                  , ["padding-top"        , "padding"]
                  , ["padding-bottom"     , "padding"] ]

      updateDim d [mt,mb,bt,bb,pt,pb] =
          let pad = padding d
              mar = margin d
              bor = border d
              x' = x contBlock
                 + left (margin d)
                 + left (border d)
                 + left (padding d)
              y' = y contBlock + height contBlock + pt + bt + mt
           in d{ x = x'
               , y = y'
               , padding = pad{ top = pt, bottom = pb }
               , border  = bor{ top = bt, bottom = bb }
               , margin  = mar{ top = mt, bottom = mb } }

    return $ NTree (updateDim dim vals,a) b


-- recursively lay out the children of a node
layoutChildren (NTree (dim,x) cs) = do
    (dim',cs') <- foldM foo (dim,[]) cs
    return $ NTree (dim',x) cs'

    where
        foo (d,acc) c@(NTree (cdim,_) _) = do
            c' <- layout c d
            return (d{height = height d + marginBoxHeight cdim}, acc ++ [c'])


-- compute the hight of a box
calcHeight :: LayoutBox -> Either T.Text LayoutBox
calcHeight root@(NTree (d,x)y) = do
    s <- getStyledElem root
    let d' = case value s "height" of
             Just (Length h Px)  -> d{height=h}
             Nothing             -> d
    return $ NTree (d',x) y


marginBoxHeight :: Dimensions -> Float
marginBoxHeight (Dimensions _ _ _ h p b m) = sum [ h, top p, bottom p
                                                 , top b, bottom b
                                                 , top m, bottom m ]
```

Worth noting here is that the `updateDim` function in both `calcWidth` and `calcPosition` could be be rewritten using lenses. The [Lens](http://hackage.haskell.org/package/lens) library pulls in a ton of dependencies though, so for the moment I won't be using it (I will probably eventually though, so if you're following along feel free to start using them whenever); but deeply nested data structures like we're using will usually be easier to work with if you're willing to learn how to use lenses.

It's also worth pointing out that this isn't really the ideal Haskell way to construct the tree, which would generally be to build our tree of boxes and a separate tree of dimensions, then zip them together. This would indeed be simpler to write, but unfortunately the spec allows inline boxes to modify their children to avoid overflowing their own bounding box. Since we're mostly sticking to Robinson's implementation I want to wait until I see if this behavior is ignored there.

With that said, we've now caught up with Matt's blog posts and I'm not particularly interested in slowing down, so for the next post I will be going back and making previous modules more comprehensive.

As usual, you can find the source for this post [here](https://github.com/Hrothen/Hubert/blob/master/src/Layout.hs) and the source for Robinson [here](https://github.com/mbrubeck/robinson).