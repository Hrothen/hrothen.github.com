---
layout: post
title: "Let's Write a Browser Engine in Haskell: a lens interlude"
description: ""
category: 
tags: []
---
{% include JB/setup %}

This post doesn't cover any new functionality, instead, we'll be revisiting some old code and refactoring it to be less awful. In particular, we'll be using [lenses](http://ekmett.github.io/lens/index.html) make our nested datatypes less annoying to work with. I've tried to record every change I made, but it's possible I missed something, if you have trouble compiling, look at the github changelog to try to find the issue, and also let me know so I can update this article.

## A couple notes on lenses

First: I am not going to attempt to explain lenses in any detail. [This tutorial](http://unbui.lt/#!/post/haskell-another-lens-tutorial) is a decent introduction to what lenses are, but for our purposes it's enough to know that lenses look and act a lot like accessors in other languages. Lenses compose "backwards" so for example `(content . height)` will access the height element of the content element of a `Dimensions`.

Second: Lenses are a pain to read, partly because they compose backwards but also because the `lens` library includes operators for every situation Edward Kmett could think of (over 100 operators, including such gems as the squid operator `(<<<>~)` and the cartoon invective operator `(^@!?)`). We'll try to avoid readability issues by using a really minimal set of lens functions and operators, namely: `(^.)` which gives the value of the field a lens accesses, `(.~)` which assigns a value to the field a lens accesses, `(.=)` which is `(.~)` but for the state inside a `State` monad, `(&)` which is just `flip $` and is used to chain lens operations, `(&~)` which is `(&)` inside a `State` monad, and `zoom` which "zooms in" on a lens, so if we're going to perform several operations on the same field, we don't need to keep writing it.

## Adding Lenses to Types

The `lens` library conveniently provides a template haskell macro to create lenses for a structure, all we need to do is preface all fields we want to derive a lens for with `_`, and then call `makeLenses ''<type name>`. The ability to automatically derive lenses is the main reason we're using the `lens` library instead of just writing our own simple lenses, if you don't want to include all the dependencies `lens` has, you can also use `lens-family` which has all the operators we'll be using.

For starters, we'll derive lenses for `NTree` in `Dom.hs`

```haskell
-- I'm only showing lines that have changed

-- It's not actually mentioned on the page, probably because they thought it
-- was obvious, but you need the TemplateHaskell pragma to use makeLenses
{-# LANGUAGE OverloadedStrings, FlexibleInstances, TemplateHaskell#-}

import Control.Lens

data NTree a = NTree { _root :: a, _children :: [NTree a] }

makeLenses ''NTree
```

Most of our work is going to be in `Layout.hs` which will look a lot nice by the time we finish.

```haskell
{-#LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell#-}

import Data.Function (on)

import Control.Lens hiding (children)

data Rect = Rect { _x      :: Float
                 , _y      :: Float
                 , _width  :: Float
                 , _height :: Float }

data Dimensions = Dimensions { _content :: Rect
                             , _padding :: EdgeSize
                             , _border  :: EdgeSize
                             , _margin  :: EdgeSize }

data EdgeSize = EdgeSize { _left   :: Float
                         , _right  :: Float
                         , _top    :: Float
                         , _bottom :: Float }

makeLenses ''Rect
makeLenses ''Dimensions
makeLenses ''EdgeSize


-- here we use (.~) to pass contBlock with its height value set to 0
layoutTree :: StyledNode -> Dimensions -> Either T.Text LayoutBox
layoutTree root contBlock = buildLayoutTree root >>=
                            flip layout (contBlock & content.height.~0)


-- only one line has changed in buildLayoutTree
buildLayoutTree :: StyledNode -> Either T.Text LayoutBox
...
anonify = concatMap mergeInlines . groupBy ((&&) `on` isInline)
...


-- here we use a lens to get rid of the ugly nested pattern match
layout :: LayoutBox -> Dimensions -> Either T.Text LayoutBox
layout l contBlock = case l^.root._2 of
    BlockNode  _   -> layoutBlock contBlock l
    InlineNode _   -> undefined
    AnonymousBlock -> undefined


-- the next few function have changed significantly, and several parts
-- have been pulled out into their own things

auto = Keyword "auto"
zero = Length 0 Px


calcWidth :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcWidth contBlock rt = do
    style <- getStyledElem rt
    vals  <- lookupSideVals rt
    let w               = fromMaybe auto $ value style "width"
        total           = sum $ map toPx (w:vals)
        underflow       = contBlock^.content.width - total
        (margins,vals') = splitAt 2 vals
        
        (w',ml',mr') = checkUnderflow w underflow $ checkAutoMargins margins w total

        [w'',ml,mr,blw,brw,plf,prt] = map toPx (w':ml':mr':vals')

-- did you know you can use semicolons in haskell?
    return $ rt &~ zoom (root . _1) (do
        content.width .= w''
        padding.left  .= plf; padding.right .= prt
        border.left   .= blw; border.right  .= brw
        margin.left   .= ml ; margin.right  .= mr)

  where
    checkAutoMargins [x,y] w total
          | w /= auto && total > contBlock^.content.width = (check x,check y)
          | otherwise = (x,y)
        where check a = if a == auto then zero else a

    checkUnderflow w uflow (mlf,mrt) = case (w == auto, mlf == auto, mrt == auto) of
        (False,False,False) -> (w , mlf, Length (toPx mrt + uflow) Px)
        (False,False,True)  -> (w , mlf, Length uflow Px)
        (False,True,False)  -> (w , Length uflow Px    , mrt)
        (False,True,True)   -> (w , Length (uflow/2) Px, Length (uflow/2) Px)
        (True,_,_)          ->
            let l = if mlf == auto then zero else mlf
                r = if mrt == auto then zero else mrt
             in if uflow >= 0  
                then (Length uflow Px,l,r)
                else (zero,l,Length (toPx r + uflow) Px)


-- I pulled out the lookup functions since they took up a lot of space,
-- and gave them nice descriptive names

lookupSideVals :: LayoutBox -> Either T.Text [Value]
lookupSideVals rt = do
    style <- getStyledElem rt
    return $ map (\a -> lookup style a zero)
        [ ["margin-left"       , "margin"]
        , ["margin-right"      , "margin"]
        , ["border-left-width" , "border-width"]
        , ["border-right-width", "border-width"]
        , ["padding-left"      , "padding"]
        , ["padding-right"     , "padding"] ]

lookupVertVals :: LayoutBox -> Either T.Text [Float]
lookupVertVals rt = do
    style <- getStyledElem rt
    return $ map (toPx . (\a -> lookup style a zero))
        [ ["margin-top"         , "margin"]
        , ["margin-bottom"      , "margin"]
        , ["border-top-width"   , "border-width"]
        , ["border-bottom-width", "border-width"]
        , ["padding-top"        , "padding"]
        , ["padding-bottom"     , "padding"] ]


calcPosition :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcPosition contBlock rt = do
    [mt,mb,bt,bb,pt,pb] <- lookupVertVals rt
    let d = rt^.root._1
    return $ rt &~ zoom (root . _1) (do
        content.x.= contBlock^.content.x
                  + d^.margin.left
                  + d^.border.left
                  + d^.padding.left
        content.y.= contBlock^.content.y
                  + contBlock^.content.height
                  + pt + bt + mt
        padding.top .= pt; padding.bottom .= pb
        border.top  .= bt; border.bottom  .= bb
        margin.top  .= mt; margin.bottom  .= mb)


layoutChildren :: LayoutBox -> Either T.Text LayoutBox
layoutChildren rt = do
    (dim,cs) <- foldM foo (rt^.root._1,[]) $ rt^.children
    return $ rt &~ root._1.= dim &~ children.= cs
  where
    foo :: (Dimensions,[LayoutBox]) -> LayoutBox -> Either T.Text (Dimensions,[LayoutBox])
    foo (d,acc) c = do
        c' <- layout c d
        return (d & content.height+~ marginBoxHeight (c'^.root._1), acc ++ [c'])

calcHeight :: LayoutBox -> Either T.Text LayoutBox
calcHeight rt = do
    s <- getStyledElem rt
    case value s "height" of
        Just (Length h Px)  -> return $ rt & root._1.content.height.~ h
        Nothing             -> return rt


marginBoxHeight :: Dimensions -> Float
marginBoxHeight dim = (marginBox dim)^.height

getStyledElem :: LayoutBox -> Either T.Text StyledNode
getStyledElem rt = case rt^.root._2 of
    BlockNode  s   -> Right $ NTree s []
    InlineNode s   -> Right $ NTree s []
    AnonymousBlock -> Left "Error: attempted to access the nonexistant\
                           \ StyleNode of an AnonymousBlock"


expandedBy :: Rect -> EdgeSize -> Rect
expandedBy rec edge = rec &~ do
                          x -= edge^.left
                          y -= edge^.top
                          width  += (edge^.left + edge^.right)
                          height += (edge^.top + edge^.bottom)


paddingBox :: Dimensions -> Rect
paddingBox d = (d^.content) `expandedBy` (d^.padding)

marginBox :: Dimensions -> Rect
marginBox d = borderBox d `expandedBy` (d^.margin)

borderBox :: Dimensions -> Rect
borderBox d = paddingBox d `expandedBy` (d^.margin)
```

Now that we're using lenses throughout our layout tree, we'll need to update `Painting.hs` and `tests.hs` as well.

```haskell
-- Painting.hs

import Control.Lens

paint :: LayoutBox -> Rect -> Canvas
paint root bounds = let dlist  = buildDisplayList root
                        canvas = newCanvas w h
                        w = fromInteger . floor $ bounds^.width
                        h = fromInteger . floor $ bounds^.height
                    in F.foldl' paintItem canvas dlist


buildDisplayList :: LayoutBox -> DisplayList
buildDisplayList = F.foldMap renderLayoutBox


renderBorders :: (Dimensions,BoxType) -> DisplayList
renderBorders (dim,ty) = maybe mempty renderBorders' (getColor ty "border-color")
  where
    renderBorders' color = V.fromList $ map (SolidColor color) [l, r, t, b]
    bbox = borderBox dim
    bdr  = dim^.border
    
    l = bbox & width.~ bdr^.left
    
    r = bbox & x+~ bbox^.width - bdr^.right
             & width.~ bdr^.right
    
    t = bbox & height.~ bdr^.top
    
    b = bbox & y+~ bbox^.height - bdr^.bottom
             & height.~ bdr^.bottom


paintItem :: Canvas -> DisplayCommand -> Canvas
paintItem cs (SolidColor color rect) = updateChunk cs (x0,x1) (y0,y1) color
  where
    x0 = clampInt 0 (w-1) (rect^.x)
    y0 = clampInt 0 (h-1) (rect^.y)
    x1 = clampInt 0 (w-1) (rect^.x + rect^.width - 1)
    y1 = clampInt 0 (h-1) (rect^.y + rect^.height - 1)
    w = asFloat $ wdth cs
    h = asFloat $ hght cs
    asFloat = fromInteger . toInteger


-- tests.hs
import Control.Lens

contBlock = defaultDim & content.width.~800 & content.height.~168

paintpng = paintpng' s d
  where
    (Right d) = PS.parseHtml pnghtml
    (Right s) = parseCSS pngcss
    paintpng' s d = do
      let st = styleTree d s
      lyt <- layoutTree st contBlock
      let vec = pixels $ paint lyt (contBlock^.content)
      return $ generateImage (\x y-> c2px $ vec V.! (x + (y * 800))) 800 168
    c2px (Color r g b _) = PixelRGB8 r g b
```

Finally, update `hubert.cabal` to add our lens dependency

```
executable hubert
  main-is:             Main.hs
  other-modules:       Dom,
                       HTML.Parser,
                       HTML.Parsec,
                       CSS,
                       Style,
                       Layout

  -- other-extensions:    
  build-depends:       base >=4.7 && <5,
                       unordered-containers >=0.2 && <0.3,
                       mtl >= 2.2.1,
                       text >= 1.1.0.0,
                       parsec == 3.1.*,
                       lens >= 4.6


Test-Suite hunit-tests
  type:             exitcode-stdio-1.0
  main-is:          tests.hs
  other-modules:    Dom,
                    HTML.Parser,
                    HTML.Parsec,
                    CSS,
                    Style,
                    Layout,
                    Painting
  build-depends:    base >= 4.7 && < 5,
                    unordered-containers >=0.2 && <0.3,
                    mtl >= 2.2.1,
                    text >= 1.1.0.0,
                    HUnit >= 1.2.5.0,
                    parsec == 3.1.*,
                    vector >= 0.10.9.1,
                    JuicyPixels >= 3.1.7.1,
                    lens >= 4.6
  hs-source-dirs:   tests,
                    src
  default-language: Haskell2010
```

At this point, everything should compile and run correctly. We've used lenses to make the deeply nested updates in `Layout.hs` somewhat less awful to read, and we should be set until Robinson updates again.

As usual, you can find the source for this post [here](https://github.com/Hrothen/Hubert/tree/master/src) and the source for Robinson [here](https://github.com/mbrubeck/robinson).