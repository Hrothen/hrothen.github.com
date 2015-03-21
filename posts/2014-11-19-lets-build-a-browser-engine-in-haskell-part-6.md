---
title: Let's Build a Browser Engine in Haskell: part 6
description: In which we finally produce some output
category: 
tags: []
---

Welcome back, today we're going to implement painting of the layout tree to an image (but not to a real window, yet). The actual painting code is fairly simple, but before that we're going to want to backtrack and modify some older code.

First we'll add a `Foldable` instance to our `NTree` in Dom.hs.

```haskell
import Data.Monoid ((<>))   -- (<>) is an infix version of mappend
import Data.Foldable

instance Foldable NTree where
    foldMap f (NTree n []) = f n
    foldMap f (NTree n ns) = f n <> foldMap (foldMap f) ns
```

This will let us easily convert the layout tree into an intermediate representation.

Next we'll refactor a couple data structures to match how Robinson is laying things out. We'll pull the first four fields of the `Dimensions` struct out into their own `Rect` struct and write a couple helpers for it. We'll also pull the color data out of the `Value` type, so that later we can add other representations if we want to. (If this next bit seems hard to follow, the [changelog](https://github.com/Hrothen/Hubert/commits/master) has a full diff)

```haskell

---------------------- Layout.hs --------------------

data Rect = Rect { x      :: Float
                 , y      :: Float
                 , width  :: Float
                 , height :: Float }

data Dimensions = Dimensions { content :: Rect
                             , padding :: EdgeSize
                             , border  :: EdgeSize
                             , margin  :: EdgeSize }

emptyRect = Rect 0 0 0 0

defaultDim = Dimensions emptyRect emptyEdge emptyEdge emptyEdge

-- Rect and Dimensions helpers

expandedBy :: EdgeSize -> Rect -> Rect
expandedBy edge rec = Rect{ x      = x rec - left edge
                          , y      = y rec - top edge
                          , width  = width rec + left edge + right edge
                          , height = height rec + top edge + bottom edge }


paddingBox :: Dimensions -> Rect
paddingBox d = expandedBy (padding d) $ content d

marginBox :: Dimensions -> Rect
marginBox d = expandedBy (margin d) $ borderBox d

borderBox :: Dimensions -> Rect
borderBox d = expandedBy (border d) $ paddingBox d


layoutTree :: StyledNode -> Dimensions ->Either T.Text LayoutBox
layoutTree root contBlock = buildLayoutTree root >>=
                            flip layout contBlock{content = (content contBlock){height=0}}


-- updateDim in calcWidth
updateDim d = let pad = padding d
                        mar = margin d
                        bor = border d
                        rec = content d
                     in d{ content = rec{ width = w'' }
                         , padding = pad{ left  = plf, right = prt }
                         , border  = bor{ left  = blw, right = brw }
                         , margin  = mar{ left  = ml,  right = mr } }


-- updateDim in calcPosition
updateDim d [mt,mb,bt,bb,pt,pb] =
          let pad  = padding d
              mar  = margin d
              bor  = border d
              brec = content contBlock
              drec = content d
              x' = x brec
                 + left (margin d)
                 + left (border d)
                 + left (padding d)
              y' = y brec + height brec + pt + bt + mt
           in d{ content = drec{ x = x', y = y' }
               , padding = pad{ top = pt, bottom = pb }
               , border  = bor{ top = bt, bottom = bb }
               , margin  = mar{ top = mt, bottom = mb } }


layoutChildren (NTree (dim,x) cs) = do
    (dim',cs') <- foldM foo (dim,[]) cs
    return $ NTree (dim',x) cs'

    where
        foo (d,acc) c@(NTree (cdim,_) _) = do
            c'@(NTree (cdim',_)_) <- layout c d
            let rec = content d
            return (d{ content =
                rec{height = height rec + marginBoxHeight cdim'}}, acc ++ [c'])


calcHeight :: LayoutBox -> Either T.Text LayoutBox
calcHeight root@(NTree (d,x)y) = do
    s <- getStyledElem root
    let d' = case value s "height" of
             Just (Length h Px)  -> d{content = (content d){height=h}}
             Nothing             -> d
    return $ NTree (d',x) y

----------------------------- Css.hs ------------------------------

data Value = Keyword T.Text
           | ColorValue Color
           | Length Float Unit
  deriving (Show, Eq)

data Color = Color Word8 Word8 Word8 Word8
  deriving (Show, Eq)

color = do
    char '#'
    cs <- count 3 (count 2 hexDigit)
    let [r,g,b] = map (fst . head . readHex) cs
    return $ ColorValue (Color r g b 255)               
```

There are a couple other places where you'll need to replace `foo rect` with `foo (content rect)` but the compiler will point them out.

Now we're ready to implement painting.

Painting will be a two-phase process: first we construct a `DisplayList` from our layout tree, and then we write all the entries in the `DisplayList` to a canvas.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Painting
    ( Canvas (..)
    , newCanvas
    , paint
    ) where

import Data.Monoid ((<>),mempty)
import Data.Word

import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Text as T

import Dom
import Layout
import Style
import CSS (Value(ColorValue), Color(..))

type DisplayList = V.Vector DisplayCommand

data DisplayCommand = SolidColor Color Rect

data Canvas = Canvas { pixels :: V.Vector Color
                     , wdth   :: Word   -- we need to abbreviate these because they conflict
                     , hght   :: Word } -- with Rect.width and Rect.height, GHC 7.10 fixes this

newCanvas :: Word -> Word -> Canvas
newCanvas w h = let white = Color 255 255 255 255 in
                Canvas (V.replicate (fromIntegral(w * h)) white) w h
```

The `DisplayList` is an intermediate representation we can use to modify the data we draw, for instance by culling boxes that are entirely outside the screen. I suspect that a `Data.Vector` will be a good choice for this structure, but I'd like to minimize the amount of code we need to change if we later want to use something like `Data.Sequence` instead, so we'll try to use functions from `Data.Foldable` where possible. Right now we're only drawing boxes, so our `DisplayCommand` is just a solid color and a rectangle.

Building a `DisplayList` is a simple fold:

```haskell
buildDisplayList :: LayoutBox -> DisplayList
buildDisplayList lbox = F.foldMap renderLayoutBox lbox
```

Rendering a layout box consists of drawing the background rectangle, and then a rect for each border. If a color isn't specified for the background or borders, we don't add a `DisplayCommand` for that part.

```haskell
renderLayoutBox :: (Dimensions,BoxType) -> DisplayList
renderLayoutBox box = renderBackgroud box <> renderBorders box


renderBackgroud :: (Dimensions,BoxType) -> DisplayList
renderBackgroud (dim,ty) = maybe mempty
    (return . flip SolidColor (borderBox dim)) (getColor ty "background")


renderBorders :: (Dimensions,BoxType) -> DisplayList
renderBorders (dim,ty) = maybe mempty renderBorders' (getColor ty "border-color")
  where
    renderBorders' color = V.fromList $ map (SolidColor color) [l, r, t, b]
    bbox = borderBox dim
    bdr  = border dim
    
    l = bbox{ width = left bdr }
    
    r = bbox{ x     = x bbox + width bbox - right bdr
            , width = right bdr }
    
    t = bbox{ height = top bdr }
    
    b = bbox{ y      = y bbox + height bbox - bottom bdr
            , height = bottom bdr }


getColor :: BoxType -> T.Text -> Maybe Color
getColor (BlockNode style) name  = getColor' style name
getColor (InlineNode style) name = getColor' style name
getColor AnonymousBlock _        = Nothing

getColor' style name = case value (NTree style []) name of
                         Just (ColorValue (Color r g b a)) -> Just (Color r g b a)
                         _                    -> Nothing
```

We're not actually implementing any functions to modify the `DisplayList` right now, so all that's left is painting. We draw each command on top of the previous one.

```haskell
paint :: LayoutBox -> Rect -> Canvas
paint root bounds = let dlist  = buildDisplayList root
                        canvas = newCanvas w h
                        w = fromInteger . floor $ width  bounds
                        h = fromInteger . floor $ height bounds
                    in F.foldl' paintItem canvas dlist


paintItem :: Canvas -> DisplayCommand -> Canvas
paintItem cs (SolidColor color rect) = updateChunk cs (x0,x1) (y0,y1) color
  where
    x0 = clampInt 0 (w-1) (x rect)
    y0 = clampInt 0 (h-1) (y rect)
    x1 = clampInt 0 (w-1) (x rect + width rect - 1)
    y1 = clampInt 0 (h-1) (y rect + height rect - 1)
    w = asFloat $ wdth cs
    h = asFloat $ hght cs
    asFloat = fromInteger . toInteger



-- this probably modifies the pixel vector in-place, if I'm reading the
-- Data.Vector source correctly
updateChunk :: Canvas -> (Integer,Integer) -> (Integer,Integer) -> Color -> Canvas
updateChunk cs (x0,x1) (y0,y1) c = let pxs = V.update (pixels cs) chunk in
                                   cs{ pixels = pxs}
  where
    chunk = V.map (\a->(fromIntegral a,c)) indicies
    indicies = V.fromList [ y * (toInteger $ wdth cs) + x | x <- [x0..x1], y <- [y0..y1] ]


clampInt :: Float -> Float -> Float -> Integer
clampInt f c = floor . min c . max f
```

Note that this code differs slightly from the equivalent Rust code in Robinson: clamping to [0,height] and [0,width] is a runtime error (you'll try to write one row/column of pixels too far, I'm not sure why Rust allows it), and straight up adding the dimensions to the coordinates will also result in boxes that are 1 pixel too large in both dimensions.

That's all our painting code, we just need to add a quick test to make sure it works. We'll need a library to load images for this, and sadly there isn't one included in the Haskell Platform, so we'll need to install it. Add the following lines to build-depends in your .cabal file:

```
vector >= 0.10.9.1,
JuicyPixels >= 3.1.7.1
```

And then in a command prompt run:

```bash
cabal update
cabal install --enable-tests --only-dependencies
```

This will install the JuicyPixels library, which has good support for dealing with images in the most common formats.

We'll add one test (you'll need to grab rainbow.png from the github repo)

```haskell
-- add these at the top of tests.hs
import Data.Maybe

import qualified Data.Vector as V

import Codec.Picture
import Codec.Picture.Types

import Painting

----------------------------- PAINT TESTS ---------------------------

testPaint = TestCase $ do 
    testpng <- readPng "tests/rainbow.png"
    either (\_->assertFailure "missing png image")
           (compareImage paintpng)
           testpng

compareImage (Left e) _ = assertFailure $ T.unpack e
compareImage (Right i1) (ImageRGB8 i2) = do
  assertEqual "height" (imageHeight i1) (imageHeight i2)
  assertEqual "width"  (imageWidth i1)  (imageWidth i2)
  assertEqual "pixels" (imageData i1)   (imageData i2)


contBlock = defaultDim{content = (content defaultDim){ width=800, height=168 } }

paintpng = paintpng' s d
  where
    (Right d) = PS.parseHtml pnghtml
    (Right s) = parseCSS pngcss
    paintpng' s d = do
      let st = styleTree d s
      lyt <- layoutTree st contBlock
      let vec = pixels $ paint lyt (content contBlock)
      return $ generateImage (\x y-> c2px $ vec V.! (x + (y * 800))) 800 168
    c2px (Color r g b _) = PixelRGB8 r g b

pnghtml = "<div class=\"a\">\
\  <div class=\"b\">\
\    <div class=\"c\">\
\      <div class=\"d\">\
\        <div class=\"e\">\
\          <div class=\"f\">\
\            <div class=\"g\">\
\            </div>\
\          </div>\
\        </div>\
\      </div>\
\    </div>\
\  </div>\
\</div>"

pngcss = "* { display: block; padding: 12px; }\
\.a { background: #ff0000; }\
\.b { background: #ffa500; }\
\.c { background: #ffff00; }\
\.d { background: #008000; }\
\.e { background: #0000ff; }\
\.f { background: #4b0082; }\
\.g { background: #800080; }"
```

Don't forget to add `testPaint` to the test list.

That's everything for painting. I might put up another post covering some refactorings and additions to the current code before the next Robinson update comes out, or I might work on something completely different for a bit. We'll see.

As usual, you can find the source for this post [here](https://github.com/Hrothen/Hubert/blob/master/src/Painting.hs) and the source for Robinson [here](https://github.com/mbrubeck/robinson).