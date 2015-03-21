---
title: "Let's Build a Browser Engine in Haskell"
description: ""
category: 
tags: []
---

Matt Brubeck over at Mozilla has recently started a series of [blog posts](http://limpet.net/mbrubeck/2014/08/08/toy-layout-engine-1.html) on constructing a toy html rendering engine in Rust. Now, I'm not particularly interested in Rust, but I *am* interested in web browsers, so I thought it might be fun to try following along in Haskell instead (obviously, I could also use an imperative language like C++, but that would be less interesting).

Build Environment
=================

Matt's trying to stick to using only the Rust standard library, but for Haskell that would limit us unnecessarily; instead, I'll try to stick with libraries shipped with the Haskell Platform (so no lenses, unless things get really awkward). I'll use GHC 7.8, but the code will probably compile with 7.6.

We'll name the project Hubert because in Haskell we like to start or end program names with H.


Getting Started: Setting up the DOM
===================================

A browser's DOM is a tree of Nodes representing the structure of an HTML page. Robinson represents the DOM with an intrusive tree: each Node has a NodeType, and a `vec` of it's own children. In Haskell, we generally like to separate structure and data when possible, so instead we'll build a Tree and fill it with Nodes.

First the tree type:

```haskell
data NTree a = NTree a [NTree a]
  deriving (Show)
```

That was easy, next up are the Nodes. Matt uses a heavily pared down DOM, we'll only have text and element nodes, which we represent with a sum type.

```haskell
import qualified Data.Text as T
-- data specific to each node type
data NodeType = Text T.Text
              | Element ElementData
  deriving (Show)
```

Finally we'll make a nice type alias for a Node:

```haskell
type Node = NTree NodeType
```

All that's left to do is define our node types. We already have a full definition for `Text`, it just holds a `Text`, `Element` holds an `ElementData` however, which consists of a name, and a hashmap of attributes (which we'll represent with Texts), imaginatively named `AttrMap`. Efficient maps can be annoying to write in Haskell, so we'll import a HashMap from `unordered-containers`.

```haskell
import qualified Data.HashMap.Strict as HM

type AttrMap = HM.HashMap T.Text T.Text

data ElementData = ElementData T.Text AttrMap
  deriving (Show)
```

Haskell manages to use a lot less space by not bothering to name fields in our types, the trade off is that we need to write the full constructor name out in our function declarations. If that gets too annoying we can switch to record fields later.

Matt also provides constructor functions for a `Node` with each `NodeType`. We don't strictly need these, but they'll save us a lot of space so let's write them.

```haskell
text :: T.Text -> Node
text = flip NTree [] . Text

elem :: T.Text -> AttrMap -> [Node] -> Node
elem name atts cs = NTree (Element (ElementData name atts)) cs
```

We'll probably write some accessors later but this is enough to get started with. Next time, we'll write the HTML parser, and actually build a DOM using these types.

The full source for this article can be found [here](https://github.com/Hrothen/Hubert/blob/master/src/Dom.hs). The full source for Robinson can be found [here](https://github.com/mbrubeck/robinson).