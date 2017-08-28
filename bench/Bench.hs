{-# LANGUAGE TypeFamilies #-}
module Main where

import Trivial

import Data.Origami
import Data.Tree

import Data.Monoid

import Control.Exception
import Control.DeepSeq

main = Trivial.run suite

suite = tests
  [ weigh
  ]

tree :: Tree Int
tree = Node 0 (go 5)
  where
    go 0 = [  ]
    go n = Prelude.replicate n (Node n (go (n - 1)))

weight :: (d ~ Double, m ~ Sum d, a ~ Int, b ~ (d,a), f ~ Tree)
       => ((m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m))
       -> f a
       -> (f b,m)
weight f = f (\ps cs t a -> (getSum (ps <> cs <> Sum 1) / getSum t,a)) (\_ _ -> Sum 1)

weigh = do
  io $ evaluate $ force tree
  br1 <- nf "foldll" (weight foldll) tree
  notep br1
  br2 <- nf "foldlr" (weight foldlr) tree
  notep br2
  br3 <- nf "foldrl" (weight foldrl) tree
  notep br3
  br4 <- nf "foldrr" (weight foldrr) tree
  notep br4
