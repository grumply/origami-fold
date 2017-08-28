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
  , sort
  ]

tree :: Tree Int
tree = Node 0 (go 5)
  where
    go 0 = [  ]
    go n = Prelude.replicate n (Node n (go (n - 1)))

{-# INLINE weight #-}
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

-- Ordered List as Monoid by Conor McBride:
-- https://stackoverflow.com/questions/21877572/can-you-formulate-the-bubble-sort-as-a-monoid-or-semigroup
newtype OL x = OL [x]
  deriving Show
instance NFData x => NFData (OL x) where
  rnf (OL xs) = rnf xs
instance Ord x => Monoid (OL x) where
  {-# INLINE mempty #-}
  mempty = OL []
  {-# INLINE mappend #-}
  mappend (OL xs) (OL ys) = OL (merge xs ys) where
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x : xs') ys@(y : ys')
       | x <= y = x : merge xs' ys
       | otherwise = y : merge xs ys'

isort :: Ord x => [x] -> OL x
isort = foldMap (OL . pure)

{-# INLINE foldSort #-}
foldSort f = f (\_ _ _ c -> c) (\ds c -> OL [c])

sort = do
  io $ evaluate $ force tree
  br1 <- nf "foldll" (foldSort foldll) tree
  notep br1
  br2 <- nf "foldlr" (foldSort foldlr) tree
  notep br2
  br3 <- nf "foldrl" (foldSort foldrl) tree
  notep br3
  br4 <- nf "foldrr" (foldSort foldrr) tree
  notep br4

