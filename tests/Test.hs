module Main where

import Data.Origami

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Debug.Trace
import Data.Monoid

import qualified Data.Sequence as Seq
import Data.Tree

main = void $ evaluate $ force $ do
  b <- [True,False]
  t <- [testList,testTree,testSeq]
  return (t b)

testList b =
  let as = [1..5]
      i = Sum (1 :: Int)
      l _ ds _ c = traceShow c (ds <> i)
      r _ c = traceShow c i
      (xs1,x1) = foldll l r as
      b1 = trace "\nll\n" 0
      (xs2,x2) = foldlr l r as
      b2 = trace "\nlr\n" 0
      (xs3,x3) = foldrl l r as
      b3 = trace "\nrl\n" 0
      (xs4,x4) = foldrr l r as
      b4 = trace "\nrr\n" 0
  in if b
       then b1 `seq` x1 `seq` b2 `seq` x2 `seq` b3 `seq` x3 `seq` b4 `seq` x4 `seq` x1 + x2 + x3 + x4
       else b1 `seq` xs1 `deepseq` b2 `seq` xs2 `deepseq` b3 `seq` xs3 `deepseq` b4 `seq` xs4 `deepseq` 0

testSeq b =
  let as = Seq.fromList [1..5]
      i = Sum (1 :: Int)
      l _ ds _ c = traceShow c (ds <> i)
      r _ c = traceShow c i
      (xs1,x1) = foldll l r as
      b1 = trace "\nll\n" 0
      (xs2,x2) = foldlr l r as
      b2 = trace "\nlr\n" 0
      (xs3,x3) = foldrl l r as
      b3 = trace "\nrl\n" 0
      (xs4,x4) = foldrr l r as
      b4 = trace "\nrr\n" 0
  in if b
       then b1 `seq` x1 `seq` b2 `seq` x2 `seq` b3 `seq` x3 `seq` b4 `seq` x4 `seq` x1 + x2 + x3 + x4
       else b1 `seq` xs1 `deepseq` b2 `seq` xs2 `deepseq` b3 `seq` xs3 `deepseq` b4 `seq` xs4 `deepseq` 0

testTree b = do
  let tree = Node 1 [ Node 2 [ Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
      t = trace (drawTree $ fmap show tree) 0
      i = Sum (1 :: Int)
      l as ds t c = traceShow c $ return (ds <> i)
      r _ c = traceShow c (return i)
      b1 = trace "\nll\n" 0
      b2 = trace "\nlr\n" 0
      b3 = trace "\nrl\n" 0
      b4 = trace "\nrr\n" 0
  (xs1,x1) <- foldllM l r tree

  (xs2,x2) <- foldlrM l r tree
  (xs3,x3) <- foldrlM l r tree
  (xs4,x4) <- foldrrM l r tree
  if b
    then t `seq` b1 `seq` x1 `seq` b2 `seq` x2 `seq` b3 `seq` x3 `seq` b4 `seq` x4 `seq` x1 + x2 + x3 + x4
    else t `seq` b1 `seq` xs1 `deepseq` b2 `seq` xs2 `deepseq` b3 `seq` xs3 `deepseq` b4 `seq` xs4 `deepseq` 0

