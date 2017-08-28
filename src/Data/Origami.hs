{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}
{-# language RecursiveDo #-}
module Data.Origami where

import Control.Monad.Fix

import Data.Bifunctor
import Data.Monoid

import Data.Tree
import Data.Sequence hiding (fromList,toList)

import GHC.Exts

import Debug.Trace

import Control.DeepSeq

-- | A generalized middle-out origami-like lazy monoidal mapping fold.
--
-- To label each node in a tree with the number of nodes traversed to arrive
-- at the current node plus the node's count of children plus one for the
-- current node divided by the total number of nodes in the tree; a relative
-- node weight:
-- > foldsl (\ps cs t a -> (getSum (ps <> cs <> Sum 1) / getSum t,a)) (\_ _ -> Sum 1)
--
-- Given the tree:
-- 1
-- |
-- +- 2
-- |  |
-- |  `- 3
-- |
-- +- 4
-- |
-- `- 5
--    |
--    `- 6
--       |
--       `- 7
--          |
--          +- 8
--          |
--          +- 9
--          |
--          `- 10
--
-- The above origami fold would produce:
-- (1.0,1)
-- |
-- +- (0.3,2)
-- |  |
-- |  `- (0.3,3)
-- |
-- +- (0.2,4)
-- |
-- `- (0.7,5)
--    |
--    `- (0.7,6)
--       |
--       `- (0.7,7)
--          |
--          +- (0.5,8)
--          |
--          +- (0.5,9)
--          |
--          `- (0.5,10)
--
-- > foldsl (\before after total a -> total) (\_ a -> a) [undefined,Any False,Any True]
-- Exception: Prelude.undefined
--
-- > foldsr (\before after total a -> total) (\_ a -> a) [undefined,Any False,Any True]
-- ([Any True,Any True,Any True],Any True)
--
-- > foldel (\before after total a -> total) (\_ a -> a) [Any False,Any True,undefined]
-- ([Any True,Any True,Any True],Any True)
--
-- > folder (\before after total a -> total) (\_ a -> a) [Any False,Any True,undefined]
-- Exception: Prelude.undefined
class Origami f where
  -- | Dragons!
  --
  -- This method is oddly magical. It represents simultaneous beginning-to-end
  -- and end-to-beginning folding and can modify any element in the structure
  -- as if it has already done both. It does, of course, have limitations.
  --
  -- Given: foldo c f xs
  --        c :: m -> m -> m -- mappend or flip mappend, generally
  --        f :: m -> m -> m -> a -> (b,m)
  --
  -- There are two rules:
  --   1. `f` may choose to use one of the first two 'm's to produce the result
  --      'm', but must not switch during evaluation. This is equivalent to
  --      deciding to evaluate top-down or bottom-up, left-to-right or
  --      right-to-left.
  --   2. You may not use the last 'm' to produce the result 'm'.
  --
  -- Conveniently, you may use any or all of the three 'm's to produce the 'b'.
  -- If you do so, however, be aware that using the 'b' to produce the result
  -- 'm' will <<loop>>.
  --
  -- This method need only traverse the structure once and should be, in general,
  -- linear.
  --
  -- The first 'm' represents the `mconcat` of the previous monoidal fold results.
  -- The second 'm' represents the `mconcat` of the future monoidal fold results.
  -- The third 'm' represents the final monoidal fold result for the entire structure.
  --
  -- For safety, use the derived methods:
  --   pure: foldsl, foldsr, foldel, folder
  --   monadic: foldslM, foldsrM, foldelM, folderM
  foldo :: Monoid m => m -> (m -> m -> m) -> (m -> m -> m -> a -> (b,m)) -> f a -> (f b,m)

  -- | Dragons!
  --
  -- foldoM is the generic interface for origami folds, monadically; same dragons as
  -- found in foldo plus a few extras. Note that traversal order often depends upon
  -- the monad chosen. For instance, given the tree:
  --
  -- > t = Node 'a' [ Node 'b' [ Node 'd' []], Node 'c' []]
  --
  -- And the following traversal:
  --
  -- > foldoM mappend (\as ds t c -> traceShow c (return (getSum t,Sum 1))) t
  --
  -- Note the different print orders for Identity and IO:
  --
  -- IO:
  -- > 'a'
  -- > 'b'
  -- > 'd'
  -- > 'c'
  --
  -- Identity:
  -- > 'a'
  -- > 'c'
  -- > 'd'
  -- > 'b'
  --
  -- Note that 
  foldoM :: (Monoid m, MonadFix c) => m -> (m -> m -> m) -> (m -> m -> m -> a -> c (b,m)) -> f a -> c (f b,m)

  -- | foldsl folds from the top/start and mappends from left-to-right
  foldsl :: Monoid m => (m -> m -> m -> a -> (b,m)) -> f a -> (f b,m)
  -- foldsl f g = foldo mempty mappend (\pm cm m a -> (f pm cm m a, g pm a))

  -- | foldslM folds from the top/start and mappends from left-to-right monadically
  foldslM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  foldslM f g =
    foldoM mempty mappend
      (\pm cm m a -> mdo
          m' <- g pm a
          b  <- f pm cm m a
          return (b,m')
      )

  -- | foldsr folds from the top/start and mappends from right-to-left
  foldsr :: Monoid m => (m -> m -> m -> a -> (b,m)) -> f a -> (f b,m)
  -- foldsr f g = foldo mempty (flip mappend) (\pm cm m a -> (f pm cm m a, g pm a))

  -- | foldsrM folds from the top/start and mappends from right-to-left monadically
  foldsrM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  foldsrM f g =
    foldoM mempty (flip mappend)
      (\pm cm m a -> mdo
          m' <- g pm a
          b  <- f pm cm m a
          return (b,m')
      )

  -- | foldel folds from the bottom/end and mappends from left-to-right
  foldel :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldel f g = foldo mempty mappend (\pm cm m a -> (f pm cm m a,g cm a))

  -- | foldel folds from the bottom/end and mappends from left-to-right monadically
  foldelM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  foldelM f g =
    foldoM mempty mappend
      (\pm cm m a -> mdo
          m' <- g cm a
          b  <- f pm cm m a
          return (b,m')
      )

  -- | folder folds from the bottom/end and mappends from right-to-left
  folder :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  folder f g = foldo mempty (flip mappend) (\pm cm m a -> (f pm cm m a,g cm a))

  -- | folder folds from the bottom/end and mappends from right-to-left monadically
  folderM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  folderM f g =
    foldoM mempty (flip mappend)
      (\pm cm m a -> mdo
          m' <- g cm a
          b  <- f pm cm m a
          return (b,m')
      )

instance Origami [] where
  foldo m0 c f la = (lb,final)
    where
      ~(lb,final) = go m0 la
        where
          go as_st []      = ([],as_st)
          go as_st ~(a:as) = (b:bs,c bs_st st)
            where
              ~(b,st) = f as_st bs_st final a
              ~(bs,bs_st) = go as_st as
  {-# INLINE foldo #-}

  foldoM m c f la = mdo
    let go as_st [] = return ([],as_st)
        go as_st ~(a:as) = mdo
          ~(b,st) <- f as_st bs_st final a
          ~(bs,bs_st) <- go as_st as
          return (b:bs,c bs_st st)
    ~(lb,final) <- go m la
    return (lb,final)
  {-# INLINE foldoM #-}

testList b =
  let as = [1..5]
      i = Sum (1 :: Int)
      (xs1,x1) = foldsl (\as ds t c -> traceShow c (i,i)) as
      b1 = trace "\nsl\n" 0
      (xs2,x2) = foldsr (\as ds t c -> traceShow c (i,i)) as
      b2 = trace "\nsr\n" 0
      (xs3,x3) = foldel (\as ds t c -> traceShow c i) (\_ c -> traceShow c $ Sum 1) as
      b3 = trace "\nel\n" 0
      (xs4,x4) = folder (\as ds t c -> traceShow c i) (\_ c -> traceShow c $ Sum 1) as
      b4 = trace "\ner\n" 0
  in if b
       then b1 `seq` x1 `seq` b2 `seq` x2 `seq` b3 `seq` x3 `seq` b4 `seq` x4 `seq` x1 + x2 + x3 + x4
       else b1 `seq` xs1 `deepseq` b2 `seq` xs2 `deepseq` b3 `seq` xs3 `deepseq` b4 `seq` xs4 `deepseq` 0

testTree =
  let tree = Node 1 [ Node 2 [ Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]
      t = trace (drawTree $ fmap show tree) 0
      (_,x1) = foldsl (\as ds t c -> traceShow c (Sum 1,Sum 1)) tree
      b1 = trace "\nsl\n" 0
      (_,x2) = foldsr (\as ds t c -> traceShow c (Sum 1,Sum 1)) tree
      b2 = trace "\nsr\n" 0
      (_,x3) = foldel (\as ds t c -> traceShow c (Sum 1)) (\_ c -> traceShow c $ Sum 1) tree
      b3 = trace "\nel\n" 0
      (_,x4) = folder (\as ds t c -> traceShow c (Sum 1)) (\_ c -> traceShow c $ Sum 1) tree
      b4 = trace "\ner\n" 0
  in t `seq` b1 `seq` x1 `seq` b2 `seq` x2 `seq` b3 `seq` x3 `seq` b4 `seq` x4 `seq` x1 + x2 + x3 + x4

instance Origami Tree where
  foldo m0 c f ta = (tb,final)
    where
      ~(tb,final) = go m0 ta
        where
          go as_st (Node rt frst) = (node,m)
            where
              ~(node,m) = (Node rt' frst',c frst_st st)
              ~(rt',st) = f as_st frst_st final rt
              ~(frst',frst_st) = go' as_st frst
                where
                  go' as_st []      = ([],as_st)
                  go' as_st ~(a:as) = (b:bs,c bs_st st)
                    where
                      ~(b,st) = go as_st a
                      ~(bs,bs_st) = go' as_st as
  {-# INLINE foldo #-}

  foldsl = foldo mempty mappend

  foldsr f ta = (tb,final)
    where
      ~(tb,final) = go mempty ta
        where
          go as_st (Node rt frst) = (node,m)
            where
              ~(node,m) = (Node rt' frst',flip (<>) frst_st st)
              ~(rt',st) = f as_st frst_st final rt
              ~(frst',frst_st) = go' as_st frst
                where
                  go' as_st []      = ([],as_st)
                  go' as_st ~(a:as) = (b:bs,flip (<>) st bs_st)
                    where
                      ~(b,st) = go as_st a
                      ~(bs,bs_st) = go' as_st as
  {-# INLINE foldsr #-}

  foldoM m c f ta = mdo
    let go as_st (Node rt frst) = mdo
          ~(rt',st)   <- f as_st frst_st final rt
          -- ~(frst',ms) <- fmap unzip $ mapM (go (c as_st st)) frst
          ~(frst',frst_st) <- foldoM as_st c (\ps _ _ -> foldoM ps c f) frst
          -- let frst_st = foldr c mempty ms
          return (Node rt' frst',c st frst_st)
    ~(tb,final) <- go m ta
    return (tb,final)
  {-# INLINE foldoM #-}

instance Origami Seq where
  foldo m0 c f sa = (sb,final)
    where
      ~(sb,final) = go m0 sa
        where
          go as_st sa =
            case viewl sa of
              EmptyL -> (mempty,as_st)
              ~(a :< as) ->
                let
                  ~(b,st) = f as_st bs_st final a
                  ~(bs,bs_st) = go (c as_st st) as
                in
                  (b <| bs,c st bs_st)
  {-# INLINE foldo #-}

  foldoM m c f sa = mdo
    let go as_st sa =
          case viewl sa of
            EmptyL  -> return (mempty,mempty)
            ~(a :< as) -> mdo
              ~(b,st) <- f as_st bs_st final a
              ~(bs,bs_st) <- go (c as_st st) as
              return (b <| bs, c st bs_st)
    ~(sb,final) <- go m sa
    return (sb,final)
  {-# INLINE foldoM #-}
  -- TODO: implement more efficient methods for Seq that
  --       actually deconstruct the Seq from right to left
