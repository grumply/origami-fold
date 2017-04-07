{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}
module Data.Origami where

import Data.Bifunctor
import Data.Monoid

import Data.Tree
import Data.Sequence hiding (fromList,toList)

import GHC.Exts

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
  -- There are two rules:
  --   1. You may choose to use one of the first two 'm's to produce the result
  --      'm', but you must not switch during evaluation. This is equivalent to
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
  -- For safety, use the derived methods: foldsl, foldsr, foldel, folder.
  foldo :: Monoid m => (m -> m -> m) -> (m -> m -> m -> a -> (b,m)) -> f a -> (f b,m)

  -- | foldsl folds from the top/start and mappends from left-to-right
  foldsl :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldsl f g = foldo mappend (\pm cm m a -> (f pm cm m a, g pm a))

  -- | foldsr folds from the top/start and mappends from right-to-left
  foldsr :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldsr f g = foldo (flip mappend) (\pm cm m a -> (f pm cm m a, g pm a))

  -- | foldel folds from the bottom/end and mappends from left-to-right
  foldel :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldel f g = foldo mappend (\pm cm m a -> (f pm cm m a,g cm a))

  -- | folder folds from the bottom/end and mappends from right-to-left
  folder :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  folder f g = foldo (flip mappend) (\pm cm m a -> (f pm cm m a,g cm a))

instance Origami [] where
  foldo c f la = (lb,final)
    where
      (lb,final) = go mempty la
        where
          go as_st [] = ([],mempty)
          go as_st (a:as) = (b:bs,c st bs_st)
            where
              (b,st) = f as_st bs_st final a
              (bs,bs_st) = go (c as_st st) as
  {-# INLINE foldo #-}

instance Origami Tree where
  foldo c f ta = (tb,final)
    where
      (tb,final) = go mempty ta
        where
          go as_st (Node rt frst) = (Node rt' frst',c st frst_st)
            where
              (rt',st) = f as_st frst_st final rt
              (frst',frst_st) = fmap mconcat $ unzip $ flip map frst $ go (c as_st st)
  {-# INLINE foldo #-}

instance Origami Seq where
  foldo c f sa = (sb,final)
    where
      (sb,final) = go mempty sa
        where
          go as_st sa =
            case viewl sa of
              EmptyL -> (mempty,mempty)
              (a :< as) ->
                let
                  (b,st) = f as_st bs_st final a
                  (bs,bs_st) = go (c as_st st) as
                in
                  (b <| bs,c st bs_st)
  {-# INLINE foldo #-}
  -- TODO: implement more efficient methods for Seq that
  --       actually deconstruct the Seq from right to left
