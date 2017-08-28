{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}
{-# language RecursiveDo #-}
module Data.Origami where

import Control.Monad.Fix

import Data.Monoid

import Data.Tree
import Data.Sequence

-- | A generalized origami-like lazy monoidal mapping fold.
--
-- As an example, imagine walking an m-ary tree and labeling each node with the
-- length of the path from the root to that node plus its count of children
-- plus one for the current node and then dividing that by the total number
-- of nodes in the tree - a sort of relative weight.
--
-- > foldll
-- >   (\as@ancestors cs@children t@total a ->
-- >     ( getSum (as <> cs <> Sum 1) / getSum t
-- >     , a -- leave the element unmodified
-- >     )
-- >   )
-- >   (\_ _ -> Sum 1) -- one for each node
--
-- Given the tree:
--
-- > 1
-- > |
-- > +- 2
-- > |  |
-- > |  `- 3
-- > |
-- > +- 4
-- > |
-- > `- 5
-- >    |
-- >    `- 6
-- >       |
-- >       `- 7
-- >          |
-- >          +- 8
-- >          |
-- >          +- 9
-- >          |
-- >          `- 10
--
-- The above origami fold would produce:
--
-- > (1.0,1)
-- > |
-- > +- (0.3,2)
-- > |  |
-- > |  `- (0.3,3)
-- > |
-- > +- (0.2,4)
-- > |
-- > `- (0.7,5)
-- >    |
-- >    `- (0.7,6)
-- >       |
-- >       `- (0.7,7)
-- >          |
-- >          +- (0.5,8)
-- >          |
-- >          +- (0.5,9)
-- >          |
-- >          `- (0.5,10)
--
-- Laziness:
--
-- > foldll (\before after total a -> total) (\_ a -> a) [undefined,Any False,Any True]
-- > Exception: Prelude.undefined
--
-- > Foldlr (\before after total a -> total) (\_ a -> a) [undefined,Any False,Any True]
-- > Exception: Prelude.undefined
--
-- > foldrl (\before after total a -> total) (\_ a -> a) [Any False,Any True,undefined]
-- > ([Any True,Any True,Any True],Any True)
--
-- > foldrr (\before after total a -> total) (\_ a -> a) [Any False,Any True,undefined]
-- > ([Any True,Any True,Any True],Any True)

class Origami f where
  -- | Dragons!
  --
  -- This method is oddly magical. It represents simultaneous beginning-to-end
  -- and end-to-beginning folding and can modify any element in the structure
  -- as if it has already done both. It does, of course, have limitations.
  --
  -- Given: foldo _0 c c' f xs
  --
  --        > c,c' :: m -> m -> m -- mappend or flip mappend, generally
  --        > f :: m -> m -> m -> a -> (b,m)
  --
  -- Note that there are two `c`s, one for the spine and one
  -- for nested structures. For linear structures, like lists,
  -- only the first is used.
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
  -- linear. However, this single traversal could create a shadow
  -- of thunks of equal or greater weight than the structure being
  -- traversed.
  --
  -- The first 'm' represents the `mconcat` of the previous monoidal fold results.
  -- The second 'm' represents the `mconcat` of the future monoidal fold results.
  -- The third 'm' represents the final monoidal fold result for the entire structure; previous m <> current m <> future m.
  --
  -- For safety, these methods should keep you from looping:
  --   pure: foldll, foldlr, foldrl, foldrr
  --   monadic: foldllM, foldlrM, foldrlM, foldrrM
  --
  foldo :: Monoid m => m -> (m -> m -> m) -> (m -> m -> m) -> (m -> m -> m -> a -> (b,m)) -> f a -> (f b,m)

  -- | foldll folds from the top/start and mappends from left-to-right
  {-# INLINE foldll #-}
  foldll :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldll f g = foldo mempty mappend mappend (\as ds t c -> (f as ds t c,g as c))

  -- | foldlr folds from the top/start and mappends from right-to-left
  {-# INLINE foldlr #-}
  foldlr :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldlr f g = foldo mempty mappend (flip mappend) (\as ds t c -> (f as ds t c,g as c))

  -- | foldrl folds from the bottom/end and mappends from left-to-right
  {-# INLINE foldrl #-}
  foldrl :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldrl f g = foldo mempty (flip mappend) mappend (\as ds t c -> (f as ds t c,g ds c))

  -- | foldrr folds from the bottom/end and mappends from right-to-left
  {-# INLINE foldrr #-}
  foldrr :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldrr f g = foldo mempty (flip mappend) (flip mappend) (\as ds t c -> (f as ds t c,g ds c))

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
  -- > foldoM mempty mappend mappend (\as ds t c -> traceShow c (return (getSum t,Sum 1))) t
  --
  -- Note the different print orders for Identity and IO:
  --
  -- IO:
  --
  -- > 'a'
  -- > 'b'
  -- > 'd'
  -- > 'c'
  --
  -- Identity:
  --
  -- > 'c'
  -- > 'd'
  -- > 'b'
  -- > 'a'
  --
  foldoM :: (Monoid m, MonadFix c) => m -> (m -> m -> m) -> (m -> m -> m) -> (m -> m -> m -> a -> c (b,m)) -> f a -> c (f b,m)

  -- | foldllM folds from the top/start and mappends from left-to-right monadically
  {-# INLINE foldllM #-}
  foldllM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  foldllM f g = foldoM mempty mappend mappend $ \as ds t c -> mdo
    b <- f as ds t c
    m <- g as c
    return (b,m)

  -- | foldlrM folds from the top/start and mappends from right-to-left monadically
  {-# INLINE foldlrM #-}
  foldlrM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  foldlrM f g = foldoM mempty mappend (flip mappend) $ \as ds t c -> mdo
    b <- f as ds t c
    m <- g as c
    return (b,m)

  -- | foldrlM folds from the bottom/end and mappends from left-to-right monadically
  {-# INLINE foldrlM #-}
  foldrlM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  foldrlM f g = foldoM mempty (flip mappend) mappend $ \as ds t c -> mdo
    b <- f as ds t c
    m <- g ds c
    return (b,m)

  -- | foldrrM folds from the bottom/end and mappends from right-to-left monadically
  {-# INLINE foldrrM #-}
  foldrrM :: (Monoid m, MonadFix c) => (m -> m -> m -> a -> c b) -> (m -> a -> c m) -> f a -> c (f b,m)
  foldrrM f g = foldoM mempty (flip mappend) (flip mappend) $ \as ds t c -> mdo
    b <- f as ds t c
    m <- g ds c
    return (b,m)

instance Origami [] where
  {-# INLINE foldo #-}
  foldo m0 c1 c2 f la = (lb,final)
    where
      ~(lb,final) = go m0 la
        where
          go as_st []      = ([],as_st)
          go as_st ~(a:as) = (b:bs,bs_st)
            where
              ~(b,st) = f as_st bs_st final a
              ~(bs,bs_st) = go (c1 as_st st) as

  {-# INLINE foldoM #-}
  foldoM m c1 _ f la = mdo
    let go as_st [] = return ([],as_st)
        go as_st ~(a:as) = mdo
          ~(b,st) <- f as_st bs_st final a
          ~(bs,bs_st) <- go as_st as
          return (b:bs,c1 st bs_st)
    ~(lb,final) <- go m la
    return (lb,final)

instance Origami Tree where
  {-# INLINE foldo #-}
  foldo m0 c1 c2 f ta = (tb,final)
    where
      ~(tb,final) = go m0 ta
        where
          go as_st (Node rt frst) = (Node rt' frst',c1 st frst_st)
            where
              ~(rt',st)        = f as_st frst_st final rt
              ~(frst',frst_st) = go' (c1 st as_st) frst
                where
                  go' _     []      = ([],mempty)
                  go' as_st ~(a:as) = (b:bs,c2 st bs_st)
                    where
                      ~(b,st) = go as_st a
                      ~(bs,bs_st) = go' as_st as

  {-# INLINE foldoM #-}
  foldoM m0 c1 c2 f ta = mdo

    let go as_st (Node rt frst) = mdo
          ~(rt',st)        <- f as_st frst_st final rt
          ~(frst',frst_st) <- go' (c1 st as_st) frst
          return (Node rt' frst',c1 st frst_st)

        go' _ [] = return ([],mempty)
        go' as_st ~(a:as) = mdo
          ~(b,st) <- go as_st a
          ~(bs,bs_st) <- go' as_st as
          return (b:bs,c2 st bs_st)

    ~(tb,final) <- go m0 ta
    return (tb,final)

instance Origami Seq where
  {-# INLINE foldo #-}
  foldo m0 c1 _ f sa = (sb,final)
    where
      ~(sb,final) = go m0 sa
        where
          go as_st sa =
            case viewl sa of
              EmptyL -> (mempty,as_st)
              ~(a :< as) ->
                let
                  ~(b,st) = f as_st bs_st final a
                  ~(bs,bs_st) = go as_st as
                in
                  (b <| bs,c1 st bs_st)

  {-# INLINE foldoM #-}
  foldoM m0 c1 _ f sa = mdo
    let go as_st sa =
          case viewl sa of
            EmptyL  -> return (mempty,as_st)
            ~(a :< as) -> mdo
              ~(b,st) <- f as_st bs_st final a
              ~(bs,bs_st) <- go as_st as
              return (b <| bs,c1 st bs_st)
    ~(sb,final) <- go m0 sa
    return (sb,final)
  -- TODO: implement more efficient methods for Seq that
  --       actually deconstruct the Seq from right to left
