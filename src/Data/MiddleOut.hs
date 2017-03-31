{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}
module Data.MiddleOut where

import Data.Bifunctor
import Data.Monoid

import Data.Tree
import Data.Sequence hiding (fromList,toList)

import GHC.Exts

-- | A generalized middle-out mapping fold.
--
-- > foldsl (\before after total a -> total) (\_ a -> a) [undefined,False,True]
-- Exception: Prelude.undefined
--
-- > foldsr (\before after total a -> total) (\_ a -> a) [undefined,False,True]
-- ([True,True,True],True)
--
-- > foldel (\before after total a -> total) (\_ a -> a) [undefined,False,True]
-- Exception: Prelude.undefined
--
-- > folder (\before after total a -> total) (\_ a -> a) [undefined,False,True]
-- ([True,True,True],True)
class MiddleOut f where
  foldo :: Monoid m => (m -> m -> m) -> (m -> m -> m -> a -> (b,m)) -> f a -> (f b,m)

  foldsl :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldsl f g = foldo mappend $ \pm cm m a -> (f pm cm m a, g pm a)

  foldsl' :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldsl' f g = foldo (\x y -> let !z = mappend x y in z) $ \pm cm m a -> (f pm cm m a, g pm a)

  foldsr :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldsr f g = foldo (flip mappend) $ \pm cm m a -> (f pm cm m a, g pm a)

  foldsr' :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldsr' f g = foldo (\x y -> let !z = mappend y x in z) $ \pm cm m a -> (f pm cm m a, g pm a)

  foldel :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldel f g = foldo mappend $ \pm cm m a -> (f pm cm m a,g cm a)

  foldel' :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  foldel' f g = foldo (\x y -> let !z = mappend x y in z) $ \pm cm m a -> (f pm cm m a,g cm a)

  folder :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  folder f g = foldo (flip mappend) $ \pm cm m a -> (f pm cm m a,g cm a)

  folder' :: Monoid m => (m -> m -> m -> a -> b) -> (m -> a -> m) -> f a -> (f b,m)
  folder' f g = foldo (\x y -> let !z = mappend y x in z) $ \pm cm m a -> (f pm cm m a,g cm a)

instance MiddleOut [] where
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

instance MiddleOut Tree where
  foldo c f ta = (tb,final)
    where
      (tb,final) = go mempty ta
        where
          go as_st (Node rt frst) = (Node rt' frst',c st frst_st)
            where
              (rt',st) = f as_st frst_st final rt
              (frst',frst_st) = fmap mconcat $ unzip $ flip map frst $ go (c as_st st)
  {-# INLINE foldo #-}

instance MiddleOut Seq where
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
