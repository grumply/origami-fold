{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language RecursiveDo #-}
module Data.Origami.List where

import Control.Monad.Fix

import Data.Bifunctor
import Data.Monoid

import Data.Tree
import Data.Sequence hiding (fromList,toList)

import GHC.Exts

import Data.Origami.Class

instance Origami [] where
  -- foldo c f la = (lb,final)
  --   where
  --     ~(lb,final) = go mempty la
  --       where
  --         go _     []     = ([],mempty)
  --         go as_st (a:as) = (b:bs,c st bs_st)
  --           where
  --             ~(b,st) = f as_st bs_st final a
  --             ~(bs,bs_st) = go (c as_st st) as
  -- {-# INLINE foldo #-}

  foldsl c f la = (lb,final)
    where
      ~(lb,final) = foldl (\ ~(bs,m) t ->
                            let ~(b,m') = _
                            in (b:bs,c m m')
                          ) ([],mempty) la

        -- go mempty la
        -- where
        --   go _     []     = ([],mempty)
        --   go as_st (a:as) = (b:bs,c st bs_st)
        --     where
        --       ~(b,st) = f as_st bs_st final a
        --       ~(bs,bs_st) = go (c as_st st) as
  {-# INLINE foldsl #-}

  -- foldoM c f la = mdo
  --   let go _ [] = return ([],mempty)
  --       go as_st (a:as) = mdo
  --         ~(b,st) <- f as_st bs_st final a
  --         ~(bs,bs_st) <- go (c as_st st) as
  --         return (b:bs,c st bs_st)
  --   ~(lb,final) <- go mempty la
  --   return (lb,final)
  -- {-# INLINE foldoM #-}

