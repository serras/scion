-- |
-- Module      : Scion.Types.ExtraInstances
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@googlemail.com
-- Stability   : experimental
-- Portability : portable
--
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scion.Types.ExtraInstances where


#if CABAL_VERSION == 106
import Data.String
#endif

import Bag

import Data.Monoid
import Data.Foldable

instance Monoid (Bag a) where
  mempty = emptyBag
  mappend = unionBags
  mconcat = unionManyBags

instance Functor Bag where
  fmap = mapBag

instance Foldable Bag where
  foldr = foldrBag
  foldl = foldlBag
  

#if CABAL_VERSION == 106
instance IsString ModuleName where
     fromString = simple
#endif

