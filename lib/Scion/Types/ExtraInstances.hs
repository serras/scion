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
{-# LANGUAGE TemplateHaskell #-}

module Scion.Types.ExtraInstances where

import Data.Data
import Data.List as DL
import Data.DeriveTH

#if CABAL_VERSION == 106
import Data.String
#endif

import Distribution.Compiler
import Distribution.ModuleName
import Distribution.License
import Distribution.PackageDescription 
import Distribution.Package
import Distribution.System
import Distribution.Version

import Language.Haskell.Extension

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
  
$( derive makeTypeable ''Extension )
$( derive makeData ''Extension )

$( derive makeTypeable ''License )
$( derive makeData ''License )

$( derive makeTypeable ''CompilerFlavor )
$( derive makeData ''CompilerFlavor )

$( derive makeTypeable ''VersionRange )
$( derive makeData ''VersionRange )

$( derive makeData ''Version )

$( derive makeTypeable ''PackageIdentifier )
$( derive makeData ''PackageIdentifier )

$( derive makeTypeable ''SourceRepo )
$( derive makeData ''SourceRepo )

$( derive makeTypeable ''ModuleName )


instance Data ModuleName where
     gfoldl f z mn     = z (fromString . DL.concat . intersperse ".") `f` (components mn)

     gunfold k z _ = k (z (fromString . DL.concat . intersperse "."))

     toConstr _ = tyC
     dataTypeOf _ = ty_T

#if CABAL_VERSION == 106
instance IsString ModuleName where
     fromString = simple
#endif

tyC :: Constr
tyC = mkConstr ty_T ("ModuleName") [] Prefix
ty_T :: DataType
ty_T   = mkDataType "Distribution.ModuleName.ModuleName" [tyC]

$( derive makeTypeable ''RepoKind )
$( derive makeData ''RepoKind )

$( derive makeTypeable ''RepoType )
$( derive makeData ''RepoType )

$( derive makeTypeable ''OS )
$( derive makeData ''OS )

$( derive makeTypeable ''BuildType )
$( derive makeData ''BuildType )

$( derive makeTypeable ''Arch )
$( derive makeData ''Arch )

$( derive makeTypeable ''PackageName )
$( derive makeData ''PackageName )

$( derive makeTypeable ''BuildInfo )
$( derive makeData ''BuildInfo )

$( derive makeTypeable ''ConfVar )
$( derive makeData ''ConfVar )

$( derive makeTypeable ''Dependency )
$( derive makeData ''Dependency )

$( derive makeTypeable ''Library )
$( derive makeData ''Library )

$( derive makeTypeable ''Condition )
$( derive makeData ''Condition )

$( derive makeTypeable ''Executable )
$( derive makeData ''Executable )

$( derive makeTypeable ''CondTree )
$( derive makeData ''CondTree )

$( derive makeTypeable ''FlagName )
$( derive makeData ''FlagName )

$( derive makeTypeable ''PackageDescription )
$( derive makeData ''PackageDescription )

$( derive makeTypeable ''Flag )
$( derive makeData ''Flag )

$( derive makeTypeable ''GenericPackageDescription )
$( derive makeData ''GenericPackageDescription )
