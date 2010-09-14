-- |
-- Module      : Scion.Types.Outlines
-- Copyright   : (c) JP Moresmau 2009
-- License     : BSD-style
--
-- Maintainer  : jp@moresmau.fr
-- Stability   : experimental
-- Portability : portable
--
-- Outline representation of source code
--
module Scion.Types.Outline 
  ( OutlineDef(..),TokenDef(..),
    extractNames,trimLocationFile
  )
where

import GHC
import Scion.Types.Notes
import qualified Scion.Types.JSONDictionary as Dic
import Text.JSON.AttoJSON
import Data.List ( foldl' )
import qualified Data.ByteString.Char8 as S

data OutlineDef = OutlineDef
  { od_name       :: Either Name String,
    od_type       :: String,
    od_loc        :: Location,
    od_block      :: Location,
    od_parentName :: Maybe (Name,String)
  }

data TokenDef = TokenDef {
        td_name :: String,
        td_loc :: Location
    }
        deriving (Show,Eq)

extractNames:: [OutlineDef] -> [Name]
extractNames
  = foldl' (\l od -> case od_name od of
	               Left n -> n:l
	               Right _ -> l)
           []

trimLocationFile:: [OutlineDef] -> [OutlineDef]
trimLocationFile =
  map (\d@OutlineDef{ od_loc = l, od_block = b} -> 
           d{ od_loc = trimFile l,
              od_block = trimFile b})

instance JSON TokenDef where
  toJSON t | (_, l0, c0, l1, c1) <- viewLoc $ td_loc t =
    --Dic.makeObject $ 
    --  [("name", str $ td_name t)
    --  ,("region", JSArray (map toJSON [l0,c0,l1,c1]))]
    JSArray ((JSString $ S.pack $ td_name t): (map toJSON [l0,c0,l1,c1]))
  fromJSON _ = fail "TokenDef"