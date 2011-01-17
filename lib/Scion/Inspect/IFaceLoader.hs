-- |
-- Module      : Scion.Inspect.ModuleLoader
-- Copyright   : (c) B. Scott Michel, 2011
-- License     : BSD-style
--
-- Maintainer  : bscottm@ieee.org
-- Stability   : experimental
-- Portability : portable
--
-- Collect and load interface modules.
--
-- Note: The inspiration for this source code comes from the Leksah IDE

module Scion.Inspect.IFaceLoader (
    getModulesForTypecheck
  ) where

import Scion.Types

-- GHC's imports
import GHC
import Module

-- Exception handling, basically for when lookupModule decides it can't find 
import qualified Control.Exception as Exc

-- | Get the list of modules associated with the type-checked source
getModulesForTypecheck :: Maybe BgTcCache           -- ^ The type-checked source
                       -> ScionM (Module, [Module]) -- ^ The list of modules

getModulesForTypecheck (Just (Typechecked tcm)) = getModules (tm_parsed_module tcm)
getModulesForTypecheck (Just (Parsed pm))       = getModules pm
getModulesForTypecheck Nothing = undefined

-- | Unknown module name
unknownModule mod = mkModule (stringToPackageId "*unknown*") mod
-- |
getModules :: ParsedModule
           -> ScionM (Module, [Module])
getModules modSummary =
  let thisModSum      = pm_mod_summary modSummary
      thisMod         = ms_mod thisModSum
      innerImports    = map unLoc $ ms_imps thisModSum
      innerModNames   = map (unLoc . ideclName) innerImports
      getInnerModules = mapM (\m -> lookupModule m Nothing) innerModNames
      modLookup mod   = Exc.catch liftIO (lookupModule mod Nothing)
                                  (\_ -> return $ unknownModule mod)
  in  getInnerModules >>= (\innerMods -> return (thisMod, innerMods))
