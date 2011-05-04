{-# LANGUAGE ExistentialQuantification #-}
module Scion.Types.Core
  ( module Scion.Types.Core
  , module Exception
  , MonadIO(..),
  )
where

import MonadUtils ( MonadIO(..) ) -- from GHC
import Exception

io :: MonadIO m => IO a -> m a
io = liftIO
{-# INLINE io #-}

gcatches :: (MonadIO m, ExceptionMonad m) =>
            m a -> [HandlerM m a] -> m a
gcatches io handlers = io `gcatch` catchesHandler handlers

-- | You need this when using 'catches'.
data HandlerM m a = forall e . Exception e => HandlerM (e -> m a)

catchesHandler :: (MonadIO m, ExceptionMonad m) => 
                  [HandlerM m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (io (throwIO e)) handlers
    where tryHandler (HandlerM handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res
