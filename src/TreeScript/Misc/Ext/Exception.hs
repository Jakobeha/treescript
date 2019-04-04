module TreeScript.Misc.Ext.Exception
  ( ignoreException
  ) where

import Control.Exception

ignoreException :: (Applicative m) => SomeException -> m ()
ignoreException _ = pure ()
