module TreeScript.Misc.Ext.IO
  ( redirectToStdout
  , redirectExceptLast
  ) where

import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.State.Strict
import qualified Data.Text as T
import System.IO

-- | Reads and prints to stdout, line by line
redirectToStdout :: Handle -> IO ()
redirectToStdout handle
  = whileM_ (not <$> hIsEOF handle) $ putStrLn =<< hGetLine handle

-- | Redirects init lines to stdout, returns last line
redirectExceptLast :: Handle -> IO T.Text
redirectExceptLast handle = fmap (foldMap T.pack) $ (`execStateT` Nothing) $
  whileM_ (liftIO $ not <$> hIsEOF handle) $ do
    prev <- get
    case prev of
      Nothing -> pure ()
      Just prev' -> liftIO $ putStrLn prev'
    next <- liftIO $ hGetLine handle
    put $ Just next
