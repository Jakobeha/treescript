module TreeScript.Misc.Ext.Streams
  ( withFileAsInput
  , withFileAsOutput
  , fileToInputStream
  )
where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import           System.IO
import qualified System.IO.Streams             as S

withFileAsInput
  :: (MonadMask m, MonadIO m)
  => FilePath
  -> (S.InputStream B.ByteString -> m a)
  -> m a
withFileAsInput pth f =
  bracket (liftIO $ openFile pth ReadMode) (liftIO . hClose) $ \hdl -> do
    inp <- liftIO $ S.handleToInputStream hdl
    f inp

withFileAsOutput
  :: (MonadMask m, MonadIO m)
  => FilePath
  -> (S.OutputStream B.ByteString -> m a)
  -> m a
withFileAsOutput pth f =
  bracket (liftIO $ openFile pth WriteMode) (liftIO . hClose) $ \hdl -> do
    out <- liftIO $ S.handleToOutputStream hdl
    f out

-- TODO Handle exceptions properly
fileToInputStream
  :: (MonadMask m, MonadIO m) => FilePath -> m (S.InputStream B.ByteString)
fileToInputStream pth = do
  hdl <- liftIO $ openFile pth ReadMode
  liftIO
    $   S.lockingInputStream
    =<< S.atEndOfInput (hClose hdl)
    =<< S.handleToInputStream hdl
