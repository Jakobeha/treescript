module TreeScript.Misc.Ext.Streams
  ( withFileAsInput
  , withFileAsOutput
  , fileToInputStream
  , peekn
  , dropn
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

peekn :: Int -> S.InputStream a -> IO (Maybe [a])
peekn len inp = do
  let unReadn' = mapM_ (`S.unRead` inp)
      readn' 0 = pure $ Just []
      readn' n = do
        oxs <- readn' $ n - 1
        case oxs of
          Nothing -> pure Nothing
          Just xs -> do
            ox <- S.read inp
            case ox of
              Nothing -> do
                unReadn' xs
                pure Nothing
              Just x -> pure $ Just $ x : xs
  oxs <- readn' len
  case oxs of
    Nothing -> pure ()
    Just xs -> unReadn' xs
  pure oxs

dropn :: Int -> S.InputStream a -> IO Bool
dropn 0 _   = pure True
dropn n inp = do
  x <- S.read inp
  case x of
    Nothing -> pure False
    Just _  -> dropn (n - 1) inp
