{-# LANGUAGE OverloadedStrings #-}
-- | An input stream which can fail a 'MonadResult' when read.
module TreeScript.Misc.Error.ResultInputStream
  ( ResultInputStream
  , liftInputStream
  , processInputStream
  , mapImpureInputStream
  , mapPureInputStream
  , overStreamErrors
  , readResultStream
  )
where

import           TreeScript.Misc.Error.Result
import           TreeScript.Misc.Error.Error
import           TreeScript.Misc.Loc

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Maybe
import qualified Data.Set                      as Se
import qualified Data.Text                     as T
import           System.Exit
import qualified System.IO.Streams             as S
import           System.Process

data ErrSignal
  = ErrSignalError Error
  | ErrSignalTransformer (Error -> Error)

-- | An input stream which can fail a 'MonadResult' when read. Reading input fails if the next item returns a failure, or the stream ends and the process exits with code 1.
data ResultInputStream a
  = ResultInputStream
  { resultInputStream :: S.InputStream a
  , resultInputStreamErrStream :: S.OutputStream ErrSignal
  , resultInputStreamGetErrSigs :: IO [ErrSignal]
  , resultInputStreamHandle :: Maybe ProcessHandle
  }

liftInputStream :: (MonadIO m) => S.InputStream a -> m (ResultInputStream a)
liftInputStream out = liftIO $ do
  (err, getErrSigs) <- S.listOutputStream
  pure ResultInputStream { resultInputStream           = out
                         , resultInputStreamErrStream  = err
                         , resultInputStreamGetErrSigs = getErrSigs
                         , resultInputStreamHandle     = Nothing
                         }

processInputStream
  :: (MonadIO m) => S.InputStream a -> ProcessHandle -> m (ResultInputStream a)
processInputStream out hdl = liftIO $ do
  (err, getErrSigs) <- S.listOutputStream
  pure ResultInputStream { resultInputStream           = out
                         , resultInputStreamErrStream  = err
                         , resultInputStreamGetErrSigs = getErrSigs
                         , resultInputStreamHandle     = Just hdl
                         }

mapImpureInputStream
  :: (MonadIO m)
  => (S.InputStream a -> IO (S.InputStream (Result b)))
  -> ResultInputStream a
  -> m (ResultInputStream b)
mapImpureInputStream f (ResultInputStream stm estm getErrSigs hdl) =
  liftIO $ do
    stm'  <- f stm
    stm'' <- S.makeInputStream $ do
      res <- S.read stm'
      case res of
        Nothing               -> pure Nothing
        Just (ResultFail err) -> do
          S.write (Just $ ErrSignalError err) estm
          pure Nothing
        Just (Result errs x) -> do
          forM_ (Se.toList errs)
            $ \err -> S.write (Just $ ErrSignalError err) estm
          pure $ Just x
    pure $ ResultInputStream stm'' estm getErrSigs hdl

mapPureInputStream
  :: (MonadIO m)
  => (S.InputStream a -> IO (S.InputStream b))
  -> ResultInputStream a
  -> m (ResultInputStream b)
mapPureInputStream f (ResultInputStream stm estm getErrSigs hdl) = liftIO $ do
  stm' <- f stm
  pure $ ResultInputStream stm' estm getErrSigs hdl

overStreamErrors
  :: (MonadIO m)
  => (Error -> Error)
  -> ResultInputStream a
  -> m (ResultInputStream a)
overStreamErrors f stm = liftIO $ do
  S.write (Just $ ErrSignalTransformer f) $ resultInputStreamErrStream stm
  pure stm

applyErrSig :: [Error] -> ErrSignal -> [Error]
applyErrSig prev (ErrSignalError       err) = err : prev
applyErrSig prev (ErrSignalTransformer f  ) = map f prev

readResultStream
  :: (MonadIO m, MonadResult m) => ResultInputStream a -> m (Maybe a)
readResultStream (ResultInputStream stm estm getErrSigs hdl) = do
  next    <- liftIO $ S.read stm
  errSigs <- liftIO getErrSigs
  let errs = reverse $ foldl' applyErrSig [] errSigs
  tellErrors errs
  when (isNothing next) $ do
    liftIO $ S.write Nothing estm
    case hdl of
      Nothing   -> pure ()
      Just hdl' -> do
        ecode <- liftIO $ waitForProcess hdl'
        case ecode of
          ExitSuccess        -> pure ()
          ExitFailure ecode' -> tellError $ Error
            { errorStage = StageReadInput
            , errorRange = r0
            , errorMsg   = "process exited with code " <> T.pack (show ecode')
            }
  pure next
