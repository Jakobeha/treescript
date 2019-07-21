{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An input stream which can fail a 'MonadEResult' when read.
module TreeScript.Misc.Error.EResultInputStream
  ( EResultInputStream
  , liftInputStream
  , processInputStream
  , mapImpureInputStream
  , mapPureInputStream
  , overStreamErrors
  , readEResultStream
  )
where

import           TreeScript.Misc.Error.Result
import           TreeScript.Misc.Error.Error

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

-- | An input stream which can fail a 'MonadEResult' when read. Reading input fails if the next item returns a failure, or the stream ends and the process exits with code 1.
data EResultInputStream a
  = EResultInputStream
  { resultInputStream :: S.InputStream a
  , resultInputStreamErrStream :: S.OutputStream ErrSignal
  , resultInputStreamGetErrSigs :: IO [ErrSignal]
  , resultInputStreamHandle :: Maybe ProcessHandle
  }

liftInputStream :: (MonadIO m) => S.InputStream a -> m (EResultInputStream a)
liftInputStream out = liftIO $ do
  (err, getErrSigs) <- S.listOutputStream
  pure EResultInputStream { resultInputStream           = out
                          , resultInputStreamErrStream  = err
                          , resultInputStreamGetErrSigs = getErrSigs
                          , resultInputStreamHandle     = Nothing
                          }

processInputStream
  :: (MonadIO m) => S.InputStream a -> ProcessHandle -> m (EResultInputStream a)
processInputStream out hdl = liftIO $ do
  (err, getErrSigs) <- S.listOutputStream
  pure EResultInputStream { resultInputStream           = out
                          , resultInputStreamErrStream  = err
                          , resultInputStreamGetErrSigs = getErrSigs
                          , resultInputStreamHandle     = Just hdl
                          }

mapImpureInputStream
  :: (MonadIO m)
  => (S.InputStream a -> IO (S.InputStream (EResult b)))
  -> EResultInputStream a
  -> m (EResultInputStream b)
mapImpureInputStream f (EResultInputStream stm estm getErrSigs hdl) =
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
    pure $ EResultInputStream stm'' estm getErrSigs hdl

mapPureInputStream
  :: (MonadIO m)
  => (S.InputStream a -> IO (S.InputStream b))
  -> EResultInputStream a
  -> m (EResultInputStream b)
mapPureInputStream f (EResultInputStream stm estm getErrSigs hdl) = liftIO $ do
  stm' <- f stm
  pure $ EResultInputStream stm' estm getErrSigs hdl

overStreamErrors
  :: (MonadIO m)
  => (Error -> Error)
  -> EResultInputStream a
  -> m (EResultInputStream a)
overStreamErrors f stm = liftIO $ do
  S.write (Just $ ErrSignalTransformer f) $ resultInputStreamErrStream stm
  pure stm

applyErrSig :: [Error] -> ErrSignal -> [Error]
applyErrSig prev (ErrSignalError       err) = err : prev
applyErrSig prev (ErrSignalTransformer f  ) = map f prev

readEResultStream :: (MonadIOEResult m) => EResultInputStream a -> m (Maybe a)
readEResultStream (EResultInputStream stm estm getErrSigs hdl) = do
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
          ExitFailure ecode' -> tellError Error
            { errorRange = Nothing
            , errorMsg   = "process exited with code " <> T.pack (show ecode')
            }
  pure next
