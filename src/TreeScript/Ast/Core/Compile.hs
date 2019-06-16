{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serialize and deserialize compiled code into a file.
module TreeScript.Ast.Core.Compile
  ( decompile
  )
where

import           TreeScript.Ast.Core.Types
import           TreeScript.Misc

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Int
import qualified Data.Text                     as T

convertBinaryRes
  :: (Monad m, MonadResult m)
  => Either (B.ByteString, ByteOffset, String) (B.ByteString, ByteOffset, a)
  -> m a
convertBinaryRes (Left (_, _, msg)) =
  mkFail $ desugarError_ $ "couldn't extract module: " <> T.pack msg
convertBinaryRes (Right (rem', _, prog)) = do
  unless (B.null rem') $ tellError $ desugarError_
    "some input left after decompiling"
  pure prog

getByteString64 :: Get B.ByteString
getByteString64 = do
  len :: Int64 <- get
  getLazyByteString len

putByteString64 :: B.ByteString -> Put
putByteString64 str = do
  put (fromIntegral $ B.length str :: Int64)
  putLazyByteString str

-- | Extract the module data from a compiled executable.
decompile :: (Monad m, MonadResult m) => B.ByteString -> m (Program ())
decompile ser =
  (convertBinaryRes . decodeOrFail =<<)
    $ convertBinaryRes
    $ (`runGetOrFail` ser)
    $ do
        let getHeader = do
              nxt <- getWord8
              when (nxt /= 10) -- '\n' :: Word8
                               getHeader
        label "shebang header" getHeader
        sprog <- label "module" getByteString64
        _     <- label "interpreted" getByteString64
        pure sprog
