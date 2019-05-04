{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serialize and deserialize compiled code into a file.
module TreeScript.Ast.Core.Compile
  ( decompile
  , export
  , exportFile
  , exportInterp
  )
where

import           TreeScript.Ast.Core.Misc
import           TreeScript.Misc
import           TreeScript.Plugin

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Int
import qualified Data.Text                     as T
import           System.Posix.Files
import           System.Posix.Types

convertBinaryRes
  :: (Monad m, MonadResult m)
  => Either (B.ByteString, ByteOffset, String) (B.ByteString, ByteOffset, a)
  -> m a
convertBinaryRes (Left (_, _, msg)) =
  mkFail $ desugarError_ $ "couldn't extract module: " <> T.pack msg
convertBinaryRes (Right (rem', _, prog)) = do
  unless (B.null rem')
    $ tellError
    $ desugarError_
    $ "some input left after decompiling"
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

-- | Serialize the program, and add a shebang which makes it run as an executable.
export :: Program () -> B.ByteString
export prog = runPut $ do
  putStringUtf8 "#! /usr/bin/env treescript-interpreter\n"
  putByteString64 $ encode prog
  putByteString64 $ encodeInterp prog

exportFileMode :: FileMode
exportFileMode = CMode 0o755 -- Everyone can read and execute, owner can write

-- | Serialize the program as an executable into the output path.
exportFile :: FilePath -> Program () -> SessionRes ()
exportFile outPath prog = liftIOAndCatch StageWriteCompiled $ do
  B.writeFile outPath $ export prog
  setFileMode outPath exportFileMode

-- | Serialize the interpreter data into the output path, for testing.
exportInterp :: FilePath -> Program () -> SessionRes ()
exportInterp outPath =
  liftIOAndCatch StageWriteCompiled . B.writeFile outPath . encodeInterp
