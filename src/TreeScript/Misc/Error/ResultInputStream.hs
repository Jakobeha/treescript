-- | An input stream which can fail a 'MonadResult' when read.
module TreeScript.Misc.Error.ResultInputStream
  ( ResultInputStream(ResultInputStream)
  )
where

import           TreeScript.Misc.Error.Result

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Environment
import           System.Exit
import           System.IO
import qualified System.IO.Streams             as S
import           System.Process

-- | An input stream which can fail a 'MonadResult' when read. Reading input fails if the next item returns a failure, the error handle gets data, or the stream ends and the process exits with code 1.
data ResultInputStream a
  = ResultInputStream
  { resultInputStream :: S.InputStream (Result a)
  }
