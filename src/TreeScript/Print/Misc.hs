{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-- | Printing classes, from debug-freindly "pretty-printing" to production AST reprints.
module TreeScript.Print.Misc
  ()
where

import           TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text      as T
import           TreeScript.Print.Class

import           Control.Monad.Catch
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding.Error      as T

instance Printable () where
  pprint () = "()"

instance Printable Bool where
  pprint = T.pack . show

instance Printable Char where
  pprint = T.pack . show

instance Printable Int where
  pprint = T.pack . show

instance Printable Float where
  pprint = T.pack . show

instance Printable T.Text where
  pprint txt = "\"" <> T.escapeString txt <> "\""

instance Printable SomeException where
  pprint = T.pack . displayException

instance Printable T.UnicodeException where
  pprint = T.pack . displayException

instance (Printable a) => Printable [a] where
  pprint xs = "[" <> T.intercalate ", " (map pprint xs) <> "]"

instance Printable Loc where
  pprint = printLoc

instance Printable Range where
  pprint = printRange

instance Printable Stage where
  pprint StageSetup   = "setting up"
  pprint StageLex     = "parsing"
  pprint StageBalance = "parsing"
  pprint StageParse   = "parsing"
  pprint StageEval    = "evaluating"

instance Printable Error where
  pprint (Error Nothing    msg) = msg
  pprint (Error (Just rng) msg) = "at " <> pprint rng <> " - " <> msg

instance Printable SError where
  pprint (SError stage err) = "(while " <> pprint stage <> ") " <> pprint err

instance (Ord e, Printable e, Printable a) => Printable (Result e a) where
  pprint (ResultFail err ) = "fatal error: " <> pprint err
  pprint (Result []   res)
    | T.any (== '\n') resPrint = "success:\n" <> pprint res
    | otherwise = "success: " <> pprint res
    where resPrint = pprint res
  pprint (Result errs res) = T.unlines
    ("result:" : pprint res : "errors:" : map (T.bullet . pprint)
                                              (S.toList errs)
    )
