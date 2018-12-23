{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Test.HUnit
  ( tryTest
  , denoteFail
  , denoteFailIn
  ) where

import Control.Exception
import Data.Maybe
import qualified Data.Text as T
import Test.HUnit.Lang
import Type.Reflection

-- | An arbitrary exception with a prefix in its description.
data SomePrefixedException e
  = SomePrefixedException String e
  deriving (Typeable)

instance (Show e) => Show (SomePrefixedException e) where
  show (SomePrefixedException pre exc)
    = addPrefixToMsg pre $ show exc

instance (Exception e) => Exception (SomePrefixedException e) where
  displayException (SomePrefixedException pre exc)
    = addPrefixToMsg pre $ displayException exc

-- | Will return 'Nothing' if an exception was raised.
tryTest :: IO a -> IO (Maybe a)
tryTest action = do
  res <- try action
  case res of
       Left HUnitFailure{} -> pure Nothing
       Right val -> pure $ Just val

addPrefixToReason :: String -> FailureReason -> FailureReason
addPrefixToReason pre (Reason msg) = Reason $ addPrefixToMsg pre msg
addPrefixToReason pre (ExpectedButGot extraMsg expected got)
  = ExpectedButGot (addPrefixToExtraMsg pre extraMsg) expected got

addPrefixToFailure :: String -> HUnitFailure -> HUnitFailure
addPrefixToFailure pre (HUnitFailure srcLoc reason)
  = HUnitFailure srcLoc $ addPrefixToReason pre reason

addPrefixToGenException :: (Exception e) => String -> e -> SomePrefixedException e
addPrefixToGenException pre exc = SomePrefixedException pre exc

addPrefixToMsg :: String -> String -> String
addPrefixToMsg pre msg = pre ++ msg

addPrefixToExtraMsg :: String -> Maybe String -> Maybe String
addPrefixToExtraMsg pre = Just . addPrefixToMsg pre . fromMaybe ""

rethrowHUnitWithPrefix :: String -> HUnitFailure -> IO a
rethrowHUnitWithPrefix pre = throw . addPrefixToFailure pre

rethrowGenWithPrefix :: String -> SomeException -> IO a
rethrowGenWithPrefix pre = throw . addPrefixToGenException pre

-- | Prepends the given string to the error message.
rethrowWithPrefix :: String -> [Handler a]
rethrowWithPrefix pre =
  [ Handler $ rethrowHUnitWithPrefix pre
  , Handler $ rethrowGenWithPrefix pre
  ]

-- | If a failure occurs, prepends the given string to the error message.
denoteFail :: T.Text -> IO a -> IO a
denoteFail pre test = test `catches` rethrowWithPrefix (T.unpack pre)

-- | If a failure occurs, denotes that it occurred for "in" the given label.
denoteFailIn :: T.Text -> IO a -> IO a
denoteFailIn label = denoteFail $ "in " <> label <> ": "
