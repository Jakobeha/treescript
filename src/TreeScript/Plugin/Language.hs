{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Plugin.Language
  ( Language(..)
  , languageExt
  , langWithExt
  , langWithName
  , languageName
  )
where

import           Data.Binary
import qualified Data.Text                     as T
import           GHC.Generics

-- | Describes a language.
data Language
  = LanguageStx
  | LanguageJavaScript
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Extension of the language.
languageExt :: Language -> T.Text
languageExt LanguageStx        = ""
languageExt LanguageJavaScript = "js"

-- | Gets the language for the given extension in the session.
langWithExt :: T.Text -> Maybe Language
langWithExt ""   = Just LanguageStx
langWithExt "js" = Just LanguageJavaScript
langWithExt _    = Nothing

-- | Gets the language for the given name.
langWithName :: T.Text -> Maybe Language
langWithName "Syntax"     = Just LanguageStx
langWithName "JavaScript" = Just LanguageJavaScript
langWithName _            = Nothing

-- | Name of the language
languageName :: Language -> T.Text
languageName LanguageStx        = "Syntax"
languageName LanguageJavaScript = "JavaScript"
