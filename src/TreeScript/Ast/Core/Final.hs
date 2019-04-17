{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Ast.Core.Final
  (

  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Misc

import qualified Data.Text as T
import GHC.Generics

-- | The type and identifier of a group.
data instance GroupLoc 'Final an
  = GroupLocGlobal an Int
  | GroupLocLocal an Int
  | GroupLocFunction an T.Text
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of statements, which can be referenced by other statements.
data instance GroupDef 'Final an
  = GroupDef
  { groupDefAnn :: an
  , groupDefValueProps :: [Bind 'Final an]
  , groupDefGroupProps :: [Bind 'Final an]
  , groupDefReducers :: [Reducer 'Final an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

instance (Semigroup an) => Semigroup (Program 'Final an) where
  Program xAnn xDecls xGroups <> Program yAnn yDecls yGroups
    = Program
    { programAnn = xAnn <> yAnn
    , programRecordDecls = xDecls <> yDecls
    , programGroups = xGroups <> yGroups
    }

instance (Monoid an) => Monoid (Program 'Final an) where
  mempty
    = Program
    { programAnn = mempty
    , programRecordDecls = mempty
    , programGroups = mempty
    }

instance Printable (GroupLoc 'Final an) where
  pprint (GroupLocGlobal _ idx) = "&+" <> pprint idx
  pprint (GroupLocLocal _ idx) = "&-" <> pprint idx
  pprint (GroupLocFunction _ txt) = "#" <> txt

instance Printable (Program 'Final an) where
  pprint (Program _ decls groups)
    = T.unlines $ map pprint decls ++ [T.empty] ++ zipWith printGroupDef [0..] groups

printGroupDef :: Int -> GroupDef 'Final an -> T.Text
printGroupDef head' (GroupDef _ vprops gprops reds)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
           = "&"
          <> pprint head'
          <> printProps (map pprint vprops ++ map printGroupDefProp gprops)
          <> "."
        printGroupDefProp (Bind _ idx) = "&-" <> pprint idx
