{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types for the @Core@ AST.
module TreeScript.Ast.Core.Types
  ( module TreeScript.Ast.Core.Types
  ) where

import TreeScript.Misc

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics

-- | Declares a type of record or function but doesn't specifify property values.
data DeclCompact
  = DeclCompact
  { declCompactHead :: T.Text
  , declCompactNumProps :: Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: S.Set DeclCompact
  , declSetFunctions :: S.Set DeclCompact
  }

-- | Name defines the next step
data SubPhase
  = Local -- ^ Index local binds
  | Final -- ^ Done, now can be translated

type ASTNode a = (Eq a, Ord a, Read a, Show a)
type ASTNode1 a = (Functor a, Foldable a, Traversable a, Generic1 a, Annotatable a)

data family GroupLoc (p :: SubPhase) :: * -> *
data family GroupDef (p :: SubPhase) :: * -> *

-- | Declares a type of record.
data RecordDecl (p :: SubPhase) an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclHead :: T.Text
  , recordDeclProps :: [T.Text]
  }

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (RecordDecl p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (RecordDecl p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (RecordDecl p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (RecordDecl p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (RecordDecl p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (RecordDecl p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (RecordDecl p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (RecordDecl p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (RecordDecl p)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive (p :: SubPhase) an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (Primitive p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (Primitive p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (Primitive p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (Primitive p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (Primitive p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (Primitive p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (Primitive p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (Primitive p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (Primitive p)

-- | Contains a head and properties. A parent in the AST.
data Record (p :: SubPhase) an
  = Record
  { recordAnn :: an
  , recordHead :: T.Text
  , recordProps :: [Value p an]
  }

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (Record p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (Record p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (Record p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (Record p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (Record p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (Record p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (Record p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (Record p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (Record p)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind (p :: SubPhase) an
  = Bind
  { bindAnn :: an
  , bindIdx :: Int
  }

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (Bind p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (Bind p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (Bind p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (Bind p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (Bind p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (Bind p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (Bind p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (Bind p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (Bind p)

-- | Type of data in TreeScript.
data Value (p :: SubPhase) an
  = ValuePrimitive (Primitive p an)
  | ValueRecord (Record p an)
  | ValueBind (Bind p an)

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (Value p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (Value p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (Value p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (Value p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (Value p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (Value p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (Value p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (Value p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (Value p)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef (p :: SubPhase) an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc p an
  , groupRefValueProps :: [Value p an]
  , groupRefGroupProps :: [GroupRef p an]
  }

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (GroupRef p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (GroupRef p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (GroupRef p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (GroupRef p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (GroupRef p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (GroupRef p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (GroupRef p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (GroupRef p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (GroupRef p)

-- | Matches a value against a different value. Like a "let" statement.
data Guard (p :: SubPhase) an
  = Guard
  { guardAnn :: an
  , guardInput :: Value p an
  , guardOutput :: Value p an
  , guardNexts :: [GroupRef p an]
  }

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (Guard p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (Guard p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (Guard p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (Guard p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (Guard p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (Guard p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (Guard p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (Guard p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (Guard p)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer (p :: SubPhase) an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard p an
  , reducerSubGuards :: [Guard p an]
  }

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (Reducer p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (Reducer p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (Reducer p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (Reducer p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (Reducer p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (Reducer p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (Reducer p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (Reducer p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (Reducer p)

-- | A full TreeScript program.
data Program (p :: SubPhase) an
  = Program
  { programAnn :: an
  , programRecordDecls :: [RecordDecl p an]
  , programGroups :: [GroupDef p an]
  }

deriving instance (Eq an, Eq (GroupLoc p an), Eq (GroupDef p an)) => Eq (Program p an)
deriving instance (Ord an, Ord (GroupLoc p an), Ord (GroupDef p an)) => Ord (Program p an)
deriving instance (Read an, Read (GroupLoc p an), Read (GroupDef p an)) => Read (Program p an)
deriving instance (Show an, Show (GroupLoc p an), Show (GroupDef p an)) => Show (Program p an)
deriving instance (Functor (GroupLoc p), Functor (GroupDef p)) => Functor (Program p)
deriving instance (Foldable (GroupLoc p), Foldable (GroupDef p)) => Foldable (Program p)
deriving instance (Traversable (GroupLoc p), Traversable (GroupDef p)) => Traversable (Program p)
deriving instance (Generic1 (GroupLoc p), Generic1 (GroupDef p)) => Generic1 (Program p)
deriving instance (ASTNode1 (GroupLoc p), ASTNode1 (GroupDef p)) => Annotatable (Program p)

instance Semigroup DeclSet where
  DeclSet xRecs xFuns <> DeclSet yRecs yFuns
    = DeclSet
    { declSetRecords = xRecs <> yRecs
    , declSetFunctions = xFuns <> yFuns
    }

instance Monoid DeclSet where
  mempty
    = DeclSet
    { declSetRecords = mempty
    , declSetFunctions = mempty
    }

instance (Printable (GroupLoc p an)) => Printable (RecordDecl p an) where
  pprint (RecordDecl _ head' props)
    = head' <> printProps (map pprint props)

instance (Printable (GroupLoc p an)) => Printable (Primitive p an) where
  pprint (PrimInteger _ int) = pprint int
  pprint (PrimFloat _ float) = pprint float
  pprint (PrimString _ str) = pprint str

instance (Printable (GroupLoc p an)) => Printable (Record p an) where
  pprint (Record _ head' props)
    = head' <> "[" <> T.intercalate ", " (map pprint props) <> "]"

instance (Printable (GroupLoc p an)) => Printable (Bind p an) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance (Printable (GroupLoc p an)) => Printable (Value p an) where
  pprint (ValuePrimitive prim) = pprint prim
  pprint (ValueRecord record) = pprint record
  pprint (ValueBind bind) = pprint bind

instance (Printable (GroupLoc p an)) => Printable (GroupRef p an) where
  pprint (GroupRef _ loc vprops gprops)
    = pprint loc <> printProps (map pprint vprops ++ map pprint gprops)

instance (Printable (GroupLoc p an)) => Printable (Guard p an) where
  pprint (Guard _ input output nexts)
     = pprint input
    <> " <- "
    <> pprint output
    <> foldMap printNext nexts
    where printNext next' = " " <> pprint next'

instance (Printable (GroupLoc p an)) => Printable (Reducer p an) where
  pprint (Reducer _ (Guard _ input output nexts) guards)
     = pprint input
    <> " -> "
    <> pprint output
    <> foldMap printNext nexts
    <> foldMap printGuard guards
    <> ";"
    where printNext next' = " " <> pprint next'
          printGuard guard = ",\n  " <> pprint guard

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

-- | Specifies that a record declaration can take any number of properties.
varNumProps :: Int
varNumProps = (-1)

mkBuiltinDecl :: T.Text -> Int -> DeclCompact
mkBuiltinDecl head' numProps
  = DeclCompact
  { declCompactHead = head'
  , declCompactNumProps = numProps
  }

compactRecordDecl :: RecordDecl p an -> DeclCompact
compactRecordDecl (RecordDecl _ head' props)
  = DeclCompact
  { declCompactHead = head'
  , declCompactNumProps = length props
  }

builtinDecls :: DeclSet
builtinDecls
  = DeclSet
  { declSetRecords
      = S.fromList
      [ mkBuiltinDecl "T" varNumProps
      , mkBuiltinDecl "Unit" 0
      , mkBuiltinDecl "True" 0
      , mkBuiltinDecl "False" 0
      , mkBuiltinDecl "Nil" 0
      , mkBuiltinDecl "None" 0
      , mkBuiltinDecl "Some" 1
      , mkBuiltinDecl "Cons" 2
      , mkBuiltinDecl "Hole" 1
      ]
  , declSetFunctions = S.empty
  }

declSetToMap :: S.Set DeclCompact -> M.Map T.Text Int
declSetToMap = M.fromAscList . map declToTuple . S.toAscList
  where declToTuple (DeclCompact head' numProps) = (head', numProps)

desugarError :: Range -> T.Text -> Error
desugarError rng msg
  = addRangeToErr rng $ Error
  { errorStage = StageDesugar
  , errorRange = Nothing
  , errorMsg = msg
  }
