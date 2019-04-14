{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the @Core@ AST.
module TreeScript.Ast.Core.Types
  ( module TreeScript.Ast.Core.Types
  ) where

import TreeScript.Misc

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics

-- | What reducers get from their parent group, or output values / groups from their reducer.
data LocalEnv
  = LocalEnv
  { localEnvBinds :: S.Set Int
  , localEnvGroups :: S.Set Int
  } deriving (Eq, Ord, Read, Show)

-- | Declares what nodes a language or library enables.
data DeclSet
  = DeclSet
  { declSetRecords :: S.Set (RecordDecl ())
  , declSetFunctions :: S.Set (FunctionDecl ())
  } deriving (Eq, Ord, Read, Show)

-- | A non-record type part.
data AtomType
  = AtomTypeAny
  | AtomTypeInteger
  | AtomTypeFloat
  | AtomTypeString
  deriving (Eq, Ord, Read, Show)

-- | Part of a value type (which is a union).
data TypePart an
  = TypePartAtom an AtomType
  | TypePartRecord an T.Text
  | TypePartTuple an [Type an]
  | TypePartList an (Type an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A value type.
data Type an
  = Type
  { typeAnn :: an
  , typeParts :: [TypePart an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a type of record.
data RecordDecl an
  = RecordDecl
  { recordDeclAnn :: an
  , recordDeclHead :: T.Text
  , recordDeclProps :: [Type an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Declares a function
data FunctionDecl an
  = FunctionDecl
  { functionDeclAnn :: an
  , functionDeclInput :: RecordDecl an
  , functionDeclOutput :: Type an
  } deriving (Eq, Ord, Read, Show)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Whether the record is a regular or special type.
data RecordType
  = RecordTypeTuple
  | RecordTypeCons
  | RecordTypeRegular T.Text
  deriving (Eq, Ord, Read, Show)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordHead :: T.Text
  , recordProps :: [Value an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindIdx :: Int
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Type of data in TreeScript.
data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | The type and identifier of a group.
data GroupLoc an
  = GroupLocGlobal an Int
  | GroupLocLocal an Int
  | GroupLocFunction an T.Text
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | References a group in a reducer clause. If in an input clause, it requires the group's reducers to match for the reducer to be applied. If in an output clause, the group's reducers get applied when the reducer gets applied.
data GroupRef an
  = GroupRef
  { groupRefAnn :: an
  , groupRefLoc :: GroupLoc an
  , groupRefValueProps :: [Value an]
  , groupRefGroupProps :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Matches a value against a different value. Like a "let" statement.
data Guard an
  = Guard
  { guardAnn :: an
  , guardInput :: Value an
  , guardOutput :: Value an
  , guardNexts :: [GroupRef an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Transforms a value into a different value. Like a case in a "match" statement.
data Reducer an
  = Reducer
  { reducerAnn :: an
  , reducerMain :: Guard an
  , reducerSubGuards :: [Guard an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Defines a group of statements, which can be referenced by other statements.
data GroupDef an
  = GroupDef
  { groupDefAnn :: an
  , groupDefValueProps :: [Bind an]
  , groupDefGroupProps :: [Bind an]
  , groupDefReducers :: [Reducer an]
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | A full TreeScript program.
data Program an
  = Program
  { programAnn :: an
  , programRecordDecls :: [RecordDecl an]
  , programCastReducers :: [Reducer an]
  , programGroups :: [GroupDef an]
  }

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

instance (Semigroup an) => Semigroup (Program an) where
  Program xAnn xDecls xCastReds xGroups <> Program yAnn yDecls yCastReds yGroups
    = Program
    { programAnn = xAnn <> yAnn
    , programRecordDecls = xDecls <> yDecls
    , programCastReducers = xCastReds <> yCastReds
    , programGroups = xGroups <> yGroups
    }

instance (Monoid an) => Monoid (Program an) where
  mempty
    = Program
    { programAnn = mempty
    , programRecordDecls = mempty
    , programCastReducers = mempty
    , programGroups = mempty
    }

instance Printable AtomType where
  pprint AtomTypeAny = "any"
  pprint AtomTypeInteger = "int"
  pprint AtomTypeFloat = "float"
  pprint AtomTypeString = "string"

instance Printable (TypePart an) where
  pprint (TypePartAtom _ x) = "@" <> pprint x
  pprint (TypePartRecord _ name) = "@" <> name
  pprint (TypePartTuple _ props) = "@t" <> printProps (map pprint props)
  pprint (TypePartList _ prop) = "@list[" <> pprint prop <> "]"

instance Printable (Type an) where
  pprint (Type _ parts)
    = T.intercalate "|" (map pprint parts)

instance Printable (RecordDecl an) where
  pprint (RecordDecl _ head' props)
    = head' <> printProps (map pprint props)

instance Printable (Primitive an) where
  pprint (PrimInteger _ int) = pprint int
  pprint (PrimFloat _ float) = pprint float
  pprint (PrimString _ str) = pprint str

instance Printable (Record an) where
  pprint (Record _ head' props)
    = head' <> "[" <> T.intercalate ", " (map pprint props) <> "]"

instance Printable (Bind an) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance Printable (Value an) where
  pprint (ValuePrimitive prim) = pprint prim
  pprint (ValueRecord record) = pprint record
  pprint (ValueBind bind) = pprint bind

instance Printable (GroupLoc an) where
  pprint (GroupLocGlobal _ idx) = "&+" <> pprint idx
  pprint (GroupLocLocal _ idx) = "&-" <> pprint idx
  pprint (GroupLocFunction _ txt) = "#" <> txt

instance Printable (GroupRef an) where
  pprint (GroupRef _ loc vprops gprops)
    = pprint loc <> printProps (map pprint vprops ++ map pprint gprops)

instance Printable (Guard an) where
  pprint (Guard _ input output nexts)
     = pprint input
    <> " <- "
    <> pprint output
    <> foldMap printNext nexts
    where printNext next' = " " <> pprint next'

instance Printable (Reducer an) where
  pprint = printReducer "->"

instance Printable (Program an) where
  pprint (Program _ decls castReds groups)
     = T.unlines $ map pprint decls
    ++ [T.empty]
    ++ map (printReducer "=>") castReds
    ++ [T.empty]
    ++ zipWith printGroupDef [0..] groups

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

printReducer :: T.Text -> Reducer an -> T.Text
printReducer typ (Reducer _ (Guard _ input output nexts) guards)
   = pprint input
  <> " "
  <> pprint typ
  <> " "
  <> pprint output
  <> foldMap printNext nexts
  <> foldMap printGuard guards
  <> ";"
  where printNext next' = " " <> pprint next'
        printGuard guard = ",\n  " <> pprint guard

printGroupDef :: Int -> GroupDef an -> T.Text
printGroupDef head' (GroupDef _ vprops gprops reds)
  = T.unlines $ printDecl : map pprint reds
  where printDecl
           = "&"
          <> pprint head'
          <> printProps (map pprint vprops ++ map printGroupDefProp gprops)
          <> "."
        printGroupDefProp (Bind _ idx) = "&-" <> pprint idx

localEnvInsertBinds :: S.Set Int -> LocalEnv -> LocalEnv
localEnvInsertBinds binds env
  = LocalEnv
  { localEnvBinds = binds <> localEnvBinds env
  , localEnvGroups = localEnvGroups env
  }

mkBuiltinDecl :: T.Text -> [Type ()] -> RecordDecl ()
mkBuiltinDecl head' props
  = RecordDecl
  { recordDeclAnn = ()
  , recordDeclHead = head'
  , recordDeclProps = props
  }

builtinDecls :: DeclSet
builtinDecls
  = DeclSet
  { declSetRecords
      = S.fromList
      [ mkBuiltinDecl "Unit" []
      , mkBuiltinDecl "True" []
      , mkBuiltinDecl "False" []
      , mkBuiltinDecl "None" []
      , mkBuiltinDecl "Nil" []
      , mkBuiltinDecl "Hole" [Type () [TypePartAtom () AtomTypeInteger]]
      ]
  , declSetFunctions = S.empty
  }

transparentDecls :: S.Set T.Text
transparentDecls = S.fromList ["T", "Cons"]

-- | Whether the record is regular or a special type.
recordType :: T.Text -> RecordType
recordType head'
  | head' == "T" = RecordTypeTuple
  | head' == "Cons" = RecordTypeCons
  | otherwise = RecordTypeRegular head'

declSetToMap :: S.Set (RecordDecl ()) -> M.Map T.Text [Type ()]
declSetToMap = M.fromAscList . map declToTuple . S.toAscList
  where declToTuple (RecordDecl () head' props) = (head', props)

hole :: an -> an -> Int -> Value an
hole ann idxAnn idx
  = ValueRecord Record
  { recordAnn = ann
  , recordHead = "Hole"
  , recordProps = [ValuePrimitive $ PrimInteger idxAnn idx]
  }

desugarError :: Range -> T.Text -> Error
desugarError rng msg
  = addRangeToErr rng $ Error
  { errorStage = StageDesugar
  , errorRange = Nothing
  , errorMsg = msg
  }
