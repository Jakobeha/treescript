{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TreeScript.Ast.Core.Types.Value
  ( module TreeScript.Ast.Core.Types.Value
  )
where

import           TreeScript.Ast.Core.Types.BindEq
import           TreeScript.Ast.Core.Types.Stx
import           TreeScript.Misc
import           TreeScript.Plugin

import           Data.Binary
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           GHC.Generics

newtype TStxWrapper a an = TStxWrapper{ unTStxWrapper :: a }

data ValueTerm a where
  TType ::ValueTerm UType
  TValueStx ::StxTerm a -> ValueTerm (TStxWrapper a)
  TPrim ::ValueTerm Primitive
  TSymbol ::ValueTerm Symbol
  TRecord ::ValueTerm Record
  TBind ::ValueTerm Bind
  TValue ::ValueTerm Value

-- | The type of a primitive.
data PrimType
  = PrimTypeInteger
  | PrimTypeFloat
  | PrimTypeString
  | PrimTypeStx
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Whether the record is a regular or special type.
data RecordKind
  = RecordKindTuple
  | RecordKindList
  | RecordKindOpaque
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Type of a concrete value (no splices). An atom or product of atoms, specialized for TreeScript values.
data SType
  = STypePrim PrimType
  | STypeRecord (Symbol ())
  | STypeTuple [SType]
  | STypeList SType
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Type of an expected record property: a union of a fixed # of 'SType's, or "any" which is the union of all types.
data MType
  = MTypeAny
  | MType (S.Set SType)
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Type defined by the user, with annotations.
data UType an
  = UType
  { utypeAnn :: an
  , utype :: MType
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Whether a record has fixed or variable props
data PropsType
  = PropsTypeVarLen
  | PropsTypeFixed [MType]
  deriving (Eq, Ord, Read, Show, Generic, Binary)

-- | Raw backend code. Represents a number, string, etc. as well as an external function or splice. A leaf in the AST.
data Primitive an
  = PrimInteger an Int
  | PrimFloat an Float
  | PrimString an T.Text
  | PrimStx an LStxBlob
  deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

type ModulePath = T.Text

-- | An identifier, such as a record head or property key.
data Symbol an
  = Symbol
  { symbolAnn :: an
  , symbolModule :: ModulePath
  , symbol :: T.Text
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | Contains a head and properties. A parent in the AST.
data Record an
  = Record
  { recordAnn :: an
  , recordHead :: Symbol an
  , recordProps :: [Value an]
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | In an input value, assigns an index identifier to a value so it can be referenced later, and checks that if the identifier is already assigned the values match. If it's an output value, refers to the value already assigned the identifier. The identifier can be '0' in an input value, in which case the value is discarded, but not in an output value.
data Bind an
  = Bind
  { bindAnn :: an
  , bindIdx :: Int
  } deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

data Value an
  = ValuePrimitive (Primitive an)
  | ValueRecord (Record an)
  | ValueBind (Bind an)
  deriving (Eq, Ord, Read, Show, Generic, Binary, Functor, Foldable, Traversable, Generic1, Annotatable)

-- | @mappend == union@.
instance Semigroup MType where
  MTypeAny <> _        = MTypeAny
  _        <> MTypeAny = MTypeAny
-- TODO: Merge same length lists and tuples.
  MType xs <> MType ys = MType $ xs <> ys

-- | @mappend == union@.
instance Monoid MType where
  mempty = xBottom

instance BindEq (Symbol a) where
  x =$= y = remAnns x == remAnns y

instance BindEq (Primitive a) where
  x =$= y = remAnns x == remAnns y

instance BindEq (Value a) where
  ValuePrimitive (PrimStx _ x) =$= ValuePrimitive (PrimStx _ y) = x =$= y
  ValuePrimitive x =$= ValuePrimitive y = x =$= y
  ValueRecord (Record _ xhead xprops) =$= ValueRecord (Record _ yhead yprops) =
    xhead =$= yhead && and (zipWith (=$=) xprops yprops)
  ValueBind _ =$= _           = error "can't compare binds for bind equality"
  _           =$= ValueBind _ = error "can't compare binds for bind equality"
  _           =$= _           = False

instance Printable PrimType where
  pprint PrimTypeInteger = "int"
  pprint PrimTypeFloat   = "float"
  pprint PrimTypeString  = "string"
  pprint PrimTypeStx     = "stx"

instance Printable SType where
  pprint (STypePrim   x    ) = "@" <> pprint x
  pprint (STypeRecord name ) = "@" <> pprint name
  pprint (STypeTuple  props) = "@t" <> printProps (map pprint props)
  pprint (STypeList   elm  ) = "@l[" <> pprint elm <> "]"

instance Printable MType where
  pprint MTypeAny = "@any"
  pprint (MType parts) | null parts' = "@bottom"
                       | otherwise   = T.intercalate "|" $ map pprint parts'
    where parts' = S.toList parts

instance Printable (UType an) where
  pprint (UType _ typ) = pprint typ

instance Printable (Primitive an) where
  pprint (PrimInteger _ int  ) = pprint int
  pprint (PrimFloat   _ float) = pprint float
  pprint (PrimString  _ str  ) = pprint str
  pprint (PrimStx     _ stx  ) = pprint stx

instance Printable (Symbol an) where
  pprint (Symbol _ md txt) | md == ""  = txt
                           | otherwise = md <> "_" <> txt

instance Printable (Record an) where
  pprint (Record _ head' props) =
    pprint head' <> "[" <> T.intercalate ", " (map pprint props) <> "]"

instance Printable (Bind an) where
  pprint (Bind _ idx) = "\\" <> pprint idx

instance Printable (Value an) where
  pprint (ValuePrimitive prim  ) = pprint prim
  pprint (ValueRecord    record) = pprint record
  pprint (ValueBind      bind  ) = pprint bind

printProps :: [T.Text] -> T.Text
printProps props = "[" <> T.intercalate ", " props <> "]"

xBottom :: MType
xBottom = MType mempty

mType1 :: SType -> MType
mType1 part = MType [part]

m2sTypes :: MType -> Maybe [SType]
m2sTypes MTypeAny      = Nothing
m2sTypes (MType parts) = Just $ S.toList parts

mkTupleType :: [MType] -> MType
mkTupleType typs = case sequence <$> traverse m2sTypes typs of
  Nothing    -> MTypeAny
  Just props -> MType $ S.fromList $ map STypeTuple props

mkListType :: [MType] -> MType
mkListType typs = case concat <$> traverse m2sTypes typs of
  Nothing    -> MTypeAny
  Just props -> MType $ S.fromList $ map STypeList props

typesDisjoint :: MType -> MType -> Bool
typesDisjoint MTypeAny       _              = False
typesDisjoint _              MTypeAny       = False
typesDisjoint (MType xParts) (MType yParts) = S.disjoint xParts yParts

-- | Whether the record is opaque or what transparent type it is.
recordKind :: Symbol an -> RecordKind
recordKind sym | sym_ == mkBuiltinSymbol "T" = RecordKindTuple
               | sym_ == mkBuiltinSymbol "L" = RecordKindList
               | otherwise                   = RecordKindOpaque
  where sym_ = remAnns sym

mkBuiltinSymbol :: T.Text -> Symbol ()
mkBuiltinSymbol txt =
  Symbol { symbolAnn = (), symbolModule = "", symbol = txt }

mkLangSymbol :: T.Text -> Symbol ()
mkLangSymbol txt =
  Symbol { symbolAnn = (), symbolModule = "Lang", symbol = txt }

langHead :: an -> Language -> Maybe (Symbol an)
langHead _   LanguageStx = Nothing
langHead ann lang        = Just Symbol { symbolAnn    = ann
                                       , symbolModule = "Lang"
                                       , symbol       = languageName lang
                                       }

wrapLang :: an -> Language -> Value an -> Value an
wrapLang ann lang x = case langHead ann lang of
  Nothing    -> x
  Just head' -> ValueRecord Record { recordAnn   = getAnn x
                                   , recordHead  = head'
                                   , recordProps = [x]
                                   }

-- | Never returns StxLisp
unwrapLang :: Value an -> Maybe (Language, Value an)
unwrapLang (ValueRecord (Record _ (Symbol _ "Lang" name) [x])) =
  (, x) <$> langWithName name
unwrapLang _ = Nothing

value2Stx :: Value an -> Maybe StxBlob
value2Stx (ValuePrimitive (PrimStx _ (LStxBlob _ stx))) = Just stx
value2Stx _ = Nothing

stx2Value :: an -> StxBlob -> Value an
stx2Value ann = ValuePrimitive . PrimStx ann . LStxBlob LanguageStx

-- | If the value is a syntax blob, the # of terms. Otherwise 1.
valBlobLength :: Value an -> Int
valBlobLength val = case value2Stx val of
  Nothing           -> 1
  Just (StxBlob ns) -> length ns
