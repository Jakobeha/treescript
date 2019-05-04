{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Core.TypeCast
  ( castCheckTypes
  )
where

import           TreeScript.Ast.Core.Misc
import           TreeScript.Misc
import           TreeScript.Plugin

import           Control.Monad.State.Strict
import           Data.List
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Vector                   as V

-- | Annotates a value with its type information.
data TypeAnn
  = TypeAnn
  { typeAnnExternal :: XType -- ^ Type based on context (e.g. containing record), expected type
  , typeAnnInternal :: IType -- ^ Type based on this value's structure (e.g. is this a prim?), actual type
  }

-- | Internal type.
data IType
  = ITypeReg ITypePart
  | ITypeBind Int
  deriving (Eq, Ord, Read, Show)

type ITypePart = TypePart (S.Set IType)

data TypeEnv
  = TypeEnv
  { typeEnvBinds :: V.Vector XType
  }

type Typed = StateT TypeEnv GlobalSessionRes

type PropTypes = Maybe (Maybe [XType])

primType :: Primitive TypeAnn -> ITypePart
primType (PrimInteger _ _) = TypePartPrim PrimTypeInteger
primType (PrimFloat   _ _) = TypePartPrim PrimTypeFloat
primType (PrimString  _ _) = TypePartPrim PrimTypeString

elemType :: [IType] -> S.Set IType
elemType [x, ITypeReg (TypePartList y)] = S.insert x y
-- reify
elemType [x, ITypeBind _              ] = [x]
-- Malformed - raised errors when typing props
elemType [x, _                        ] = [x]
elemType _                              = []

recType :: Record TypeAnn -> ITypePart
recType (Record _ head' props) = case recordKind head_ of
  RecordKindTuple ->
    TypePartTuple $ map (S.singleton . typeAnnInternal . getType) props
  RecordKindNil -> TypePartList []
  RecordKindCons ->
    TypePartList $ elemType $ map (typeAnnInternal . getType) props
  RecordKindOpaque _ -> TypePartRecord head_
  where head_ = pcast head'

assertNumProps :: Range -> RecordKind -> Int -> Int -> Typed ()
assertNumProps rng kind exp act
  | exp == act
  = pure ()
  | otherwise
  = tellError
    $  typeError rng
    $  "wrong number of props for "
    <> pprint kind
    <> ": expected "
    <> pprint exp
    <> ", got "
    <> pprint act

validateNumProps :: Range -> RecordKind -> PropTypes -> Int -> Typed ()
-- Failed lookup (raised error in previous phase)
validateNumProps _ (RecordKindOpaque _) Nothing        _ = pure ()
-- Transparent record
validateNumProps _ RecordKindTuple      (Just Nothing) _ = pure ()
validateNumProps rng RecordKindNil (Just Nothing) n =
  assertNumProps rng RecordKindNil 0 n
validateNumProps rng RecordKindCons (Just Nothing) n =
  assertNumProps rng RecordKindCons 2 n
validateNumProps rng kind@(RecordKindOpaque _) (Just (Just ptyps)) n =
  assertNumProps rng kind (length ptyps) n
validateNumProps _ _ _ _ =
  error "validateNumProps: unexpected combination of arguments"

propExtType :: RecordKind -> PropTypes -> Int -> XType
-- Failed lookup (raised error in previous phase)
propExtType (RecordKindOpaque _) Nothing        _ = XTypeAny
-- Transparent record - skips some prop type errors because they're caught by parent.
propExtType RecordKindTuple      (Just Nothing) _ = XTypeAny
propExtType RecordKindNil        (Just Nothing) _ = XTypeAny
propExtType RecordKindCons       (Just Nothing) 0 = XTypeAny
propExtType RecordKindCons (Just Nothing) 1 = xType1 $ TypePartList XTypeAny
-- Opaque record
propExtType (RecordKindOpaque _) (Just (Just typs)) idx
  |
  -- Raised error in 'validateNumProps'
    idx > length typs = XTypeAny
  | otherwise         = typs !! idx
propExtType _ _ _ = error "propExtType: unexpected combination of arguments"

addPropTypeAnns
  :: RecordKind -> PropTypes -> (Int, Value ()) -> Typed (Value TypeAnn)
addPropTypeAnns kind ptyps (idx, val) = addTypeAnns typ val
  where typ = propExtType kind ptyps idx

addTypeAnnsRec :: Record () -> Typed (Record TypeAnn)
addTypeAnnsRec (Record rng head' props) = do
  let head'' = pcast head'
      kind   = recordKind head''
      nprops = length props
  ptyps <-
    fmap (fmap $ map utype)
    .   globalEnvLookup SymbolTypeRecord head''
    <$> getGlobalEnv
  validateNumProps rng kind ptyps nprops
  Record rng head'' <$> traverse (addPropTypeAnns kind ptyps) (zip [0 ..] props)
  where head'' = pcast head'

addTypeAnns :: XType -> Value () -> Typed (Value TypeAnn)
addTypeAnns ext (ValuePrimitive () prim) = pure $ ValuePrimitive typ prim'
 where
  typ   = TypeAnn ext $ ITypeReg $ primType prim'
  prim' = pcast prim
addTypeAnns ext (ValueRecord () recd) = do
  recd' <- addTypeAnnsRec recd
  let typ = TypeAnn ext $ ITypeReg $ recType recd'
  pure $ ValueRecord typ recd'
addTypeAnns ext (ValueBind () bnd) = pure $ ValueBind typ bnd'
 where
  typ  = TypeAnn ext $ ITypeBind $ bindIdx bnd'
  bnd' = pcast bnd

-- | TODO: Fix union types with duplicates / @any@.
castCheckTypes :: Program () -> GlobalSessionRes (Program ())
castCheckTypes = pure
