module TreeScript.Ast.Core.TypeCast
  ( castCheckTypes
  ) where

import TreeScript.Ast.Core.Types
import TreeScript.Plugin

import Control.Monad.State.Strict
import qualified Data.Vector as V

{-
-- | Annotates a value with its type information.
data TypeAnn
  = TypeAnn
  { typeAnnExternal :: EType -- ^ Type based on context (e.g. containing record), expected type
  , typeAnnInternal :: IType -- ^ Type based on this value's structure (e.g. is this a prim?), actual type
  }

-- | External type.
type EType = Type TypeAnn

-- | Internal type.
data IType
  = ITypeReg EType
  | ITypeBind Int

data TypeCtx
  = TypeCtx
  { typeCtxBinds :: V.Vector EType
  }

type TypeEnv a = State TypeCtx a

primType :: Primitive t -> EType
primType (PrimInteger _) = mkSType $ TypePartPrim r0 $ PrimTypeInteger
primType (PrimFloat _) = mkSType $ TypePartPrim r0 $ PrimTypeFloat
primType (PrimString _) = mkSType $ TypePartPrim r0 $ PrimTypeString

recType :: Record t -> EType
recType (Record _ head' props)
  = case recordKind head' of
      RecordKindRegular -> mkSType $ TypePartRecord r0 head'

addTypeAnnsRec :: EType -> Record () -> Type (Record TypeAnn)
addTypeAnnsRec ext (Record rng head' props)
  = case recordKind head' of
      RecordKindRegular _ -> Record rng head' <$> traverse (addPropTypeAnns head') (zip props [0..])

addTypeAnns' :: EType -> Value () -> TypeEnv (Value TypeAnn)
addTypeAnns' ext (ValuePrimitive () prim) = pure $ ValuePrimitive typ prim
  where typ = TypeAnn ext $ ITypeReg $ primType prim
addTypeAnns' ext (ValueRecord () recd) = ValueRecord typ <$> addTypeAnnsRec ext recd
  where typ = TypeAnn ext $ ITypeReg $ recIType recd
addTypeAnns' ext (ValueBind () bnd) = do
  let typ = TypeAnn ext $ ITypeBind $ bindIdx bnd

  pure $ ValueBind typ bnd

addTypeAnns :: Value () -> TypeEnv (Value TypeAnn)
addTypeAnns = addTypeAnns' $ anyType r0
-}

castCheckTypes :: Program () -> GlobalSessionRes (Program ())
castCheckTypes = pure