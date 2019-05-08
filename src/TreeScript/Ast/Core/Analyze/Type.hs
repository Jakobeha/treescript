{-# LANGUAGE OverloadedLists #-}

module TreeScript.Ast.Core.Analyze.Type
  ( surfaceType
  , addCasts
  )
where

import           TreeScript.Ast.Core.Analyze.Misc
import           TreeScript.Ast.Core.Types
import           TreeScript.Misc

import           Control.Monad.Writer.Strict

type Typed an = WriterT [Cast an] GlobalSessionRes

primType :: Primitive an -> TypePart
primType (PrimInteger _ _) = TypePartPrim PrimTypeInteger
primType (PrimFloat   _ _) = TypePartPrim PrimTypeFloat
primType (PrimString  _ _) = TypePartPrim PrimTypeString

elemType :: [SType] -> XType
elemType [x, SType1 (TypePartList y)] = s2xType x <> y
elemType [_, STypeAny               ] = XTypeAny
-- Malformed
elemType _                            = xBottom

recType :: Record an -> TypePart
recType (Record _ head' props) = case recordKind head' of
  RecordKindTuple  -> TypePartTuple $ map (s2xType . surfaceType) props
  RecordKindNil    -> TypePartList xBottom
  RecordKindCons   -> TypePartList $ elemType $ map surfaceType props
  RecordKindOpaque -> TypePartRecord $ remAnns head'

surfaceType :: Value an -> SType
surfaceType (ValuePrimitive prim) = SType1 $ primType prim
surfaceType (ValueRecord    recd) = SType1 $ recType recd
surfaceType (ValueBind      _   ) = STypeAny

addChildCasts :: Record an -> Typed an ()
addChildCasts (Record _ _ props) = forM_ (zip [0 ..] props) $ \(idx, prop) ->
  censor (map $ \cast -> cast { castPath = idx : castPath cast })
    $ addValueCasts prop

addSurfaceCasts :: Record an -> Typed an ()
addSurfaceCasts (Record _ head' props) = do
  ptyps <- globalEnvLookup SymbolTypeRecord head' <$> getGlobalEnv
  case ptyps of
    -- Lookup failure (raised error in validation)
    Nothing              -> pure ()
    -- Children are all @any@, no casts.
    Just PropsTypeVarLen -> pure ()
    Just (PropsTypeFixed ptyps') ->
      -- Don't try to cast missing properties in malformed records
      forM_ (zip3 [0 ..] ptyps' props) $ \(idx, ptyp, prop) ->
        case mkCast (getAnn prop) [idx] ptyp of
        -- No need to cast.
          Nothing    -> pure ()
          Just pcast -> tell [pcast]

addRecordCasts :: Record an -> Typed an ()
addRecordCasts recd = do
  -- Order is important.
  addChildCasts recd
  addSurfaceCasts recd

addValueCasts :: Value an -> Typed an ()
addValueCasts (ValuePrimitive _   ) = pure ()
addValueCasts (ValueRecord    recd) = addRecordCasts recd
addValueCasts (ValueBind      _   ) = pure ()

addGuardCasts :: Guard an -> GlobalSessionRes (Guard an)
addGuardCasts (Guard rng inp out nexts) = do
  casts <- execWriterT $ addValueCasts out
  -- TODO Add casts for old next functions
  let nexts' = nexts ++ map NextCast casts
  pure $ Guard rng inp out nexts'

addCasts :: Program an -> GlobalSessionRes (Program an)
-- SOON Check if this is actually right
addCasts = traverseAst TProgram TGuard addGuardCasts
