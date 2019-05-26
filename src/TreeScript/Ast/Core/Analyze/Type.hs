{-# LANGUAGE OverloadedLists #-}

module TreeScript.Ast.Core.Analyze.Type
  ( valueType
  , addCasts
  )
where

import           TreeScript.Ast.Core.Analyze.Misc
import           TreeScript.Ast.Core.Types
import           TreeScript.Misc

import           Control.Monad.Writer.Strict

type Typed an = WriterT [Cast an] GlobalSessionRes

primType :: Primitive an -> SType
primType (PrimInteger _ _) = STypePrim PrimTypeInteger
primType (PrimFloat   _ _) = STypePrim PrimTypeFloat
primType (PrimString  _ _) = STypePrim PrimTypeString

recType :: Record an -> Maybe SType
recType (Record _ head' props) = case recordKind head' of
  RecordKindTuple -> STypeTuple <$> traverse valueType props
  RecordKindCons  -> case props of
    -- Malformed
    []      -> Just $ STypeRecord $ remAnns head'
    (x : _) -> STypeCons <$> valueType x
  RecordKindICons -> case props of
    -- Malformed
    []      -> Just $ STypeRecord $ remAnns head'
    (x : _) -> STypeICons <$> valueType x
  RecordKindOpaque -> Just $ STypeRecord $ remAnns head'

valueType :: Value an -> Maybe SType
valueType (ValuePrimitive prim) = Just $ primType prim
valueType (ValueRecord    recd) = recType recd
valueType (ValueBind      _   ) = Nothing

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
  let nexts' = map NextCast casts ++ nexts
  pure $ Guard rng inp out nexts'

addCasts :: Program an -> GlobalSessionRes (Program an)
addCasts = traverseAst TProgram TGuard addGuardCasts
