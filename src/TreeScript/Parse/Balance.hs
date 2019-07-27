{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Parse.Balance
  ( parseBalance
  )
where

import           TreeScript.Ast
import           TreeScript.Misc

import           Control.Monad
import           Data.Maybe

data EncInter = EncInter EncType SrcAnn [SBalance]

data EncInterStack
  = EncInterStack
  { encInterStackBase :: [SBalance]
  , encInterStackEncs :: [EncInter]
  }

encWarn :: Enclosure -> SrcAnn -> Error
encWarn enc ann = Error { errorRange = Just $ srcAnnRange ann
                        , errorMsg = "unmatched '" <> printEnclosure enc <> "'"
                        }

completeEnc :: EncInter -> SrcAnn -> SBalance
completeEnc (EncInter otyp oann rxs) cann = Annd ann $ BalanceEnc otyp xs
 where
  ann = fromJust $ Just oann <> foldMap (Just . anndAnn) xs <> Just cann
  xs  = reverse rxs

emptyEncInterStack :: EncInterStack
emptyEncInterStack =
  EncInterStack { encInterStackBase = [], encInterStackEncs = [] }

addEncEnter :: EncType -> SrcAnn -> EncInterStack -> EResult EncInterStack
addEncEnter typ ann (EncInterStack base encs) =
  pure $ EncInterStack base $ EncInter typ ann [] : encs

addEncExit :: EncType -> SrcAnn -> EncInterStack -> EResult EncInterStack
addEncExit ctyp cann (EncInterStack base (ei@(EncInter otyp _ _) : eis))
  | ctyp == otyp = addNonEnc (completeEnc ei cann) $ EncInterStack base eis
addEncExit ctyp cann stk = do
  tellError $ encWarn (Enclosure ctyp EncPlaceClose) cann
  pure stk

addNonEnc :: SBalance -> EncInterStack -> EResult EncInterStack
addNonEnc x (EncInterStack base []) = pure $ EncInterStack (x : base) []
addNonEnc x (EncInterStack base (EncInter typ rng xs : eis)) =
  pure $ EncInterStack base $ EncInter typ rng (x : xs) : eis

parseBalance :: LexProgram SrcAnn -> EResult (BalanceProgram SrcAnn)
parseBalance =
  fmap (BalanceProgram . reverse . encInterStackBase)
    . foldM (flip op) emptyEncInterStack
    . unLexProgram
 where
  op :: SLexeme -> EncInterStack -> EResult EncInterStack
  op (Annd ann LexemeEof) = warnRemainingParens ann -- eof is removed
  op (Annd ann (LexemeEnc (Enclosure typ EncPlaceOpen))) = addEncEnter typ ann
  op (Annd ann (LexemeEnc (Enclosure typ EncPlaceClose))) = addEncExit typ ann
  op (Annd ann (LexemeAtom atm)) = addNonEnc $ Annd ann $ BalanceAtom atm
  warnRemainingParens :: SrcAnn -> EncInterStack -> EResult EncInterStack
  warnRemainingParens cann (EncInterStack base reis) = do
    let leis = reverse reis
    tellErrors $ map
      (\(EncInter typ ann _) -> encWarn (Enclosure typ EncPlaceOpen) ann)
      leis
    pure $ EncInterStack (base ++ map (`completeEnc` cann) leis) $ error
      "encountered eof so should be done"
