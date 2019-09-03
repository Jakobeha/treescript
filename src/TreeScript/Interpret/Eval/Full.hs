{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Full (effectful) AST evaluation.
module TreeScript.Interpret.Eval.Full
  ( Eval
  , evalProgram
  )
where

import           TreeScript.Ast
import           TreeScript.Interpret.Eval.Abstract
import           TreeScript.Interpret.Abstract
import           TreeScript.Print
import           TreeScript.Misc

import           Control.Monad.IO.Class
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

-- | Full (effectful) evaluation monad.
type Eval = AbsEvalT IO

defaultBuiltins :: M.Map T.Text (BuiltinVal Eval)
defaultBuiltins = M.fromList
  [ ( "print"
    , BuiltinValClosure
      ["x"]
      (\[val] -> case val of
        ValPrimitive lit -> do
          liftIO $ T.putStrLn $ pprint lit
          pure $ ValPrimitive LitDataNull
        _ -> vabsurdVal =<< raiseErr "can't print this type of value"
      )
    )
  ]

evalProgram :: (EvalAnn r) => Program r -> EResultT IO ()
evalProgram = evalProgramWith defaultBuiltins
