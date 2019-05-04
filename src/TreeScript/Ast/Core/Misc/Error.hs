module TreeScript.Ast.Core.Misc.Error
  ( module TreeScript.Ast.Core.Misc.Error
  )
where

import           TreeScript.Misc

import qualified Data.Text                     as T

desugarError_ :: T.Text -> Error
desugarError_ msg =
  Error { errorStage = StageDesugar, errorRange = r0, errorMsg = msg }

desugarError :: Range -> T.Text -> Error
desugarError rng = addRangeToErr rng . desugarError_

validateError_ :: T.Text -> Error
validateError_ msg =
  Error { errorStage = StageValidate, errorRange = r0, errorMsg = msg }

validateError :: Range -> T.Text -> Error
validateError rng = addRangeToErr rng . validateError_

typeError_ :: T.Text -> Error
typeError_ msg =
  Error { errorStage = StageType, errorRange = r0, errorMsg = msg }

typeError :: Range -> T.Text -> Error
typeError rng = addRangeToErr rng . typeError_
