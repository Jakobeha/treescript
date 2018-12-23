-- Derived from https://github.com/simonmar/alex/blob/master/examples/haskell.x and https://github.com/simonmar/alex/blob/master/examples/tiger.x
{
{-# LANGUAGE OverloadedStrings #-}

module Descript.Ast.Lex.Parse
  ( parse
  ) where

import Descript.Ast.Lex.Types
import Descript.Misc
import qualified Descript.Misc.Ext.Text as T

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B.C
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.L
import qualified Data.Text.Lazy.Encoding as T.L

}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$octit = 0-7
$hexit = [0-9 A-F a-f]
$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha = [$small $large]
$asciiSymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:\(\)\,\;\[\]\`\{\}]
$unicodeSymbol = [] -- TODO
$symbol = [$asciiSymbol $unicodeSymbol]
$escapeChar = [abfnrtv\\\"\'\&\$]
$stringChar = [$white $digit $alpha $symbol]

@decimal = $digit+
@octal = $octit+
@hexadecimal = $hexit+
@integer = '-'? (@decimal | [oO] @octal | [xX] @hexadecimal)
@decimalSuffix = '.' $digit*
@exponentSuffix = [eE] '-'? @decimal
@float = '-'? @decimal @decimalSuffix? @exponentSuffix?
@lowerSymbol = $small $alpha*
@upperSymbol = $large $alpha*

$control = [$large \@\[\\\]\^\_]
@ascii = \^ $control | NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS
       | HT | LF | VT | FF | CR | SO | SI | DLE | DC1 | DC2 | DC3 | DC4
       | NAK | SYN | ETB | CAN | EM | SUB | ESC | FS | GS | RS | US
       | SP | DEL
@escape = \\ ($escapeChar | @ascii | @decimal | o @octal | x @hexadecimal)
@gap = \\ $white+ \\
@string = $stringChar | @escape | @gap

descript :-

<0> $white+ { skip }
<0> \/ \/ .* { skip }

<0> \/ \* { enterBlockComment `andBegin` state_comment }
<0> \' { enterCodeBlock True `andBegin` state_code_block }
<0> \) { enterCodeBlock False `andBegin` state_code_block }

<0> \< { mkPunc PuncAngleBwd }
<0> \> { mkPunc PuncAngleFwd }
<0> \? { mkPunc PuncQuestion }
<0> : { mkPunc PuncColon }
<0> = { mkPunc PuncEqual }
<0> \. { mkPunc PuncPeriod }
<0> \; { mkPunc PuncSemicolon }
<0> \[ { mkPunc PuncOpenBracket }
<0> \] { mkPunc PuncCloseBracket }

<0> @integer { mkPrim (PrimInteger . T.parseInt) }
<0> @float { mkPrim (PrimFloat . T.parseFloat) }
<0> \" @string \" { mkPrim (PrimString . T.unescapeString) }

<0> @lowerSymbol { mkSymbol SymbolCaseLower }
<0> @upperSymbol { mkSymbol SymbolCaseUpper }

<state_comment> \/ \* { nestBlockComment }
<state_comment> \* \/ { unnestBlockComment }
<state_comment> . ;
<state_comment> \n { skip }
<state_code_block> @escape { addEscapeToCodeBlock }
<state_code_block> \' { exitCodeBlock True `andBegin` initialState }
<state_code_block> \\ \( { exitCodeBlock False `andBegin` initialState }
<state_code_block> \\ { \_ _ -> lexerError "illegal escape sequence" }
<state_code_block> [. \n] { addCurrentToCodeBlock }

{

type Action = AlexAction AnnLexeme

-- ** Conversion helpers

convertLoc :: AlexPosn -> Loc
convertLoc (AlexPn off line col)
  = Loc
  { locOffset = off
  , locLine = line
  , locColumn = col
  }

convertStr :: ByteString.ByteString -> T.Text
convertStr = T.L.toStrict . T.L.decodeUtf8

convertInput :: AlexInput -> Int64 -> (Loc, T.Text)
convertInput (pos, _, str, _) len
  = (convertLoc pos, convertStr $ ByteString.take (fromIntegral len) str)

-- ** States

initialState :: Int
initialState = 0

-- *** User State Monad

data AlexUserState
  = AlexUserState
  { lexerCommentDepth  :: Int
  , lexerCodeBlockState :: Bool
  , lexerCodeBlockStart :: Bool
  , lexerCodeBlockValue :: String
  }

alexInitUserState :: AlexUserState
alexInitUserState
  = AlexUserState
  { lexerCommentDepth = 0
  , lexerCodeBlockState = False
  , lexerCodeBlockStart = undefined
  , lexerCodeBlockValue = undefined
  }

getUserStateProp :: (AlexUserState -> a) -> Alex a
getUserStateProp get = Alex getUserStateProp'
  where getUserStateProp' st = Right (st, get $ alex_ust st)

setUserStateProp :: (AlexUserState -> AlexUserState) -> Alex ()
setUserStateProp set = Alex setUserStateProp'
  where setUserStateProp' st = Right (st{ alex_ust = set (alex_ust st) }, ())

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = getUserStateProp lexerCommentDepth

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth new = setUserStateProp $ \ust -> ust{ lexerCommentDepth = new }

getLexerCodeBlockState :: Alex Bool
getLexerCodeBlockState = getUserStateProp lexerCodeBlockState

setLexerCodeBlockState :: Bool -> Alex ()
setLexerCodeBlockState new = setUserStateProp $ \ust -> ust{ lexerCodeBlockState = new }

getLexerCodeBlockStart :: Alex Bool
getLexerCodeBlockStart = getUserStateProp lexerCodeBlockStart

setLexerCodeBlockStart :: Bool -> Alex ()
setLexerCodeBlockStart new = setUserStateProp $ \ust -> ust{ lexerCodeBlockStart = new }

getLexerCodeBlockValue :: Alex String
getLexerCodeBlockValue = getUserStateProp lexerCodeBlockValue

setLexerCodeBlockValue :: String -> Alex ()
setLexerCodeBlockValue new = setUserStateProp $ \ust -> ust{ lexerCodeBlockValue = new }

addCharToLexerCodeBlockValue :: Char -> Alex ()
addCharToLexerCodeBlockValue chr = do
  str <- getLexerCodeBlockValue
  setLexerCodeBlockValue $ chr : str

-- ** Lexeme Constructors

alexEOF :: Alex AnnLexeme
alexEOF
  = return AnnLexeme
  { annLexemeRange = undefined
  , annLexeme = LexemePunc $ PuncEof
  }

mkPunc :: Punc -> Action
mkPunc punc inp len
  = return AnnLexeme
  { annLexemeRange = mkRange loc str
  , annLexeme = LexemePunc punc
  }
  where (loc, str) = convertInput inp len

mkPrim :: (T.Text -> Prim) -> Action
mkPrim mk inp len
  = return AnnLexeme
  { annLexemeRange = mkRange loc str
  , annLexeme = LexemePrim (mk str)
  }
  where (loc, str) = convertInput inp len

mkSymbol :: SymbolCase -> Action
mkSymbol cas inp len
  = return AnnLexeme
  { annLexemeRange = mkRange loc str
  , annLexeme
      = LexemeSymbol Symbol
      { symbolCase = cas
      , symbolText = str
      }
  }
  where (loc, str) = convertInput inp len

-- *** State-changing Constructors

enterBlockComment :: Action
enterBlockComment input len = do
  setLexerCommentDepth 1
  skip input len

nestBlockComment :: Action
nestBlockComment input len = do
  depth <- getLexerCommentDepth
  setLexerCommentDepth $ depth + 1
  skip input len

unnestBlockComment :: Action
unnestBlockComment input len = do
  depth <- getLexerCommentDepth
  setLexerCommentDepth $ depth - 1
  when (depth == 1) $ alexSetStartCode initialState
  skip input len

enterCodeBlock :: Bool -> Action
enterCodeBlock isStart _ _ = do
  setLexerCodeBlockState True
  setLexerCodeBlockStart isStart
  setLexerCodeBlockValue ""
  alexMonadScan

addCharToCodeBlock :: Char -> Alex AnnLexeme
addCharToCodeBlock chr = do
  addCharToLexerCodeBlockValue chr
  alexMonadScan

addCurrentToCodeBlock :: Action
addCurrentToCodeBlock (_, _, str, _) len = addCharToCodeBlock chr
  where chr
          | len == 1 = B.C.head str
          | otherwise = error "addCurrentToCodeBlock: not a single character"

addEscapeToCodeBlock :: Action
addEscapeToCodeBlock (_, _, strAll, _) len = addCharToCodeBlock $ T.unescapeChar str
  where str = convertStr $ ByteString.take (fromIntegral len) strAll

exitCodeBlock :: Bool -> Action
exitCodeBlock isEnd inp len = do
  contentStr <- T.pack . reverse <$> getLexerCodeBlockValue
  setLexerCodeBlockState False
  isStart <- getLexerCodeBlockStart
  let (loc, rawStr) = convertInput inp len
  return AnnLexeme
    { annLexemeRange = mkRange loc rawStr
    , annLexeme
        = LexemePrim $ PrimCode $ SpliceFrag
        { spliceFragStart = isStart
        , spliceFragEnd = isEnd
        , spliceFragContent = contentStr
        }
    }

-- ** Execution

alexComplementError :: Alex a -> Alex (a, Maybe T.Text)
alexComplementError (Alex al) = Alex al'
  where al' st
          = case al st of
              Left errMsg -> Right (st, (undefined, Just $ T.pack errMsg))
              Right (st', x) -> Right (st', (x, Nothing))

maxErrorInputLength :: Int
maxErrorInputLength = 32 -- TODO Make a user setting?

getInputErrorDesc :: ByteString.ByteString -> T.Text
getInputErrorDesc inp
  | T.null inp' = "end of file"
  | T.null inp'' = "end of line"
  | otherwise = "chars \"" <> inp'' <> "\""
  where inp' = T.filter (/= '\r') $ T.takeWhile (/= '\n') $ convertStr inp
        inp''
          | T.length inp' > maxErrorInputLength = T.strip $ T.take maxErrorInputLength inp'
          | otherwise = T.strip inp'

lexerError :: T.Text -> Alex a
lexerError msg = do
  (pos, chr, inp, _) <- alexGetInput
  let loc = convertLoc pos
      inpDesc = getInputErrorDesc inp
  alexError $ T.unpack $ msg <> " at " <> pprint loc <> " on char " <> pprint chr <> " before " <> inpDesc

unexpectedCharsError :: Alex a
unexpectedCharsError = do
  (pos, _, inp, _) <- alexGetInput
  let loc = convertLoc pos
      inpDesc = getInputErrorDesc inp
  alexError $ T.unpack $ "after " <> pprint loc <> " - unexpected " <> inpDesc

scanner :: ByteString.ByteString -> Either String [AnnLexeme]
scanner str = runAlex str scanRest
  where scanRest = do
          (tok, errMsgOpt) <- alexComplementError alexMonadScan
          case errMsgOpt of
            Nothing -> pure ()
            Just _ -> unexpectedCharsError
          if (annLexeme tok == LexemePunc PuncEof) then do
            inCodeBlock <- getLexerCodeBlockState
            inComment <- (/= 0) <$> getLexerCommentDepth
            case (inCodeBlock, inComment) of
              (True, True) -> error "lexer in multiple states at once"
              (True, False) -> alexError "code block not closed at end of file"
              (False, True) -> alexError "comment not closed at end of file"
              (False, False) -> return [tok]
          else do
            toks <- scanRest
            return (tok : toks)

parse :: T.Text -> Result Program
parse str
  = case scanner $ T.L.encodeUtf8 $ T.L.fromStrict str of
         Left errMsg
           -> ResultFail Error
            { errorStage = StageLexing
            , errorMsg = T.pack errMsg
            }
         Right lexemes -> ResultSuccess $ Program lexemes
}
