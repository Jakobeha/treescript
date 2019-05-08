-- Derived from https://github.com/simonmar/alex/blob/master/examples/haskell.x and https://github.com/simonmar/alex/blob/master/examples/tiger.x
{
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Ast.Lex.Parse
  ( parse
  ) where

import TreeScript.Ast.Lex.Types
import TreeScript.Misc
import qualified TreeScript.Misc.Ext.Text as T

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B.C
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.L
import qualified Data.Text.Lazy.Encoding as T.L

}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$octit = 0-7
$hexit = [0-9 A-F a-f]
$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff]
$alpha = [$small $large]
$alphaNum = [$alpha $digit \_]
$asciiSymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:\(\)\,\;\[\]\`\{\}]
$unicodeSymbol = [] -- TODO
$symbol = [$asciiSymbol $unicodeSymbol]
$escapeChar = [abfnrtv\\\"\'\&]
$stringChar = [$white $digit $alpha $symbol]

@ellipsis = \. \. \.
@decimal = $digit+
@octal = $octit+
@hexadecimal = $hexit+
@integer = '-'? (@decimal | 0 [oO] @octal | 0 [xX] @hexadecimal)
@decimalSuffix = '.' $digit*
@exponentSuffix = [eE] '-'? @decimal
@float = '-'? @decimal @decimalSuffix? @exponentSuffix?
@lowerSymbol = $small $alphaNum*
@upperSymbol = $large $alphaNum*

$control = [$large \@\[\\\]\^\_]
@ascii = \^ $control | NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS
       | HT | LF | VT | FF | CR | SO | SI | DLE | DC1 | DC2 | DC3 | DC4
       | NAK | SYN | ETB | CAN | EM | SUB | ESC | FS | GS | RS | US
       | SP | DEL
@escape = \\ ($escapeChar | @ascii | @decimal | o @octal | x @hexadecimal)
@gap = \\ $white+ \\
@string = $stringChar | @escape | @gap

treescript :-

<0> $white+ { skip }
<0> \/ \/ .* { skip }

<0> \/ \* { enterBlockComment `andBegin` state_comment }
<0> \' { enterCodeBlock True `andBegin` state_code_block }

<0> @ellipsis { mkPunc PuncEllipsis } -- Always results in a parser error, but ok here
<0> \_ { mkPunc PuncUnderscore }
<0> \& { mkPunc PuncAnd }
<0> \# { mkPunc PuncHash }
<0> \\ { mkPunc PuncBackSlash }
<0> \@ { mkPunc PuncAt }
<0> \- \> { mkPunc PuncFwdArrow }
<0> \< \- { mkPunc PuncBwdArrow }
<0> \= \> { mkPunc PuncFwdEq }
<0> \| { mkPunc PuncVerticalBar }
<0> \. { mkPunc PuncPeriod }
<0> \; { mkPunc PuncSemicolon }
<0> \, { mkPunc PuncComma }
<0> \[ { mkPunc PuncOpenBracket }
<0> \] { mkPunc PuncCloseBracket }

<0> @integer { mkPrim (PrimInteger . mapAnnd T.parseInt) }
<0> @float { mkPrim (PrimFloat . mapAnnd T.parseFloat) }
<0> \" @string* \" { mkPrim (PrimString . mapAnnd (T.pack . read . T.unpack)) }

<0> @lowerSymbol { mkSymbol SymbolCaseLower }
<0> @upperSymbol { mkSymbol SymbolCaseUpper }

<state_comment> \/ \* { nestBlockComment }
<state_comment> \* \/ { unnestBlockComment }
<state_comment> . ;
<state_comment> \n { skip }
<state_code_block> \' { exitCodeBlock True `andBegin` initialState }
<state_code_block> \\ \' { addCharToCodeBlock '\'' }
<state_code_block> \\ \\ { addCharToCodeBlock '\\' }
<state_code_block> \\ { exitCodeBlock False `andBegin` state_code_block_bind }
<state_code_block> [. \n] { addCurrentToCodeBlock }
<state_code_block_bind> @ellipsis { mkPunc PuncEllipsis }
<state_code_block_bind> @lowerSymbol { exitCodeBlockSplice (\str -> LexemeSymbol $ Symbol str SymbolCaseLower) `andBegin` state_code_block }
<state_code_block_bind> @decimal { exitCodeBlockSplice (LexemePrim . PrimInteger . mapAnnd T.parseInt) `andBegin` state_code_block }
<state_code_block_bind> \_ { exitCodeBlockSplice (LexemePunc . PuncUnderscore . getAnn) `andBegin` state_code_block }

{

type Action = AlexAction (Lexeme Range)

-- = Conversion helpers

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

inputEndLoc :: AlexInput -> Int64 -> Loc
inputEndLoc inp len = advanceLoc loc str
  where (loc, str) = convertInput inp len

inputRange :: AlexInput -> Int64 -> Range
inputRange inp len = mkRange loc str
  where (loc, str) = convertInput inp len

-- = States

initialState :: Int
initialState = 0

-- == User State Monad

data AlexUserState
  = AlexUserState
  { lexerCommentDepth  :: Int
  , lexerCodeBlockState :: Bool
  , lexerCodeBlockStartLoc :: Loc
  , lexerCodeBlockStart :: Bool
  , lexerCodeBlockValue :: String
  }

alexInitUserState :: AlexUserState
alexInitUserState
  = AlexUserState
  { lexerCommentDepth = 0
  , lexerCodeBlockState = False
  , lexerCodeBlockStartLoc = undefined
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

getLexerCodeBlockStartLoc :: Alex Loc
getLexerCodeBlockStartLoc = getUserStateProp lexerCodeBlockStartLoc

setLexerCodeBlockStartLoc :: Loc -> Alex ()
setLexerCodeBlockStartLoc new = setUserStateProp $ \ust -> ust{ lexerCodeBlockStartLoc = new }

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

-- = Lexeme Constructors

alexEOF :: Alex (Lexeme Range)
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return $ LexemePunc $ PuncEof $ singletonRange $ convertLoc pos

mkPunc :: (Range -> Punc Range) -> Action
mkPunc punc inp len
  = return $ LexemePunc $ punc rng
  where rng = inputRange inp len

mkPrim :: (Annd T.Text Range -> Primitive Range) -> Action
mkPrim mk inp len
  = return $ LexemePrim $ mk $ Annd rng str
  where (loc, str) = convertInput inp len
        rng = mkRange loc str

mkSymbol :: SymbolCase -> Action
mkSymbol cas inp len
  = return $ LexemeSymbol Symbol
      { symbolCase = cas
      , symbolText = Annd rng str
      }
  where (loc, str) = convertInput inp len
        rng = mkRange loc str

-- == State-changing Constructors

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

enterCodeBlockDirect :: Bool -> Loc -> Alex ()
enterCodeBlockDirect isStart startLoc = do
  setLexerCodeBlockState True
  setLexerCodeBlockStartLoc startLoc
  setLexerCodeBlockStart isStart
  setLexerCodeBlockValue ""

enterCodeBlock :: Bool -> Action
enterCodeBlock isStart (pos, _, _, _) _ = do
  enterCodeBlockDirect isStart $ convertLoc pos
  alexMonadScan

addCharToCodeBlockDirect :: Char -> Alex (Lexeme Range)
addCharToCodeBlockDirect chr = do
  addCharToLexerCodeBlockValue chr
  alexMonadScan

addCharToCodeBlock :: Char -> Action
addCharToCodeBlock chr _ _ = addCharToCodeBlockDirect chr

addCurrentToCodeBlock :: Action
addCurrentToCodeBlock (_, _, str, _) len = addCharToCodeBlockDirect chr
  where chr
          | len == 1 = B.C.head str
          | otherwise = error "addCurrentToCodeBlock: not a single character"

exitCodeBlock :: Bool -> Action
exitCodeBlock isEnd inp len = do
  contentStr <- T.pack . reverse <$> getLexerCodeBlockValue
  setLexerCodeBlockState False
  startLoc <- getLexerCodeBlockStartLoc
  isStart <- getLexerCodeBlockStart
  let rng = Range{ rangeStart = startLoc, rangeEnd = inputEndLoc inp len }
  return $ LexemePrim $ PrimCode SpliceFrag
    { spliceFragStart = isStart
    , spliceFragEnd = isEnd
    , spliceFragContent = Annd rng contentStr
    }

exitCodeBlockSplice :: (Annd T.Text Range -> Lexeme Range) -> Action
exitCodeBlockSplice f inp len = do
  let (loc, str) = convertInput inp len
      rng = mkRange loc str
      sb = f $ Annd rng str
  enterCodeBlockDirect False $ rangeEnd rng
  return sb

-- = Execution

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
  | T.null inp' = "chars or end of file"
  | T.null inp'' = "end of line"
  | otherwise = "chars: " <> inp''
  where inp' = T.filter (/= '\r') $ T.takeWhile (/= '\n') $ convertStr inp
        inp''
          | T.length inp' > maxErrorInputLength = T.strip $ T.take maxErrorInputLength inp'
          | otherwise = T.strip inp'

lexerError :: T.Text -> Alex a
lexerError msg = do
  (pos, _, _, _) <- alexGetInput
  let loc = convertLoc pos
  alexError $ show loc ++ T.unpack msg

unexpectedCharsError :: Alex a
unexpectedCharsError = do
  (pos, _, inp, _) <- alexGetInput
  let inpDesc = getInputErrorDesc inp
      loc = convertLoc pos
  lexerError $ "after " <> pprint loc <> " - unexpected " <> inpDesc

scanner :: ByteString.ByteString -> Either String [Lexeme Range]
scanner str = runAlex str scanRest
  where scanRest = do
          (tok, errMsgOpt) <- alexComplementError alexMonadScan
          case errMsgOpt of
            Nothing -> pure ()
            Just _ -> unexpectedCharsError
          case tok of
            LexemePunc (PuncEof _) -> do
              inCodeBlock <- getLexerCodeBlockState
              inComment <- (/= 0) <$> getLexerCommentDepth
              case (inCodeBlock, inComment) of
                (True, True) -> error "lexer in multiple states at once"
                (True, False) -> lexerError "code block not closed at end of file"
                (False, True) -> lexerError "comment not closed at end of file"
                (False, False) -> return [tok]
            _ -> do
              toks <- scanRest
              return $ tok : toks

parse :: T.Text -> Result (Program Range)
parse str
  = case scanner $ T.L.encodeUtf8 $ T.L.fromStrict str of
         Left locAndErrMsg
           -> case reads locAndErrMsg of
                [(loc, errMsg)] -> ResultFail Error
                  { errorStage = StageLex
                  , errorRange = singletonRange loc
                  , errorMsg = T.pack errMsg
                  }
                _ -> error $ "bad lexer error format: " ++ locAndErrMsg
         Right lexemes -> Result S.empty $ Program $ Annd rng lexemes
           where rng = mkRange loc1 str
}
