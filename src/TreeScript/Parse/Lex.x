-- Derived from https://github.com/simonmar/alex/blob/master/examples/haskell.x and https://github.com/simonmar/alex/blob/master/examples/tiger.x
{
{-# LANGUAGE OverloadedStrings #-}

module TreeScript.Parse.Lex
  ( parseLex
  ) where

import TreeScript.Ast
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
$asciiSymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:\,\;\`]
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
<0> \' { enterString LexQuoteSingle `andBegin` state_string }
<0> \" { enterString LexQuoteDouble `andBegin` state_string }
<0> \\ { enterString LexQuoteSplice `andBegin` state_string }

<0> $symbol { mkPunc } -- Always results in a parser error, but ok here
<0> \( { mkEnc (Enclosure EncTypeParen EncPlaceOpen) }
<0> \) { mkEnc (Enclosure EncTypeParen EncPlaceClose) }
<0> \[ { mkEnc (Enclosure EncTypeBrace EncPlaceOpen) }
<0> \] { mkEnc (Enclosure EncTypeBrace EncPlaceClose) }
<0> \{ { mkEnc (Enclosure EncTypeBracket EncPlaceOpen) }
<0> \} { mkEnc (Enclosure EncTypeBracket EncPlaceClose) }

<0> @integer { mkPrim (LexPrimInteger . T.parseInt) }
<0> @float { mkPrim (LexPrimFloat . T.parseFloat) }

<0> @lowerSymbol { mkSymbol SymbolCaseLower }
<0> @upperSymbol { mkSymbol SymbolCaseUpper }

<state_comment> \/ \* { nestBlockComment }
<state_comment> \* \/ { unnestBlockComment }
<state_comment> . ;
<state_comment> \n { skip }
<state_string> \' \' { addCharToString '\'' }
<state_string> \" \" { addCharToString '"' }
<state_string> \\ \\ { addCharToString '\\' }
<state_string> \' { exitString LexQuoteSingle `andBegin` initialState }
<state_string> \" { exitString LexQuoteDouble `andBegin` initialState }
<state_string> \\ { exitString LexQuoteSplice `andBegin` initialState }
<state_string> [. \n] { addCurrentToString }

{

type Action = AlexAction SLexeme
type SimpleAction = T.Text -> Alex Lexeme

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

inputStartLoc :: AlexInput -> Loc
inputStartLoc (pos, _, _, _) = convertLoc pos

inputText :: AlexInput -> Int64 -> T.Text
inputText (_, _, str, _) len
  = convertStr $ ByteString.take (fromIntegral len) str

inputEndLoc :: AlexInput -> Int64 -> Loc
inputEndLoc inp len = advanceLoc loc txt
  where loc = inputStartLoc inp
        txt = inputText inp len

inputAnn :: AlexInput -> Int64 -> SrcAnn
inputAnn inp len
  = SrcAnn
  { srcAnnRange = mkRange loc txt
  , srcAnnText = txt
  }
  where loc = inputStartLoc inp
        txt = inputText inp len

liftAction :: SimpleAction -> Action
liftAction action inp len = Annd ann <$> action (srcAnnText ann)
  where ann = inputAnn inp len

-- = States

initialState :: Int
initialState = 0

-- == User State Monad

data AlexUserState
  = AlexUserState
  { lexerCommentDepth  :: Int
  , lexerStringState :: Bool
  , lexerStringStartLoc :: Loc
  , lexerStringStart :: LexQuote
  , lexerStringValue :: String
  , lexerStringRaw :: String
  }

alexInitUserState :: AlexUserState
alexInitUserState
  = AlexUserState
  { lexerCommentDepth = 0
  , lexerStringState = False
  , lexerStringStartLoc = undefined
  , lexerStringStart = undefined
  , lexerStringValue = undefined
  , lexerStringRaw = undefined
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

getLexerStringState :: Alex Bool
getLexerStringState = getUserStateProp lexerStringState

setLexerStringState :: Bool -> Alex ()
setLexerStringState new = setUserStateProp $ \ust -> ust{ lexerStringState = new }

getLexerStringStartLoc :: Alex Loc
getLexerStringStartLoc = getUserStateProp lexerStringStartLoc

setLexerStringStartLoc :: Loc -> Alex ()
setLexerStringStartLoc new = setUserStateProp $ \ust -> ust{ lexerStringStartLoc = new }

getLexerStringStart :: Alex LexQuote
getLexerStringStart = getUserStateProp lexerStringStart

setLexerStringStart :: LexQuote -> Alex ()
setLexerStringStart new = setUserStateProp $ \ust -> ust{ lexerStringStart = new }

getLexerStringValue :: Alex String
getLexerStringValue = getUserStateProp lexerStringValue

setLexerStringValue :: String -> Alex ()
setLexerStringValue new = setUserStateProp $ \ust -> ust{ lexerStringValue = new }

getLexerStringRaw :: Alex String
getLexerStringRaw = getUserStateProp lexerStringRaw

setLexerStringRaw :: String -> Alex ()
setLexerStringRaw new = setUserStateProp $ \ust -> ust{ lexerStringRaw = new }

addCharToLexerString :: Char -> String -> Alex ()
addCharToLexerString vchr rhd = do
  vstr <- getLexerStringRaw
  rstr <- getLexerStringRaw
  setLexerStringValue $ vchr : vstr
  setLexerStringRaw $ rhd ++ rstr

-- = Lexeme Constructors

alexEOF :: Alex SLexeme
alexEOF = do
  inp <- alexGetInput
  let ann = SrcAnn{ srcAnnRange = singletonRange $ inputStartLoc inp, srcAnnText = "" }
  pure $ Annd ann LexemeEof

mkPunc :: Action
mkPunc = liftAction $ pure . LexemeAtom . AtomPunc . T.head

mkPrim :: (T.Text -> LexPrim) -> Action
mkPrim mk = liftAction $ pure . LexemeAtom . AtomPrim . mk

mkEnc :: Enclosure -> Action
mkEnc enc = liftAction $ \_ -> pure $ LexemeEnc enc

mkSymbol :: SymbolCase -> Action
mkSymbol cas = liftAction $ pure . LexemeAtom . AtomSymbol cas

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

enterString :: LexQuote -> Action
enterString squot inp len = do
  setLexerStringState True
  setLexerStringStartLoc $ inputStartLoc inp
  setLexerStringStart squot
  setLexerStringValue ""
  setLexerStringRaw $ reverse $ T.unpack $ inputText inp len
  alexMonadScan

addCharToStringDirect :: Char -> T.Text -> Alex SLexeme
addCharToStringDirect chr txt = do
  addCharToLexerString chr $ reverse $ T.unpack txt
  alexMonadScan

addCharToString :: Char -> Action
addCharToString chr inp len = addCharToStringDirect chr $ inputText inp len

addCurrentToString :: Action
addCurrentToString inp@(_, _, rst, _) len = addCharToStringDirect chr $ inputText inp len
  where chr
          | len == 1 = B.C.head rst
          | otherwise = error "addCurrentToString: not a single character"

exitString :: LexQuote -> Action
exitString equot inp len = do
  vtxt <- T.pack . reverse <$> getLexerStringValue
  rtxt <- T.pack . reverse <$> getLexerStringRaw
  setLexerStringState False
  startLoc <- getLexerStringStartLoc
  let endLoc = inputEndLoc inp len
  squot <- getLexerStringStart
  let rng = Range{ rangeStart = startLoc, rangeEnd = endLoc }
      ann = SrcAnn{ srcAnnRange = rng, srcAnnText = rtxt }
  pure $ Annd ann $ LexemeAtom $ AtomPrim $ LexPrimString squot vtxt equot

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
  lexerError $ "after " <> printLoc loc <> " - unexpected " <> inpDesc

scanner :: ByteString.ByteString -> Either String [SLexeme]
scanner str = runAlex str scanRest
  where scanRest = do
          (tok, errMsgOpt) <- alexComplementError alexMonadScan
          case errMsgOpt of
            Nothing -> pure ()
            Just _ -> unexpectedCharsError
          case tok of
            Annd _ LexemeEof -> do
              inString <- getLexerStringState
              inComment <- (/= 0) <$> getLexerCommentDepth
              case (inString, inComment) of
                (True, True) -> error "lexer in multiple states at once"
                (True, False) -> lexerError "string or code not closed at end of file"
                (False, True) -> lexerError "comment not closed at end of file"
                (False, False) -> pure [tok]
            _ -> do
              toks <- scanRest
              pure $ tok : toks

parseLex :: T.Text -> EResult (LexProgram SrcAnn)
parseLex str
  = case scanner $ T.L.encodeUtf8 $ T.L.fromStrict str of
         Left locAndErrMsg
           -> case reads locAndErrMsg of
                [(loc, errMsg)] -> ResultFail Error
                  { errorRange = Just $ singletonRange loc
                  , errorMsg = T.pack errMsg
                  }
                _ -> error $ "bad lexer error format: " ++ locAndErrMsg
         Right lexemes -> Result S.empty $ LexProgram lexemes
}
