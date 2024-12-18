module Lexer(
  scan,
  LexError(..)
) where
import Token
import Data.Map as Map
import Data.Char (isDigit)
import qualified Data.Foldable as F

data LexError = LexError Int Int String
  deriving (Show, Eq)

data LexState = LexState Int Int String
  deriving (Show, Eq)

type Scanner = LexState -> (Maybe (Either LexError Token), LexState)

(||>):: Scanner -> Scanner -> Scanner
ta ||> tb = \state ->
  let res@(_, taState) = ta state
  in if taState /= state
    then res
    else tb state

singleCharScanner:: Scanner
singleCharScanner state@(LexState line column input) =
  let tokens = Map.fromList [
              ('(', LeftParen),
              (')', RightParen),
              ('{', LeftBrace),
              ('}', RightBrace),
              (',', Comma),
              ('.', Dot),
              ('-', Minus),
              ('+', Plus),
              (';', Semicolon),
              ('*', Star)
            ]
  in case input of
    [] -> (Nothing, LexState line column input)
    (c:cs) ->
      case Map.lookup c tokens of
        Just token -> (Just (Right token), LexState line (column + 1) cs)
        Nothing -> (Nothing, state)


doubleLookAheadScanner:: Char -> Char -> Token -> Token -> Scanner
doubleLookAheadScanner c1 c2 token1 token2 state@(LexState line column input) =
  case input of
    (ca:cb:cs)
     | ca == c1 && cb == c2 -> (Just (Right token2), LexState line (column + 2) cs)
    (ca:cs)
     | ca == c1 -> (Just (Right token1), LexState line (column + 1) cs)
    _ -> (Nothing, state)

bangScanner:: Scanner
bangScanner = doubleLookAheadScanner '!' '='  Bang BangEqual

equalScanner:: Scanner
equalScanner = doubleLookAheadScanner '=' '=' Equal EqualEqual

greaterScanner:: Scanner
greaterScanner = doubleLookAheadScanner '>' '=' Greater GreaterEqual

lessScanner:: Scanner
lessScanner = doubleLookAheadScanner '<' '=' Less LessEqual

slashScanner:: Scanner
slashScanner state@(LexState line column input) =
  case input of
    ('/':'/':cs) ->
      let (c, rest) = span (/= '\n') cs
      in (Nothing, LexState line (column + length c) rest)
    ('/':cs) -> (Just (Right Slash), LexState line (column + 1) cs)
    _ -> (Nothing, state)

scanStringEnd:: LexState -> (Maybe String, LexState)
scanStringEnd state@(LexState line column input) =
  case input of
    ('"':cs) -> (Just "", LexState line (column + 1) cs)
    (c:cs) ->
      let nextState = if c == '\n'
                     then LexState (line + 1) 1 cs
                     else LexState line (column + 1) cs
          (str, newState) = scanStringEnd nextState
      in case str of
        Just s -> (Just (c:s), newState)
        Nothing -> (Nothing, newState)
    _ -> (Nothing, state)

stringScanner:: Scanner
stringScanner state@(LexState line column input) =
  case input of
    ('"':cs) ->
      let (str, newState) = scanStringEnd (LexState line (column + 1) cs)
      in case str of
        Just s -> (Just (Right (StringToken s)), newState)
        Nothing -> (Just (Left (LexError line column "unterminated string.")), newState)
    _ -> (Nothing, state)

numPrefix:: String -> (String, String)
numPrefix input =
  case input of
    ('.':c:cs)
      | isDigit c ->
        let (subNum, rest) = numPrefix cs
        in ('.' : c : subNum, rest)
    (c:cs)
      | isDigit c ->
        let (subNum, rest) = numPrefix cs
        in (c : subNum, rest)
    _ -> ([], input)

numberScanner:: Scanner
numberScanner state@(LexState line column input) =
  let (num, rest) = numPrefix input
  in case num of
    [] -> (Nothing, state)
    _ -> (Just (Right (NumberToken (read num))), LexState line (column + length num) rest)

keywords:: Map String Token
keywords = fromList [
    ("and", And),
    ("class", Class),
    ("else", Else),
    ("false", FalseToken),
    ("for", For),
    ("fun", Fun),
    ("if", If),
    ("nil", Nil),
    ("or", Or),
    ("print", Print),
    ("return", Return),
    ("super", Super),
    ("this", This),
    ("true", TrueToken),
    ("var", Var),
    ("while", While)
  ]

alphaNumericScanner:: Scanner
alphaNumericScanner state@(LexState line column input) =
  let (alphanum, rest) = span (\c -> c == '_' || isDigit c || c `elem` ['a'..'z'] || c `elem` ['A'..'Z']) input
  in case alphanum of
    [] -> (Nothing, state)
    _ -> let keyword = Map.lookup alphanum keywords
         in case keyword of
           Just token -> (Just (Right token), LexState line (column + length alphanum) rest)
           Nothing -> (Just (Right (Identifier alphanum)), LexState line (column + length alphanum) rest)

whitespaceScanner:: Scanner
whitespaceScanner (LexState line column input) =
  let (whitespace, rest) = span (\c -> c == ' ' || c == '\r' || c == '\t') input
  in (Nothing, LexState line (column + length whitespace) rest)

newlineScanner:: Scanner
newlineScanner state@(LexState line _ input) =
  case input of
    ('\n':cs) -> (Nothing, LexState (line + 1) 1 cs)
    _ -> (Nothing, state)

combinedScanner::Scanner
combinedScanner = F.foldr (||>) alphaNumericScanner [
    singleCharScanner,
    bangScanner,
    equalScanner,
    greaterScanner,
    lessScanner,
    slashScanner,
    stringScanner,
    numberScanner,
    whitespaceScanner,
    newlineScanner
  ]

scanForErrors :: LexState -> LexError -> [LexError]
scanForErrors (LexState _ _ []) err = [err]
scanForErrors state err =
  let res = doScan state
  in case res of
    Right _ -> [err]
    Left errors -> err : errors

scanForTokens :: LexState -> TokenWithContext -> Either [LexError] [TokenWithContext]
scanForTokens (LexState _ _ []) token = Right [token]
scanForTokens state token =
  let res = doScan state
  in case res of
    Right tokens -> Right (token : tokens)
    Left errors -> Left errors

doScan :: LexState -> Either [LexError] [TokenWithContext]
doScan (LexState _ _ []) = Right []

doScan state@(LexState line column (c:cs)) =
  let (res, newState) = combinedScanner state
  in if state == newState
     then let err = LexError line column ("unexpected character: " ++ [c])
          in Left (scanForErrors (LexState line (column + 1) cs) err)
     else case res of
        Nothing -> doScan newState
        Just (Left err) -> Left (scanForErrors newState err)
        Just (Right token) -> 
          let tokenWithContext = TokenWithContext token line column
          in scanForTokens newState tokenWithContext

scan:: String -> Either [LexError] [TokenWithContext]
scan input = doScan (LexState 1 1 input)
