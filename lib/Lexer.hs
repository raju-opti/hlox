module Lexer(
  lex
) where
import Token
import Data.Map as Map

data LexError = LexError Int Int String
  deriving (Show)

data LexState = LexState Int Int String
  deriving (Show, Eq)

type Tokenizer = LexState -> (Maybe Token, LexState)

(||>):: Tokenizer -> Tokenizer -> Tokenizer
ta ||> tb = \state -> 
  let (taToken, taState) = ta state
  in case taToken of
    Just token -> (Just token, taState)
    Nothing -> tb state

singleCharTokenizer:: Tokenizer
singleCharTokenizer input = 
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
        Just token -> (Just token, LexState line (column + 1) cs)
        Nothing -> (Nothing, LexState line (column + 1) cs)


singleLookAheadTokenizer:: Char -> Char -> Token -> Token -> Tokenizer
singleLookAheadTokenizer c1 c2 token1 token2 (LexState line column input) = 
  case input of
    (c1:c2:cs) -> Just (token2, LexState line (column + 2) cs)
    (c1:cs) -> Just (token1, LexState line (column + 1) cs)
    _ -> (Nothing, LexState line column input)

bangTokenizer:: Tokenizer
bangTokenizer = singleLookAheadTokenizer '!' '='  Bang BangEqual

equalTokenizer:: Tokenizer
equalTokenizer = singleLookAheadTokenizer '=' '=' Equal EqualEqual

greaterTokenizer:: Tokenizer
greaterTokenizer = singleLookAheadTokenizer '>' '=' Greater GreaterEqual

lessTokenizer:: Tokenizer
lessTokenizer = singleLookAheadTokenizer '<' '=' Less LessEqual

slashTokenizer:: Tokenizer
slashTokenizer (LexState line column input) = 
  case input of
    ('/':'/':cs) -> 
      let (_, rest) = span (/= '\n') cs
      in (Nothing, LexState (line + 1) 1 rest)
    ('/':cs) -> (Just Slash, LexState line (column + 1) (cs)
    _ -> (Nothing, LexState line column input)

stringTokenizer:: Tokenizer
stringTokenizer (LexState line column input) = 
  case input of
    ('"':cs) -> 
      let (str, rest) = span (/= '"') cs
      in (Just (StringT str), LexState line (column + 2 + length str) rest)
    _ -> (Nothing, LexState line column input)

-- "Not implemented"
lex:: String -> Either [LexError] [TokenWithContext]
lex input = error 