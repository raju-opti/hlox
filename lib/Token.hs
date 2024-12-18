module Token where

-- enum TokenType {
--   // Single-character tokens.
--   LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
--   COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

--   // One or two character tokens.
--   BANG, BANG_EQUAL,
--   EQUAL, EQUAL_EQUAL,
--   GREATER, GREATER_EQUAL,
--   LESS, LESS_EQUAL,

--   // Literals.
--   IDENTIFIER, STRING, NUMBER,

--   // Keywords.
--   AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
--   PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

--   EOF
-- }

data Token = LeftParen 
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier String
  | StringToken String
  | NumberToken Double
  | And
  | Class
  | Else
  | FalseToken
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | TrueToken
  | Var
  | While
  | EOF
  deriving (Show, Eq)

data TokenWithContext = TokenWithContext {
                          tcToken :: Token,
                          tcLine :: Int,
                          tcColumn :: Int
                        }
            deriving (Show, Eq) 
