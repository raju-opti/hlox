{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module Parser where
import Token
import Control.Monad
import Data.Maybe
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Ast
import Text.ParserCombinators.ReadPrec (reset)

data ParserError = ParserError (Maybe TokenWithContext) String
  deriving (Eq)

instance Show ParserError where
  show (ParserError (Just (TokenWithContext _ line column)) message) = "Syntax Error: " ++ message ++ " at line " ++ show line ++ " column " ++ show column
  show (ParserError Nothing message) = "Syntax Error: " ++ message

type ParseFn a = [TokenWithContext] -> (Either ParserError a, [TokenWithContext])

newtype Parser a = Parser {
            runParse :: ParseFn a
          }

noMatch :: [TokenWithContext] -> String -> ParserError
noMatch tokens = ParserError (listToMaybe tokens)

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap


instance Monad Parser where
  return :: a -> Parser a
  return a = Parser (Right a,)

  (>>=) parser f = Parser $ \tokens ->
    let (result, tokens') = runParse parser tokens
    in case result of
      Left err -> (Left err, tokens')
      Right a -> runParse (f a) tokens'


instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance MonadPlus Parser where
  mzero = Parser $ \tokens -> (Left $ noMatch tokens "Failed to parse", tokens)

  -- mplus p1 p2 = Parser $ \tokens ->
  --   let result@(value, rest) = runParse p1 tokens
  --   in case value of
  --     Right _ -> result
  --     Left _ 
  --       | rest /= tokens -> result
  --       | otherwise -> let result'@(value', rest') = runParse p2 tokens in
  --         case value' of
  --           Right _ -> result'
  --           Left _ 
  --             | rest' /= tokens -> result'
  --             | otherwise -> (Left $ noMatch tokens "Failed to parse", tokens)

  mplus p1 p2 = Parser $ \tokens ->
    let result@(value, rest) = runParse p1 tokens
    in case value of
      Right _ -> result
      Left _ 
        | rest /= tokens -> result
        | otherwise -> runParse p2 tokens


  -- mplus p1 p2 = Parser $ \tokens ->
  --   let result@(_, rest) = runParse p1 tokens
  --   in if rest /= tokens
  --     then result
  --     else let result'@(_, rest') = runParse p2 tokens in
  --       if rest' /= tokens
  --         then result'
  --         else (Left $ noMatch tokens "Failed to parse", tokens)


expressionParser :: Parser Expression
expressionParser = equalityParser

noConsumptionOnFailure :: Parser a -> Parser a
noConsumptionOnFailure parser = Parser $ \tokens ->
  let (result, tokens') = runParse parser tokens
  in case result of
    Left _ -> (result, tokens)
    Right _ -> (result, tokens')

repeatParser' :: Parser a -> Parser [a]
repeatParser' parser = let repeatParse = do
                            a <- parser
                            as <- repeatParse <|> return []
                            return (a:as)
                        in repeatParse

repeatParser :: Parser a -> Parser [a]
repeatParser parser = repeatParser' parser <|> return []


leftAssociativeParser :: Parser Expression -> Parser TokenWithContext -> Parser Expression
leftAssociativeParser expParser operatorParser = let subParser = repeatParser $ do
                                                                    operator <- operatorParser
                                                                    expression <- expParser
                                                                    return (operator, expression)
                                                in do
                                                  expression <- expParser
                                                  return expression
                                                  foldl (\acc (op, expr) -> Binary acc op expr) expression <$> subParser

equalityParser :: Parser Expression
equalityParser = leftAssociativeParser comparisonParser (tokenParser (== BangEqual) <|> tokenParser (== EqualEqual))

comparisonParser :: Parser Expression
comparisonParser = leftAssociativeParser termParser (tokenParser (== Greater) <|> tokenParser (== GreaterEqual) <|> tokenParser (== Less) <|> tokenParser (== LessEqual))

termParser :: Parser Expression
termParser = leftAssociativeParser factorParser (tokenParser (== Minus) <|> tokenParser (== Plus))

factorParser :: Parser Expression
factorParser = leftAssociativeParser unaryParser (tokenParser (== Star) <|> tokenParser (== Slash))

parseToken:: (Token -> Bool)-> ParseFn TokenWithContext
parseToken fn (t:rest)
  | fn (tcToken t) = (Right t, rest)
parseToken _ input = (Left $ ParserError Nothing "Expected token", input)

parseToken':: (Token -> Bool)-> String -> ParseFn TokenWithContext
parseToken' fn _ (t:rest)
  | fn (tcToken t) = (Right t, rest)
parseToken' _ msg input@(t:_) = (Left $ ParserError (Just t) msg, input)
parseToken' _ msg input = (Left $ ParserError Nothing msg, input)

tokenParser :: (Token -> Bool) -> Parser TokenWithContext
tokenParser = Parser . parseToken

tokenParser' :: (Token -> Bool) -> String -> Parser TokenWithContext
tokenParser' fn = Parser . parseToken' fn

unaryParser :: Parser Expression
unaryParser = let parser =
                    let operatorParser = tokenParser (== Bang) <|> tokenParser (== Minus)
                    in do
                      operator <- operatorParser
                      Unary operator <$> unaryParser
              in parser <|> primaryParser

isNumberToken :: Token -> Bool
isNumberToken (NumberToken _) = True
isNumberToken _ = False

isStringToken :: Token -> Bool
isStringToken (StringToken _) = True
isStringToken _ = False

isTrueToken :: Token -> Bool
isTrueToken TrueToken = True
isTrueToken _ = False

isFalseToken :: Token -> Bool
isFalseToken FalseToken = True
isFalseToken _ = False

isNilToken :: Token -> Bool
isNilToken Nil = True
isNilToken _ = False

identifierName :: Token -> String
identifierName (Identifier name) = name

primaryParser :: Parser Expression
primaryParser = literalParser 
                <|> groupingParser
                <|> identifierParser 
                <|> Parser (\input -> (Left $ ParserError (listToMaybe input) "Expected expression", input))
                where
                  literalParser = let parser =
                                        tokenParser isNumberToken
                                        <|> tokenParser isStringToken
                                        <|> tokenParser isTrueToken
                                        <|> tokenParser isFalseToken
                                        <|> tokenParser isNilToken
                                    in Literal <$> parser
                  groupingParser = do
                    tokenParser (== LeftParen)
                    expr <- unaryParser
                    tokenParser (== RightParen)
                    return $ Grouping expr
                  identifierParser = IdentifierExpr <$> tokenParser isIdentifier



parseExpression :: [TokenWithContext] -> (Either ParserError Expression, [TokenWithContext])
parseExpression = runParse expressionParser

parseProgram :: [TokenWithContext] -> Either ParserError [Statement]
parseProgram = fst . runParse programParser

programParser :: Parser [Statement]
programParser = do
  statements <- repeatParser declarationParser
  tokenParser (== EOF)
  return statements

isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False


declarationParser :: Parser Statement
declarationParser = statementParser <|> do
  tokenParser (== Var)
  identifier <- tokenParser' isIdentifier "expect variable name"
  expression <- optional $ do
    tokenParser (== Equal)
    expressionParser
  tokenParser' (== Semicolon) "Expect ';' after variable declaration."
  return $ Declaration identifier expression

statementParser :: Parser Statement
statementParser = expressionStatementParser <|> printStatementParser

expressionStatementParser :: Parser Statement
expressionStatementParser = do
  expression <- expressionParser
  tokenParser' (== Semicolon) "Expect ';' after value."
  return $ ExpressionStatement expression

printStatementParser :: Parser Statement
printStatementParser = do
  tokenParser (== Print)
  expression <- expressionParser
  tokenParser' (== Semicolon) "Expect ';' after value."
  return $ PrintStatement expression
