{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module Ast where
import Token
import Control.Monad
import Data.Maybe
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import GHC.Conc (par)
import Data.Char (isNumber)
import Debug.Trace 

debug :: c -> String -> c
debug = flip trace

data Expression =
  Binary Expression TokenWithContext Expression
  | Unary TokenWithContext Expression
  | Literal TokenWithContext
  | Grouping Expression
  deriving (Show, Eq)

data ParserError = ParserError (Maybe TokenWithContext) String
  deriving (Show, Eq)

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

  mplus p1 p2 = Parser $ \tokens ->
    let result@(value, _) = runParse p1 tokens
    in case value of
      Right _ -> result
      Left _ -> case runParse p2 tokens of
        result'@(Right _, _) -> result'
        _ -> (Left $ noMatch tokens "Faaailed to parse", tokens)


expressionParser :: Parser Expression
expressionParser = equalityParser

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

tokenParser :: (Token -> Bool) -> Parser TokenWithContext
tokenParser = Parser . parseToken

unaryParser :: Parser Expression
unaryParser = let parser =
                    let operatorParser = tokenParser (== Bang) <|> tokenParser (== Minus)
                    in do
                      operator <- operatorParser
                      Unary operator <$> unaryParser
              in parser <|> primaryParser

isNumberToken :: Token -> Bool
isNumberToken (NumberToken _) = True `debug` "isNumberToken"
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

primaryParser :: Parser Expression
primaryParser = literalParser <|> groupingParser <|> Parser (\input -> (Left $ ParserError (listToMaybe input) "Expected expression", input))
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


parseExpression :: [TokenWithContext] -> (Either ParserError Expression, [TokenWithContext])
parseExpression = runParse expressionParser
