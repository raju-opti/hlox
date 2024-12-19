{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE TupleSections #-}
module Ast where
import Token
import Control.Monad
import Data.Maybe
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

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
        _ -> (Left $ noMatch tokens "Failed to parse", tokens)


parseExpression :: ParseFn Expression
parseExpression = parseEquality

parseEquality :: ParseFn Expression
parseEquality = parseComparison

parseComparison :: ParseFn Expression
parseComparison = parseTerm

parseTerm :: ParseFn Expression
parseTerm = parseFactor

parseFactor :: ParseFn Expression
parseFactor = parseUnary

parseUnary :: ParseFn Expression
parseUnary = parsePrimary


parseLeftParen :: ParseFn TokenWithContext
parseLeftParen (token@(TokenWithContext LeftParen _ _):rest) = (Right token, rest)
parseLeftParen input = (Left $ noMatch input "Expected '('", input)

parseRightParen :: ParseFn TokenWithContext
parseRightParen (token@(TokenWithContext RightParen _ _):rest) = (Right $ token, rest)
parseRightParen input = (Left $ noMatch input "Expected ')'", input)

parsePrimary :: ParseFn Expression
parsePrimary [] = (Left $ ParserError Nothing "Unexpected EOF", [])
parsePrimary input@(token:rest) =
  case token of
    TokenWithContext (NumberToken n) _ _ -> (Right $ Literal token, rest)
    TokenWithContext (StringToken s) _ _ -> (Right $ Literal token, rest)
    TokenWithContext TrueToken _ _ -> (Right $ Literal token, rest)
    TokenWithContext FalseToken _ _ -> (Right $ Literal token, rest)
    TokenWithContext Nil _ _ -> (Right $ Literal token, rest)
    TokenWithContext LeftParen _ _ -> let parser = do
                                            Parser parseLeftParen
                                            e <- Parser parseExpression
                                            Parser parseRightParen
                                            return e
                                        in case runParse parser input of
                                            (Right expr, rest) -> (Right $ Grouping expr, rest)
                                            (Left err, rest) -> (Left err, rest)
    _ -> (Left $ ParserError (Just token) "Expected expression", input)
