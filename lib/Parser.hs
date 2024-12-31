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
import Data.Traversable (for)
import GHC.Read (list)
import Resolver (FunctionType(Function))
import Ast (Statement(FunDeclaration))

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
expressionParser = assignmentParser

assignmentParser :: Parser Expression
assignmentParser = do
  expression <- equalityParser
  eqSign <- optional $ tokenParser (== Equal)
  case eqSign of
    Just _ -> do
      valueExpr <- assignmentParser
      case expression of
        IdentifierExpr _ _-> return $ Assignment expression valueExpr
        Get obj name -> return $ Set obj name valueExpr
        _ -> Parser (Left $ ParserError eqSign "Invalid assignment target",)
    Nothing -> return expression

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
                                                  foldl (\acc (op, expr) -> Binary acc op expr) expression <$> subParser

orParser :: Parser Expression
orParser = leftAssociativeParser andParser (tokenParser (== Or))

andParser :: Parser Expression
andParser = leftAssociativeParser equalityParser (tokenParser (== And))

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
              in parser <|> callParser

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

atLeastOneParser :: Parser a -> Parser [a]
atLeastOneParser parser = do
  a <- parser
  as <- repeatParser parser
  return (a:as)


argumentListParser :: Parser ([Expression], TokenWithContext)
argumentListParser = do
  tokenParser (== LeftParen)
  arguments <- argumentsParser
  closing <- tokenParser' (== RightParen) "Expect ')' after arguments."
  return (arguments, closing)
  where
    argumentsParser = do
      expression <- optional expressionParser
      case expression of
        Just expr ->
          let subParser = repeatParser $ do
                tokenParser (== Comma)
                expressionParser
          in do
            exprs <- subParser
            return (expr:exprs)
        Nothing -> return []

eitherParser :: Parser a -> Parser b -> Parser (Either a b)
eitherParser p1 p2 = (Left <$> p1) <|> (Right <$> p2)

callParser :: Parser Expression
callParser = do
      callee <- primaryParser
      ops <- repeatParser (eitherParser argumentListParser propertyAccessParser)
      return $ foldl foldF callee ops
      where propertyAccessParser = do
              tokenParser (== Dot)
              tokenParser' isIdentifier "Expect property name after '.'."
            foldF acc arg = case arg of
              Left (exprs, closing) -> Call acc closing exprs
              Right prop -> Get acc prop


primaryParser :: Parser Expression
primaryParser = literalParser
                <|> groupingParser
                <|> identifierParser
                <|> thisExprParser
                <|> superExprParser
                <|> Parser (\input -> (Left $ ParserError (listToMaybe input) "Expected expression", input))
                where
                  literalParser = let parser =
                                        tokenParser isNumberToken
                                        <|> tokenParser isStringToken
                                        <|> tokenParser isTrueToken
                                        <|> tokenParser isFalseToken
                                        <|> tokenParser isNilToken
                                    in Literal <$> parser
                  thisExprParser = (`ThisExpr` 0) <$> tokenParser (== This)
                  superExprParser = do
                    tokenParser (== Super)
                    superToken <- tokenParser' (== Dot) "Expect '.' after 'super'."
                    method <- tokenParser' isIdentifier "Expect superclass method name"
                    return $ SuperExpr superToken method 0
                  groupingParser = do
                    tokenParser (== LeftParen)
                    expr <- expressionParser
                    tokenParser (== RightParen)
                    return $ Grouping expr
                  identifierParser = (`IdentifierExpr` Nothing) <$> tokenParser isIdentifier


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

parameterListParser :: String -> Parser [TokenWithContext]
parameterListParser kind = do
  tokenParser' (== LeftParen) $ "Expect '(' after " ++ kind ++ " name."
  parameters <- paramParser
  tokenParser' (== RightParen) "Expect ')' after arguments."
  return parameters
  where
    paramParser = do
      let paramParser = tokenParser' isIdentifier "Expect parameter name"
      token <- optional paramParser
      case token of
        Just p ->
          let subParser = repeatParser $ do
                tokenParser (== Comma)
                paramParser
          in do
            params <- subParser
            return (p:params)
        Nothing -> return []

functionParser :: String -> Parser AstFunction
functionParser kind = do
  name <- tokenParser' isIdentifier $ "Expect " ++ kind ++ " name"
  parameters <- parameterListParser kind
  AstFunction name parameters <$> blockedStatementsParser

funDeclarationParser :: Parser Statement
funDeclarationParser = do
  tokenParser (== Fun)
  FunDeclaration <$> functionParser "function"

classDeclarationParser :: Parser Statement
classDeclarationParser = do
  tokenParser (== Class)
  name <- tokenParser' isIdentifier "Expect class name"
  superclass <- optional $ do
    tokenParser (== Less)
    token <- tokenParser' isIdentifier "Expect superclass name."
    return $ IdentifierExpr token Nothing
  tokenParser (== LeftBrace)
  methods <- repeatParser $ functionParser "method"
  tokenParser (== RightBrace)
  return $ ClassDeclaration $ AstClass name methods superclass

varDeclarationParser :: Parser Statement
varDeclarationParser = do
  tokenParser (== Var)
  identifier <- tokenParser' isIdentifier "expect variable name"
  expression <- optional $ do
    tokenParser (== Equal)
    expressionParser
  tokenParser' (== Semicolon) "Expect ';' after variable declaration."
  return $ VarDeclaration identifier expression

declarationParser :: Parser Statement
declarationParser = statementParser <|> varDeclarationParser <|> funDeclarationParser <|> classDeclarationParser

ifStatementParser :: Parser Statement
ifStatementParser = do
  tokenParser (== If)
  tokenParser' (== LeftParen) "Expect '(' after 'if'."
  condition <- expressionParser
  tokenParser' (== RightParen) "Expect ')' after if condition."
  thenBranch <- statementParser
  elseBranch <- optional $ do
    tokenParser (== Else)
    statementParser
  return $ IfStatement condition thenBranch elseBranch

whileStatementParser :: Parser Statement
whileStatementParser = do
  tokenParser (== While)
  tokenParser' (== LeftParen) "Expect '(' after 'while'."
  condition <- expressionParser
  tokenParser' (== RightParen) "Expect ')' after while condition."
  WhileStatement condition <$> statementParser

forStatementParser :: Parser Statement
forStatementParser = do
  tokenParser (== For)
  tokenParser' (== LeftParen) "Expect '(' after 'for'."
  initializer <- valueOrSemicolon declarationParser
  condition <- optional expressionParser
  tokenParser' (== Semicolon) "Expect ';' after loop condition."
  increment <- optional expressionParser
  tokenParser' (== RightParen) "Expect ')' after for clauses."
  body <- statementParser
  let body' = case increment of
                Just expr -> Block [body, ExpressionStatement expr]
                Nothing -> body
      condition' = fromMaybe (Literal $ TokenWithContext TrueToken 0 0) condition
      while = WhileStatement condition' body'
  return $ case initializer of
    Just stmt -> Block [stmt, while]
    Nothing -> while
    where valueOrSemicolon parser = (Just <$> parser) <|> (tokenParser (== Semicolon) >> return Nothing)

statementParser :: Parser Statement
statementParser = expressionStatementParser
    <|> printStatementParser
    <|> blockParser
    <|> ifStatementParser
    <|> whileStatementParser
    <|> forStatementParser
    <|> returnStatementParser

returnStatementParser :: Parser Statement
returnStatementParser = do
  returnToken <- tokenParser (== Return)
  value <- optional expressionParser
  tokenParser' (== Semicolon) "Expect ';' after return value."
  return $ ReturnStatement returnToken value

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

blockParser :: Parser Statement
blockParser = Block <$> blockedStatementsParser

blockedStatementsParser :: Parser [Statement]
blockedStatementsParser = do
  tokenParser (== LeftBrace)
  statements <- repeatParser declarationParser
  tokenParser' (== RightBrace) "Expect '}' after block."
  return statements
