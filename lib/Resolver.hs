module Resolver where
import Ast
import Token
import Data.Map as Map

type Scope = [Map String Bool]

data SemanticError = SemanticError String Int Int
  deriving (Eq)

instance Show SemanticError where
  show (SemanticError msg line column) = "Syntax Error: " ++ msg ++ " at line " ++ show line ++ " column " ++ show column

data FunctionType = None | Function
  deriving (Eq, Show)

data ResolutionState = ResolutionState {
  rsScopes :: Scope,
  rsFunctionType :: FunctionType
}

selfReferenceError :: TokenWithContext -> SemanticError
selfReferenceError (TokenWithContext (Identifier name) line column) =
  SemanticError ("Can't read local variable in its own initializer: " ++ name) line column

topLevelReturnError :: TokenWithContext -> SemanticError
topLevelReturnError (TokenWithContext _ line column) =
  SemanticError "Can't return from top-level code." line column

maybeError :: Maybe String -> Int -> Int -> [SemanticError]
maybeError Nothing _ _ = []
maybeError (Just msg) line column = [SemanticError msg line column]

resolveDistance':: Scope -> String -> Int -> Maybe (Int, Bool)
resolveDistance' [] _ _ = Nothing
resolveDistance' (scope:scopes) name distance = case Map.lookup name scope of
  Just b -> Just (distance, b)
  Nothing -> resolveDistance' scopes name (distance + 1)

resolveDistance :: Scope -> String -> Maybe (Int, Bool)
resolveDistance scope name = resolveDistance' scope name 0

resolveExpression :: Expression -> Scope -> (Expression, [TokenWithContext])
resolveExpression (Binary left op right) scope =
  let (lr, le) = resolveExpression left scope
      (rr, re) = resolveExpression right scope
  in (Binary lr op rr, le ++ re)
resolveExpression (Unary op right) scope =
  let (rr, re) = resolveExpression right scope
  in (Unary op rr, re)
resolveExpression (Literal token) _ = (Literal token, [])
resolveExpression (Grouping expr) scope =
  let (er, ee) = resolveExpression expr scope
  in (Grouping er, ee)

resolveExpression (IdentifierExpr token@(TokenWithContext (Identifier name) _ _) _) scope =
  case resolveDistance scope name of
    Just (distance, b)
      | b -> (IdentifierExpr token (Just distance), [])
      | otherwise -> (IdentifierExpr token Nothing, [token])
    Nothing -> (IdentifierExpr token Nothing, [])

resolveExpression (Assignment target expr) scope =
  let (tr, te) = resolveExpression target scope
      (er, ee) = resolveExpression expr scope
  in (Assignment tr er, te ++ ee)

resolveExpression (Call callee paren arguments) scope =
  let (cr, ce) = resolveExpression callee scope
      (ar, ae) = Prelude.foldr (\arg (acc, accE) ->
          let (r, e) = resolveExpression arg scope
          in (r:acc, e ++ accE)
        ) ([], []) arguments
  in (Call cr paren ar, ce ++ ae)

resolveExpression e _ = (e, [])

declare :: String -> ResolutionState -> (ResolutionState, Maybe String)
declare _ rs@(ResolutionState [] _) = (rs, Nothing)
declare name (ResolutionState (scope:scopes) cf) =
  let err = case Map.lookup name scope of
        Just _ -> Just "Already a variable with this name in this scope."
        Nothing -> Nothing
  in (ResolutionState (Map.insert name False scope:scopes) cf, err)

define :: String -> ResolutionState -> ResolutionState
define _ rs@(ResolutionState [] _) = rs
define name (ResolutionState (scope:scopes) cf) = ResolutionState (Map.insert name True scope:scopes) cf

addScope :: ResolutionState -> ResolutionState
addScope (ResolutionState scopes cf) = ResolutionState (Map.empty:scopes) cf

newFunction :: ResolutionState -> ResolutionState
newFunction (ResolutionState scopes _) = ResolutionState (Map.empty:scopes) Function

resolveStatement :: Statement -> ResolutionState -> (Statement, [SemanticError], ResolutionState)
resolveStatement (ExpressionStatement expr) rs =
  let (er, ee) = resolveExpression expr (rsScopes rs)
  in (ExpressionStatement er, selfReferenceError <$> ee, rs)

resolveStatement (PrintStatement expr) rs =
  let (er, ee) = resolveExpression expr (rsScopes rs)
  in (PrintStatement er, selfReferenceError <$> ee, rs)

resolveStatement stmt@(VarDeclaration token@(TokenWithContext (Identifier name) l c) expr) rs =
  let (rs', msg) = declare name rs
      err = maybeError msg l c
  in case expr of
    Just e ->
      let (er, ee) = resolveExpression e (rsScopes rs')
      in (VarDeclaration token (Just er), (selfReferenceError <$> ee) ++ err, define name rs')
    Nothing -> (stmt, err, define name rs')

resolveStatement (FunDeclaration (AstFunction token@(TokenWithContext (Identifier name) l c) params body)) rs =
  let (rs', msg) = declare name rs
      err = maybeError msg l c
      rs'' = define name rs'
      (rs''', pe) = Prelude.foldr defineParam (newFunction rs'', []) params
          where defineParam (TokenWithContext (Identifier paramName) l c) (s, err) = 
                  let (s', msg) = declare paramName s
                      err' = maybeError msg l c
                  in (define paramName s', err' ++ err)

      (resolvedBody, be, _) = resolve body rs'''
  in (FunDeclaration (AstFunction token params resolvedBody), err ++ pe ++ be, rs'')

resolveStatement (Block stmts) rs =
  let rs' = addScope rs
      (resolvedStmts, e, _) = resolve stmts rs'
  in (Block resolvedStmts, e, rs)

resolveStatement (IfStatement expr thenStmt elseStmt) rs =
  let (er, ee) = resolveExpression expr (rsScopes rs)
      err = selfReferenceError <$> ee
      (resolvedThen, eThen, _) = resolveStatement thenStmt rs
      in case elseStmt of
        Just stmt ->
          let (resolvedElse, eElse, _) = resolveStatement stmt rs
          in (IfStatement er resolvedThen (Just resolvedElse), err ++ eThen ++ eElse, rs)
        Nothing -> (IfStatement er resolvedThen Nothing, err ++ eThen, rs)

resolveStatement (WhileStatement expr stmt) rs =
  let (er, ee) = resolveExpression expr (rsScopes rs)
      err = selfReferenceError <$> ee
      (resolvedStmt, e, _) = resolveStatement stmt rs
  in (WhileStatement er resolvedStmt, err ++ e, rs)

resolveStatement stmt@(ReturnStatement token expr) rs = 
  let err = case rsFunctionType rs of
        Function -> []
        _ ->  [topLevelReturnError token]
  in case expr of
    Just e ->
      let (er, ee) = resolveExpression e (rsScopes rs)
      in (ReturnStatement token (Just er), (selfReferenceError <$> ee) ++ err, rs)
    Nothing -> (stmt, err, rs)
    
resolveStatement stmt scope = (stmt, [], scope)


resolve:: [Statement] -> ResolutionState -> ([Statement], [SemanticError], ResolutionState)
resolve stmt scope = Prelude.foldl resolve' ([], [], scope) stmt
  where resolve' (accStmt, accTokens, accScope) s =
          let (resolvedStmt, tokens, newScope) = resolveStatement s accScope
          in (accStmt ++ [resolvedStmt], accTokens ++ tokens, newScope)
