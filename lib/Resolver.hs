module Resolver where
import Ast
import Token
import Data.Map as Map

type Scope = [Map String Bool]

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

declare :: String -> Scope -> Scope
declare _ [] = []
declare name (scope:scopes) = Map.insert name False scope:scopes

define :: String -> Scope -> Scope
define _ [] = []
define name (scope:scopes) = Map.insert name True scope:scopes

addScope :: Scope -> Scope
addScope scope = Map.empty:scope

resolveStatement :: Statement -> Scope -> (Statement, [TokenWithContext], Scope)
resolveStatement (ExpressionStatement expr) scope = 
  let (er, ee) = resolveExpression expr scope
  in (ExpressionStatement er, ee, scope)

resolveStatement (PrintStatement expr) scope = 
  let (er, ee) = resolveExpression expr scope
  in (PrintStatement er, ee, scope)
  
resolveStatement stmt@(VarDeclaration token@(TokenWithContext (Identifier name) _ _) exprs) scope = 
  let newScope = declare name scope
      exprRes = fmap (`resolveExpression` newScope) exprs
  in case exprRes of
    Just (er, ee) -> (VarDeclaration token (Just er), ee, define name scope)
    Nothing -> (stmt, [], define name scope)

resolveStatement (FunDeclaration token@(TokenWithContext (Identifier name) _ _) params body) scope =
  let newScope = define name scope
      newScope' = Prelude.foldr defineParam (addScope newScope) params
          where defineParam (TokenWithContext (Identifier paramName) _ _) sc = define paramName sc
                defineParam _ sc = sc

      (resolvedBody, e, _) = resolve body newScope'
  in (FunDeclaration token params resolvedBody, e, newScope)

resolveStatement (Block stmts) scope =
  let newScope = addScope scope
      (resolvedStmts, e, _) = resolve stmts newScope
  in (Block resolvedStmts, e, scope)

resolveStatement (IfStatement expr thenStmt elseStmt) scope =
  let (er, ee) = resolveExpression expr scope
      (resolvedThen, eThen, _) = resolveStatement thenStmt scope
      in case elseStmt of
        Just stmt -> 
          let (resolvedElse, eElse, _) = resolveStatement stmt scope
          in (IfStatement er resolvedThen (Just resolvedElse), ee ++ eThen ++ eElse, scope)
        Nothing -> (IfStatement er resolvedThen Nothing, ee ++ eThen, scope)

resolveStatement (WhileStatement expr stmt) scope =
  let (er, ee) = resolveExpression expr scope
      (resolvedStmt, e, _) = resolveStatement stmt scope
  in (WhileStatement er resolvedStmt, ee ++ e, scope)

resolveStatement stmt@(ReturnStatement _ Nothing) scope = (stmt, [], scope)
resolveStatement (ReturnStatement token (Just expr)) scope = 
  let (er, ee) = resolveExpression expr scope
  in (ReturnStatement token (Just er), ee, scope)

resolveStatement stmt scope = (stmt, [], scope)


resolve:: [Statement] -> Scope -> ([Statement], [TokenWithContext], Scope)
resolve stmt scope = Prelude.foldl resolve' ([], [], scope) stmt
  where resolve' (accStmt, accTokens, accScope) s = 
          let (resolvedStmt, tokens, newScope) = resolveStatement s accScope
          in (accStmt ++ [resolvedStmt], accTokens ++ tokens, newScope)
