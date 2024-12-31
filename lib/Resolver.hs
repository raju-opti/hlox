module Resolver where
import Ast
import Token
import Data.Map as Map
import Data.HashTable.Class (HashTable(new))
import Data.Maybe (fromMaybe)

type Scope = [Map String Bool]

data SemanticError = SemanticError String Int Int
  deriving (Eq)

semanticError :: TokenWithContext -> String -> SemanticError
semanticError (TokenWithContext _ line column) msg = SemanticError msg line column

instance Show SemanticError where
  show (SemanticError msg line column) = "Syntax Error: " ++ msg ++ " at line " ++ show line ++ " column " ++ show column

data FunctionType = NoneF | Function | Method
  deriving (Eq, Show)

data ClassType = NoneC | ClassC | SubclassC
  deriving (Eq, Show)

data ResolutionState = ResolutionState {
  rsScopes :: Scope,
  rsFunctionType :: FunctionType,
  rsClassType :: ClassType
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

resolveExpression :: Expression -> ResolutionState -> (Expression, [SemanticError])
resolveExpression (Binary left op right) rs =
  let (lr, le) = resolveExpression left rs
      (rr, re) = resolveExpression right rs
  in (Binary lr op rr, le ++ re)
resolveExpression (Unary op right) rs =
  let (rr, re) = resolveExpression right rs
  in (Unary op rr, re)
resolveExpression (Literal token) _ = (Literal token, [])
resolveExpression (Grouping expr) rs =
  let (er, ee) = resolveExpression expr rs
  in (Grouping er, ee)

resolveExpression (IdentifierExpr token@(TokenWithContext (Identifier name) _ _) _) rs =
  case resolveDistance (rsScopes rs) name of
    Just (distance, b)
      | b -> (IdentifierExpr token (Just distance), [])
      | otherwise -> (IdentifierExpr token Nothing, [selfReferenceError token])
    Nothing -> (IdentifierExpr token Nothing, [])

resolveExpression (Assignment target expr) rs =
  let (tr, te) = resolveExpression target rs
      (er, ee) = resolveExpression expr rs
  in (Assignment tr er, te ++ ee)

resolveExpression (Call callee paren arguments) rs =
  let (cr, ce) = resolveExpression callee rs
      (ar, ae) = Prelude.foldr (\arg (acc, accE) ->
          let (r, e) = resolveExpression arg rs
          in (r:acc, e ++ accE)
        ) ([], []) arguments
  in (Call cr paren ar, ce ++ ae)

resolveExpression (Get obj name) rs =
  let (r, oe) = resolveExpression obj rs
  in (Get r name, oe)

resolveExpression (Set obj name value) rs =
  let (obr, obe) = resolveExpression obj rs
      (vr, ve) = resolveExpression value rs
  in (Set obr name vr, obe ++ ve)

resolveExpression (ThisExpr token _) ResolutionState{rsClassType = NoneC} =
  (ThisExpr token 0, [semanticError token "Can't use 'this' outside of a class."])
resolveExpression (ThisExpr token _) rs =
  case resolveDistance (rsScopes rs) "this" of
    Just (distance, _) -> (ThisExpr token distance, [])
    Nothing -> (ThisExpr token 0, [])

resolveExpression expr@(SuperExpr token method _) rs =
  case rsClassType rs of
    SubclassC -> case resolveDistance (rsScopes rs) "super" of
      Just (distance, _) -> (SuperExpr token method distance, [])
      Nothing -> (SuperExpr token method 0, [])
    NoneC -> (expr, [semanticError token "Can't use 'super' outside of a class."])
    ClassC -> (expr, [semanticError token "Can't use 'super' in a class with no superclass."])

resolveExpression e _ = (e, [])

declare :: String -> ResolutionState -> (ResolutionState, Maybe String)
declare _ rs@ResolutionState{ rsScopes=[] } = (rs, Nothing)
declare name rs@ResolutionState{ rsScopes=(scope:scopes) } =
  let err = case Map.lookup name scope of
        Just _ -> Just "Already a variable with this name in this scope."
        Nothing -> Nothing
  in (rs{rsScopes = Map.insert name False scope:scopes }, err)

define :: String -> ResolutionState -> ResolutionState
define _ rs@ResolutionState{ rsScopes=[] } = rs
define name rs@ResolutionState{ rsScopes=(scope:scopes) } = rs{rsScopes = Map.insert name True scope:scopes }

addScope :: ResolutionState -> ResolutionState
addScope rs@ResolutionState{ rsScopes = scopes } = rs { rsScopes  = Map.empty:scopes }

newFunction :: FunctionType -> ResolutionState -> ResolutionState
newFunction ft rs@(ResolutionState{rsScopes=scopes}) = rs { rsScopes = Map.empty : scopes, rsFunctionType = ft }

newClass :: ClassType -> ResolutionState -> ResolutionState
newClass ct rs@(ResolutionState{rsScopes=scopes}) =
  let newScope = Map.fromList [("this", True)]
  in rs { rsScopes = newScope : scopes, rsClassType = ct }

resolveStatement :: Statement -> ResolutionState -> (Statement, [SemanticError], ResolutionState)
resolveStatement (ExpressionStatement expr) rs =
  let (er, ee) = resolveExpression expr rs
  in (ExpressionStatement er, ee, rs)

resolveStatement (PrintStatement expr) rs =
  let (er, ee) = resolveExpression expr rs
  in (PrintStatement er, ee, rs)

resolveStatement stmt@(VarDeclaration token@(TokenWithContext (Identifier name) l c) expr) rs =
  let (rs', msg) = declare name rs
      err = maybeError msg l c
  in case expr of
    Just e ->
      let (er, ee) = resolveExpression e rs'
      in (VarDeclaration token (Just er), ee ++ err, define name rs')
    Nothing -> (stmt, err, define name rs')

resolveStatement (FunDeclaration (AstFunction token@(TokenWithContext (Identifier name) l c) params body)) rs =
  let (rs', msg) = declare name rs
      err = maybeError msg l c
      rs'' = define name rs'
      (resolvedFn, fe, _) = resolveFunction Function (AstFunction token params body) rs''

  in (FunDeclaration resolvedFn, err ++ fe, rs'')

resolveStatement (ClassDeclaration (AstClass token@(TokenWithContext (Identifier name) l c) methods superclass)) rs =
  let (rs', msg) = declare name rs
      err = maybeError msg l c
      (superR, superErr) = maybe (Nothing, []) resolveSuperclass superclass
         where resolveSuperclass s = let (r, e) = resolveExpression s rs'
                in (Just r, e)

      selfInheritErr = case superclass of
        Just (IdentifierExpr (TokenWithContext (Identifier superName) _ _) _)
          | name == superName -> [semanticError token "A class can't inherit from itself."]
        _ -> []

      rs'' = define name rs'
      rs''' = case superclass of
        Just _ -> newClass SubclassC . define "super" . addScope $ rs''
        _ -> newClass ClassC rs''
  
      (resolvedMethods, me) = Prelude.foldr resolveMethod ([], []) methods
          where resolveMethod m (acc, aerr) =
                  let (resolvedMethod, e, _) = resolveFunction Method m rs'''
                  in (resolvedMethod:acc, e ++ aerr)

  in (ClassDeclaration (AstClass token resolvedMethods superR), err ++ superErr ++ selfInheritErr ++ me, rs'')

resolveStatement (Block stmts) rs =
  let rs' = addScope rs
      (resolvedStmts, e, _) = resolve stmts rs'
  in (Block resolvedStmts, e, rs)

resolveStatement (IfStatement expr thenStmt elseStmt) rs =
  let (er, ee) = resolveExpression expr rs
      (resolvedThen, eThen, _) = resolveStatement thenStmt rs
      in case elseStmt of
        Just stmt ->
          let (resolvedElse, eElse, _) = resolveStatement stmt rs
          in (IfStatement er resolvedThen (Just resolvedElse), ee ++ eThen ++ eElse, rs)
        Nothing -> (IfStatement er resolvedThen Nothing, ee ++ eThen, rs)

resolveStatement (WhileStatement expr stmt) rs =
  let (er, ee) = resolveExpression expr rs
      (resolvedStmt, e, _) = resolveStatement stmt rs
  in (WhileStatement er resolvedStmt, ee ++ e, rs)

resolveStatement stmt@(ReturnStatement token expr) rs =
  let err = case rsFunctionType rs of
        Function -> []
        Method -> []
        _ ->  [topLevelReturnError token]
  in case expr of
    Just e ->
      let (er, ee) = resolveExpression e rs
      in (ReturnStatement token (Just er), ee ++ err, rs)
    Nothing -> (stmt, err, rs)

resolveStatement stmt scope = (stmt, [], scope)

resolveFunction :: FunctionType -> AstFunction -> ResolutionState -> (AstFunction, [SemanticError], ResolutionState)
resolveFunction ft (AstFunction token params body) rs =
  let (rs', pe) = Prelude.foldr defineParam (newFunction ft rs, []) params
         where defineParam (TokenWithContext (Identifier paramName) l c) (s, err) =
                let (s', msg) = declare paramName s
                    err' = maybeError msg l c
                in (define paramName s', err' ++ err)

      (resolvedBody, be, _) = resolve body rs'
  in (AstFunction token params resolvedBody, pe ++ be, rs)

resolve:: [Statement] -> ResolutionState -> ([Statement], [SemanticError], ResolutionState)
resolve stmt scope = Prelude.foldl resolve' ([], [], scope) stmt
  where resolve' (accStmt, accTokens, accScope) s =
          let (resolvedStmt, tokens, newScope) = resolveStatement s accScope
          in (accStmt ++ [resolvedStmt], accTokens ++ tokens, newScope)
