module Infernu.Parse
       (translate)
       where

import           Control.Arrow                    ((***))
import           Data.Maybe                       (catMaybes, mapMaybe, fromMaybe)
import qualified Infernu.Log                      as Log
import           Infernu.Prelude
import           Infernu.Expr
import           Infernu.Source                   (GenInfo(..), Source(..), SourcePosSpan(..), Comment(..))

import qualified Language.ECMAScript5.PrettyPrint as ES5PP
import qualified Language.ECMAScript5.Syntax      as ES5
import qualified Text.Parsec.Pos                  as Pos
-- import qualified Data.Map.Strict as Map
-- import  Data.Map.Strict (Map)
import qualified Data.Set as Set
import  Data.Set (Set)
import Infernu.Builtins.Names (refOp, derefOp, refAssignOp)

-- | A 'magic' impossible variable name that can never occur in valid JS syntax.
poo :: EVarName
poo = "_/_"

-- | A dummy expression that does nothing (but has a type).
empty :: a -> Exp (GenInfo, a)
empty z = ELit (gen z) LitUndefined -- EVar z poo

errorNotSupported :: (Show a, ES5PP.Pretty b) => String -> a -> b -> c
errorNotSupported featureName sourcePos expr = error $ "Not supported: '" ++ featureName ++ "' at " ++ show sourcePos ++ " in\n" ++ show (ES5PP.prettyPrint expr)

foldStmts :: Show a => FuncScope -> [ES5.Statement a] -> Exp (GenInfo, a) -> Exp (GenInfo, a)
foldStmts _ [] expr = expr
foldStmts f [x] expr = fromStatement f x expr
foldStmts f (x:xs) expr = fromStatement f x (foldStmts f xs expr)

-- Doesn't carry context over from one statement to the next (good for branching)
parallelStmts :: Show a => FuncScope -> a -> [ES5.Statement a] -> Exp (GenInfo, a) -> Exp (GenInfo, a)
parallelStmts _ _ [] expr = expr
parallelStmts f z stmts expr = ETuple (gen z) $ expr : map (flip (fromStatement f) $ empty z) stmts

chainExprs :: Show a => a -> Exp (GenInfo, a) -> (Exp (GenInfo, a) -> Exp (GenInfo, a)) -> Exp (GenInfo, a) -> Exp (GenInfo, a)
chainExprs a init' getExpr expr = ELet (gen a) poo init' $ getExpr expr

singleStmt :: Show a => a -> Exp a -> Exp a -> Exp a
singleStmt a exp' = ELet a poo exp'

gen :: a -> (GenInfo, a)
gen x = (GenInfo True Nothing, x)

src :: a -> (GenInfo, a)
src x = (GenInfo False Nothing, x)

decl :: a -> String -> (GenInfo, a)
decl x n = (GenInfo False (Just n), x)

litBool :: a -> Bool -> Exp (GenInfo, a)
litBool z b = ELit (gen z) (LitBoolean b)

fromStatement :: Show a => FuncScope -> ES5.Statement a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
fromStatement f (ES5.BlockStmt _ stmts) = foldStmts f stmts
fromStatement _ (ES5.EmptyStmt _) = id
fromStatement f (ES5.ExprStmt z (ES5.AssignExpr _ op target expr)) = \k -> toAssignExpr f z op target expr $ Just k
fromStatement f (ES5.ExprStmt z e) = singleStmt (gen z) $ fromExpression f e
fromStatement f (ES5.IfStmt z pred' thenS elseS) = mkIf f z pred' thenS elseS
-- fromStatement f (ES5.IfSingleStmt z pred' thenS) = mkIf f z pred' thenS (ES5.EmptyStmt z)
-- TODO: The while/do conversion is hacky
fromStatement f (ES5.WhileStmt z pred' loopS) = chainExprs z (EArray (gen z) [fromExpression f pred', litBool z False]) $ fromStatement f loopS
fromStatement f (ES5.DoWhileStmt z loopS pred') = chainExprs z (EArray (gen z) [fromExpression f pred', litBool z False]) $ fromStatement f loopS
fromStatement _ (ES5.BreakStmt _ _) = id -- TODO verify we can ignore this
fromStatement _ (ES5.ContinueStmt _ _) = id -- TODO verify we can ignore this
-- try/catch/finally are indepdendent branches that shouldn't be sharing context. catch is a like an
-- abstraction over the (optional) exception-bound name.
fromStatement f (ES5.TryStmt z stmt mCatch mFinally) = chainExprs z catchExpr $ parallelStmts f z (stmt ++ finallyS)
  where catchExpr = case mCatch of
                        Just (ES5.CatchClause _ (ES5.Id z' e) s) -> EAbs (gen z') [e] (foldStmts f s $ empty z')
                        Nothing -> empty z
        finallyS = case mFinally of
                    Just f' -> f'
                    Nothing -> []
fromStatement _ (ES5.ThrowStmt _ _) = id
fromStatement _ s@(ES5.WithStmt z _ _) = errorNotSupported "with" z s
fromStatement f s@(ES5.ForInStmt z init' expr body) = case init' of
                                                        ES5.ForInVar (ES5.VarDecl{}) -> errorNotSupported "'for..in' with var decl (var hoisting would occur)" z s -- ELet (gen z') name (ELit (gen z') $ LitString "") (foldStmts [body] k)
                                                        ES5.ForInExpr (ES5.VarRef z' (ES5.Id _ name)) -> chainExprs z' (assignToVar z' name str' Nothing) body'
                                                        ES5.ForInExpr (ES5.DotRef z' objExpr (ES5.Id _ name)) -> chainExprs z' (assignToProperty f z objExpr (EPropName name) str' Nothing) body'
                                                        ES5.ForInExpr (ES5.BracketRef z' objExpr idxExpr) -> chainExprs z' (assignToIndex f z objExpr idxExpr str') body'
    where str' = ELit (gen z) $ LitString ""
          body' = fromStatement f body
fromStatement f (ES5.LabelledStmt _ _ s) = fromStatement f s
fromStatement f s@(ES5.ForStmt z init' test increment body) = case init' of
                                                                ES5.NoInit -> forBody
                                                                ES5.VarInit _ -> errorNotSupported "'for' with var decl (var hoisting would occur)" z s -- chainDecls varDecls . forBody
                                                                ES5.ExprInit expr -> chainExprs z (fromExpression f expr) forBody
    where forBody = chainExprs z test'' rest
          test'' = case test of
                    Nothing -> EVar (gen z) poo
                    Just test' -> EArray (gen z) [fromExpression f test', litBool z False]
          body' = fromStatement f body
          rest = case increment of
                   Nothing -> body'
                   Just increment' -> chainExprs z (fromExpression f increment') body'

fromStatement f (ES5.SwitchStmt z switch cases) = chainExprs z (EArray (gen z) tests) . parallelStmts f z $ concatMap getCaseBody cases
    where tests = fromExpression f switch : mapMaybe (fmap (fromExpression f) . getCaseTest) cases
          getCaseTest (ES5.CaseDefault _ _) = Nothing
          getCaseTest (ES5.CaseClause _ test' _) = Just test'
          getCaseBody (ES5.CaseDefault _ body') = body'
          getCaseBody (ES5.CaseClause _ _ body') = body'

fromStatement f (ES5.VarDeclStmt _ decls) = chainDecls f decls
fromStatement f (ES5.FunctionStmt z name args stmts) = toNamedAbs f z args stmts name
fromStatement f (ES5.ReturnStmt z x) = EPropAssign (gen z) (EVar (gen z) "return") (EPropName "value")
                                     $ case x of
                                        Nothing -> ELit (gen z) LitUndefined
                                        Just x' -> fromExpression f x'

-- TODO: Extremely inefficient, the continuation is duplicated between the case branches.
--
-- We should have a name source (monad?) we can use here to generate unique names and wrap the whole
-- case in a let, binding 'k' to a unique name and using EVar to refer to it in the branches.
mkIf :: Show a => FuncScope -> a -> ES5.Expression a -> ES5.Statement a -> ES5.Statement a
                    -> Exp (GenInfo, a) -> Exp (GenInfo, a)
mkIf f z pred' thenS elseS =
    \k -> ECase (gen z) (fromExpression f pred')
          . map (\(v,s) -> (v, chainExprs z (fromStatement f s $ empty z) id k))
          $ [ (LitBoolean True, thenS)
            , (LitBoolean False, elseS)]

-- | Creates an EAbs (function abstraction)
toAbs :: Show a => FuncScope -> a -> [ES5.Id c] -> [ES5.Statement a] -> Exp (GenInfo, a)
toAbs f' z args stmts = EAbs (src z) ("this" : args') body'
  -- TODO: this can lead to problems if "return" was never called (there's a partial function here - dereferencing array element 0)
  where f = foldr collectVars f' stmts
        mutName x = '`' : x
        toMutName x = if x `Set.member` mutableVars f
                      then mutName x
                      else x
        argNames = map ES5.unId args
        args' = map toMutName argNames
        --mutArgDecl :: String -> Exp (GenInfo, a) -> Exp (GenInfo, a)
        mutArgDecl x = if x `Set.member` mutableVars f
                       then ELet (gen z) x $ mkRef z (EVar (gen z) (mutName x))
                       else id
        body' = foldr mutArgDecl bodyExpr argNames
        bodyExpr =
            case any hasReturn stmts of
                True -> ELet (gen z) "return" (ERow (gen z) True []) $ foldStmts f stmts $ (EProp (gen z) (EVar (gen z) "return") (EPropName "value"))
                False -> foldStmts f stmts $ (ELit (gen z) LitUndefined)

data FuncScope = FuncScope { declVars :: Set String, mutableVars :: Set String }
               deriving (Show, Eq, Ord)

instance Monoid FuncScope where
    mempty = FuncScope { declVars = Set.empty, mutableVars = Set.empty }
    mappend x y = FuncScope { declVars = (declVars x) `Set.union` (declVars y)
                            , mutableVars = (mutableVars x) `Set.union` (mutableVars y)
                            }

addMutableVar :: String -> FuncScope -> FuncScope
addMutableVar name f = f { mutableVars = Set.insert name $ mutableVars f }

addDeclVar :: String -> FuncScope -> FuncScope
addDeclVar name f = f { declVars = Set.insert name $ declVars f }

collectVars :: ES5.Statement a -> FuncScope -> FuncScope
collectVars (ES5.BlockStmt _ stmts) = \s -> foldr collectVars s stmts
collectVars (ES5.EmptyStmt _) = id
collectVars (ES5.ExprStmt _ expr) = collectVarsE expr
collectVars (ES5.IfStmt _ e thenS elseS) = collectVarsE e . collectVars elseS . collectVars thenS
-- collectVars (ES5.IfSingleStmt _ e thenS) = collectVarsE e . collectVars thenS
collectVars (ES5.WhileStmt _ e loopS) = collectVarsE e . collectVars loopS
collectVars (ES5.DoWhileStmt _ loopS e) = collectVarsE e . collectVars loopS
collectVars (ES5.BreakStmt _ _) = id
collectVars (ES5.ContinueStmt _ _) = id
collectVars (ES5.TryStmt _ stmt mCatch mFinally) = \s -> foldr collectVars s (stmt ++ finallyS ++ catchS)
  where catchS = case mCatch of
                  Just (ES5.CatchClause _ (ES5.Id _ catchVar) s') -> s'
                  Nothing -> []
        finallyS = case mFinally of
                    Just f -> f
                    Nothing -> []
collectVars (ES5.ThrowStmt _ _) = id
collectVars (ES5.WithStmt _ _ _) = error "Not supported: with" -- collectVars stmt
collectVars (ES5.ForInStmt _ (ES5.ForInVar (ES5.VarDecl _ (ES5.Id _ name) Nothing)) expr stmt) = addDeclVar name . collectVarsE expr . collectVars stmt
collectVars (ES5.ForInStmt _ (ES5.ForInExpr initExpr) expr stmt) = collectVarsLVal initExpr . collectVarsE expr . collectVars stmt
collectVars (ES5.LabelledStmt _ _ stmt) = collectVars stmt
collectVars (ES5.ForStmt _ ES5.NoInit             e1 e2 stmt) = collectVars stmt . collectVarsMaybeExpr e1 . collectVarsMaybeExpr e2
collectVars (ES5.ForStmt _ (ES5.VarInit varDecls) e1 e2 stmt) = \s -> collectVars stmt $ foldr collectVarDecl (collectVarsMaybeExpr e1 . collectVarsMaybeExpr e2 $ s) varDecls
collectVars (ES5.ForStmt _ (ES5.ExprInit expr)    e1 e2 stmt) = collectVars stmt . collectVarsE expr . collectVarsMaybeExpr e1 . collectVarsMaybeExpr e2
collectVars (ES5.SwitchStmt _ e cases) = \s -> collectVarsE e . foldr collectVars s $ concatMap fromCase cases
  where fromCase (ES5.CaseClause _ _ stmts) = stmts
        fromCase (ES5.CaseDefault _ stmts) = stmts
collectVars (ES5.VarDeclStmt _ vs) = \s -> foldr collectVarDecl s vs
collectVars (ES5.FunctionStmt _ _ argNames stmts) = collectFuncVars argNames stmts
collectVars (ES5.ReturnStmt _ e) = collectVarsMaybeExpr e

collectVarDecl :: ES5.VarDecl a -> FuncScope -> FuncScope
collectVarDecl (ES5.VarDecl _ (ES5.Id _ name) expr) = addDeclVar name . collectVarsMaybeExpr expr

collectVarsLVal :: ES5.Expression a -> FuncScope -> FuncScope
collectVarsLVal (ES5.VarRef _ (ES5.Id _ name)) = addMutableVar name
collectVarsLVal (ES5.DotRef _ expr _) = collectVarsE expr
collectVarsLVal (ES5.BracketRef _ e1 e2) = collectVarsE e1 . collectVarsE e2

collectVarsMaybeExpr :: Maybe (ES5.Expression a) -> FuncScope -> FuncScope
collectVarsMaybeExpr Nothing s = s
collectVarsMaybeExpr (Just expr) s = collectVarsE expr s

collectVarsE :: ES5.Expression a -> FuncScope -> FuncScope
collectVarsE (ES5.AssignExpr _ _ lexpr expr)     = collectVarsLVal lexpr . collectVarsE expr
collectVarsE (ES5.UnaryAssignExpr _ _ expr)      = collectVarsLVal expr
collectVarsE (ES5.FuncExpr _ _ argNames stmts)   = collectFuncVars argNames stmts
collectVarsE (ES5.ArrayLit _ exprs)              = \s -> foldr collectVarsE s $ catMaybes exprs
collectVarsE (ES5.ObjectLit _ propExprs)         = \s -> foldr collectVarsE s propValExprs
    where propValExprs = catMaybes $ map getPropVal propExprs
          getPropVal (ES5.PValue _ _ expr) = Just expr
          getPropVal _ = Nothing -- TODO support ES5 get/set
collectVarsE (ES5.DotRef _ expr _)               = collectVarsE expr
collectVarsE (ES5.BracketRef _ expr1 expr2)      = collectVarsE expr1 . collectVarsE expr2
collectVarsE (ES5.NewExpr _ expr exprs)          = \s -> foldr collectVarsE (collectVarsE expr s) exprs
collectVarsE (ES5.PrefixExpr _ _ expr)           = collectVarsE expr
collectVarsE (ES5.InfixExpr _ _ expr1 expr2 )    = collectVarsE expr1 . collectVarsE expr2
collectVarsE (ES5.CondExpr _ expr1 expr2 expr3 ) = collectVarsE expr1 . collectVarsE expr2 . collectVarsE expr3
collectVarsE (ES5.CommaExpr _ exprs)              = \s -> foldr collectVarsE s exprs
collectVarsE (ES5.CallExpr _ expr exprs)         = \s -> foldr collectVarsE (collectVarsE expr s) exprs
collectVarsE _                                   = id

collectFuncVars :: Foldable t => [ES5.Id b] -> t (ES5.Statement a) -> FuncScope -> FuncScope
collectFuncVars argNames stmts = \s -> s { mutableVars = mvs `Set.union` mutableVars s }
    where innerScope = foldr collectVars (FuncScope { declVars = Set.fromList (map ES5.unId argNames) , mutableVars = Set.empty }) stmts
          mvs = (mutableVars innerScope) `Set.difference` (declVars innerScope)

hasReturn :: ES5.Statement a -> Bool
hasReturn (ES5.BlockStmt _ stmts) = any hasReturn stmts
hasReturn (ES5.EmptyStmt _) = False
hasReturn (ES5.ExprStmt _ _) = False
hasReturn (ES5.IfStmt _ _ thenS elseS) = any hasReturn [thenS, elseS]
--hasReturn (ES5.IfSingleStmt _ _ thenS) = hasReturn thenS
hasReturn (ES5.WhileStmt _ _ loopS) = hasReturn loopS
hasReturn (ES5.DoWhileStmt _ loopS _) = hasReturn loopS
hasReturn (ES5.BreakStmt _ _) = False
hasReturn (ES5.ContinueStmt _ _) = False
hasReturn (ES5.TryStmt _ stmts mCatch mFinally) = any hasReturn (stmts ++ finallyS ++ catchS)
  where catchS = case mCatch of
                  Just (ES5.CatchClause _ _ s) -> s
                  Nothing -> []
        finallyS = case mFinally of
                    Just f -> f
                    Nothing -> []
hasReturn (ES5.ThrowStmt _ _) = False
hasReturn (ES5.WithStmt _ _ s) = hasReturn s
hasReturn (ES5.ForInStmt _ _ _ s) = hasReturn s
hasReturn (ES5.LabelledStmt _ _ s) = hasReturn s
hasReturn (ES5.ForStmt _ _ _ _ body) = hasReturn body
hasReturn (ES5.SwitchStmt _ _ cases) = and $ map fromCase cases
  where fromCase (ES5.CaseClause _ _ s) = any hasReturn s
        fromCase (ES5.CaseDefault _ stmts) = any hasReturn stmts
hasReturn (ES5.VarDeclStmt _ _) = False
hasReturn (ES5.FunctionStmt _ _ _ _) = False
hasReturn (ES5.ReturnStmt _ _) = True


addDecl :: Show a => a -> String -> Exp (GenInfo, a) -> Exp (GenInfo, a)
addDecl z name expr = Log.trace ("addDecl: " ++ show res) res
    where res = mapTopAnnotation (const $ decl z name) expr

toNamedAbs :: Show a => FuncScope -> a -> [ES5.Id c] -> [ES5.Statement a] -> ES5.Id a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
toNamedAbs f z args stmts (ES5.Id zn name) letBody = let abs' = addDecl zn name $ toAbs f z args stmts
                                                     in ELet (gen z) name abs' letBody

chainDecls :: Show a => FuncScope -> [ES5.VarDecl a] -> Exp (GenInfo, a) -> Exp (GenInfo, a)
chainDecls _ [] k = k
chainDecls f (ES5.VarDecl z' (ES5.Id _ name) v:xs) k = ELet (gen z') name v' (chainDecls f xs k)
    where ref' = if Set.member name (mutableVars f)
                 then mkRef z'
                 else id
          v' = case v of
                 Just v'' -> addDecl z' name $ ref' $ fromExpression f v''
                 Nothing -> ref' (ELit (gen z') LitUndefined)

mkRef :: a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
mkRef z x = EApp (gen z) (EVar (gen z) refOp) [x]

applyDeref :: a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
applyDeref z x = EApp (gen z) (EVar (gen z) derefOp) [x]

makeThis :: Show a => a -> Exp a
makeThis z = ELit z $ LitEmptyThis

fromExpression :: Show a => FuncScope -> ES5.Expression a -> Exp (GenInfo, a)
fromExpression _ (ES5.StringLit z s) = ELit (src z) $ LitString s
fromExpression _ (ES5.RegexpLit z s g i m) = ELit (src z) $ LitRegex s g i m
fromExpression _ (ES5.BoolLit z s) = ELit (src z) $ LitBoolean s
--fromExpression _ (ES5.IntLit z s) = ELit (src z) (LitNumber $ fromIntegral s)
fromExpression _ (ES5.NumLit z s) = let n = case s of Left i -> fromIntegral i
                                                      Right d -> d
                                    in ELit (src z) $ LitNumber n
fromExpression f (ES5.NullLit z) = ELit (src z) LitNull
fromExpression f (ES5.ArrayLit z exprs) = EArray (src z) $ if null $ catMaybes exprs
                                                         then []
                                                         else exprs'
    where exprs' = map fromMaybeExpression exprs
          fromMaybeExpression Nothing = ELit (gen z) LitUndefined
          fromMaybeExpression (Just x) = fromExpression f x
fromExpression f (ES5.ObjectLit z props) = let stringProps = map (fromPropString . fst . unProp') props
                                         in if all (\x -> x /= Nothing) stringProps
                                            then EStringMap (src z) $ zip (catMaybes stringProps) (map (fromExpression f . snd . unProp') props)
                                            else ERow (src z) False $ map ((fromProp *** fromExpression f) . unProp') props
    where unProp' (ES5.PValue _ p e) = (p, e)
          fromPropStr' (ES5.PValue _ p _) = fromPropString p
          -- TOOD support ES5 property get/set?
fromExpression f (ES5.BracketRef z arrExpr indexExpr) = getIndex f z arrExpr indexExpr
fromExpression f (ES5.VarRef z (ES5.Id _ name)) = deref' z $ EVar (src z) name
    where deref' = if Set.member name (mutableVars f)
                   then applyDeref
                   else const id
fromExpression f (ES5.CondExpr z ePred eThen eElse) = ECase (src z) (fromExpression f ePred) [(LitBoolean True, fromExpression f eThen), (LitBoolean False, fromExpression f eElse)]
fromExpression f (ES5.CallExpr z expr argExprs) =
  -- Instead of simply translating, here we also do some specific simplification by defining
  -- (adding an ELet) for the object expression if the function is a method call.
  -- The idea is to prevent duplicate expressions in the output tree (<complicated expr>.method
  -- (<complicated expr>, ...)) by binding the object expression to '__obj__'.
  -- So that we get: let __obj__ = <complicated expr> in __obj__.method(__obj__, ...)
  case expr of
   ES5.DotRef z' varExpr@(ES5.VarRef _ _) (ES5.Id _ propName) -> appExpr (Just propName) (EProp (src z') var (EPropName propName)) var
     where var = fromExpression f varExpr
   ES5.DotRef z' objExpr (ES5.Id _ propName) -> ELet (gen z') objVarName obj $ appExpr (Just propName) (EProp (src z') objVar (EPropName propName)) objVar
     where obj = fromExpression f objExpr
           objVar = EVar (gen z') objVarName
           objVarName = "_/obj/_"
   _ -> appExpr Nothing (fromExpression f expr) (ELit (gen z) LitEmptyThis)
  where appExpr (Just "call") _ obj = (EApp (src z) obj (map (fromExpression f) argExprs)) -- TODO: may be wrong if object expression is not a function!
        appExpr _ funcExpr thisExpr = (EApp (src z) funcExpr (thisExpr : map (fromExpression f) argExprs))
  --error $ "Assetion failed: expecting at least 'this'"
fromExpression f (ES5.AssignExpr z op target expr) = toAssignExpr f z op target expr Nothing

fromExpression f (ES5.FuncExpr z Nothing     argNames stmts) = toAbs f z argNames stmts
fromExpression f (ES5.FuncExpr z (Just name) argNames stmts) = toNamedAbs f z argNames stmts name (EVar (gen z) $ ES5.unId name)

fromExpression f e@(ES5.CommaExpr z exprs) =
    case exprs of
      [] -> errorNotSupported "empty list (,) expression" z e
      [x] -> fromExpression f x
      -- Should the let here use an allocated name here?
      xs -> ELet (gen z) poo (ETuple (gen z) (tail exprs')) (head exprs')
          where exprs' = reverse . map (fromExpression f) $ xs
fromExpression _ (ES5.ThisRef z) = EVar (src z) "this"
fromExpression f (ES5.DotRef z expr propId) = EProp (src z) (fromExpression f expr) (EPropName $ ES5.unId propId)
fromExpression f (ES5.NewExpr z expr argExprs) = ENew (src z) (fromExpression f expr) (map (fromExpression f) argExprs)
--  ELet z "__this__" (ERow z True []) (ELet z "_bla_" (EApp z (fromExpression f expr) ((EVar z "__this__") : map (fromExpression f) argExprs)) (EVar z "__this__"))
fromExpression f e@(ES5.PrefixExpr z op expr) =
  case op of
    -- prefix +/- are converted to 0-x and 0+x
    ES5.PrefixPlus -> EApp (gen z) (opFunc z ES5.OpAdd) [makeThis (gen z), ELit (gen z) $ LitNumber 0, fromExpression f expr]
    ES5.PrefixMinus -> EApp (gen z) (opFunc z ES5.OpSub) [makeThis (gen z), ELit (gen z) $ LitNumber 0, fromExpression f expr]
    -- delete, void unsupported
    ES5.PrefixVoid -> errorNotSupported "void" z e
    ES5.PrefixDelete -> errorNotSupported "delete" z e
    -- all the rest are expected to exist as unary builtin functions
    _ -> EApp (src z) (EVar (gen z) $ show . ES5PP.prettyPrint $ op) [makeThis (gen z), fromExpression f expr]
fromExpression f (ES5.InfixExpr z op e1 e2) = EApp (gen z) (EVar (gen z) $ show . ES5PP.prettyPrint $ op) [makeThis (gen z), fromExpression f e1, fromExpression f e2]
fromExpression _ (ES5.UnaryAssignExpr z op (ES5.VarRef _ (ES5.Id _ name))) = assignToVar z name (addConstant z op (applyDeref z $ EVar (src z) name)) Nothing
fromExpression f (ES5.UnaryAssignExpr z op (ES5.DotRef _ objExpr (ES5.Id _ name))) = assignToProperty f z objExpr (EPropName name) (addConstant z op (EProp (src z) objExpr' (EPropName name))) Nothing
  where objExpr' = fromExpression f objExpr
fromExpression f (ES5.UnaryAssignExpr z op (ES5.BracketRef _ objExpr idxExpr)) = assignToIndex f z objExpr idxExpr $ addConstant z op (getIndex f z objExpr idxExpr)

-- toAssignExpr :: Show t =>
--     FuncScope -> t -> ES5.AssignOp -> ES5.Expression t -> ES5.Expression t -> Maybe (Exp (GenInfo, t)) -> Exp (GenInfo, t)
toAssignExpr f z op target expr cont = assignExpr
  where sz = src z
        (assignExpr, oldValue) = case target of
          ES5.VarRef _ (ES5.Id _ name) -> (assignToVar z name value cont, applyDeref z $ EVar sz name)
          ES5.DotRef _ objExpr (ES5.Id _ name) -> (assignToProperty f z objExpr (EPropName name) value cont, EProp sz (fromExpression f objExpr) (EPropName name))
          ES5.BracketRef _ objExpr idxExpr -> (singleStmt (gen z) (assignToIndex f z objExpr idxExpr value) (fromMaybe atIndex' cont), atIndex')
              where atIndex' = getIndex f z objExpr idxExpr
        expr' = fromExpression f expr
        value = case op of
          ES5.OpAssign -> expr'
          ES5.OpAssignAdd -> applyOpFunc z ES5.OpAdd [oldValue, expr']
          ES5.OpAssignSub -> applyOpFunc z ES5.OpSub [oldValue, expr']
          ES5.OpAssignMul -> applyOpFunc z ES5.OpMul [oldValue, expr']
          ES5.OpAssignDiv -> applyOpFunc z ES5.OpDiv [oldValue, expr']
          ES5.OpAssignMod -> applyOpFunc z ES5.OpMod [oldValue, expr']
          ES5.OpAssignLShift   -> applyOpFunc z ES5.OpLShift   [oldValue, expr']
          ES5.OpAssignSpRShift -> applyOpFunc z ES5.OpSpRShift [oldValue, expr']
          ES5.OpAssignZfRShift -> applyOpFunc z ES5.OpZfRShift [oldValue, expr']
          ES5.OpAssignBAnd     -> applyOpFunc z ES5.OpBAnd     [oldValue, expr']
          ES5.OpAssignBXor     -> applyOpFunc z ES5.OpBXor     [oldValue, expr']
          ES5.OpAssignBOr      -> applyOpFunc z ES5.OpBOr      [oldValue, expr']




opFunc :: a -> ES5.InfixOp -> Exp (GenInfo, a)
opFunc z op = EVar (gen z) $ show . ES5PP.prettyPrint $ op

applyOpFunc :: Show a => a -> ES5.InfixOp -> [Exp (GenInfo, a)] -> Exp (GenInfo, a)
applyOpFunc z op exprs = EApp (gen z) (opFunc z op) (makeThis (gen z) : exprs)

-- TODO: the translation results in equivalent types, but currently ignore pre vs. postfix so the data flow is wrong.
addConstant :: Show a => a -> ES5.UnaryAssignOp -> Exp (GenInfo, a) -> Exp (GenInfo, a)
addConstant z op expr = EApp (gen z) (opFunc z ES5.OpAdd) [makeThis (gen z), expr, ELit (gen z) $ LitNumber x]
  where x = case op of
             ES5.PrefixInc -> 1
             ES5.PrefixDec -> -1
             ES5.PostfixInc -> 1
             ES5.PostfixDec -> -1

assignToVar :: Show a => a -> EVarName -> Exp (GenInfo, a) -> Maybe (Exp (GenInfo, a)) -> Exp (GenInfo, a)
assignToVar z name expr cont = ELet (gen z) poo assignApp' $ fromMaybe (applyDeref z $ EVar (src z) name) cont
    where assignApp' = EApp (src z) (EVar (gen z) refAssignOp) [EVar (gen z) name, expr]

assignToProperty :: Show a => FuncScope -> a -> ES5.Expression a -> EPropName -> Exp (GenInfo, a) -> Maybe (Exp (GenInfo, a)) -> Exp (GenInfo, a)
assignToProperty f z objExpr name expr cont = EPropAssign (src z) objExpr' name expr $ fromMaybe (EProp (src z) objExpr' name) cont
  where objExpr' = fromExpression f objExpr

applyPropFunc :: a -> EPropName -> Exp (GenInfo, a) -> [Exp (GenInfo, a)] -> Exp (GenInfo, a)
applyPropFunc z prop arrExpr args = ELet (gen z) obj' arrExpr $ applyPropFunc'
    where obj' = "bracketObj"
          objVar = EVar (gen z) obj'
          applyPropFunc' = EApp (gen z) getProp' (objVar:args)
          getProp' = EProp (gen z) objVar prop

getIndex :: Show a => FuncScope -> a -> ES5.Expression a -> ES5.Expression a -> Exp (GenInfo, a)
getIndex f z arrExpr indexExpr = applyPropFunc z EPropGetIndex (fromExpression f arrExpr) [fromExpression f indexExpr]

assignToIndex :: Show a => FuncScope -> a -> ES5.Expression a  -> ES5.Expression a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
assignToIndex f z objExpr idxExpr expr = applyPropFunc z EPropSetIndex objExpr' [idxExpr', expr]
  where objExpr' = fromExpression f objExpr
        idxExpr' = fromExpression f idxExpr


fromProp :: ES5.Prop a -> EPropName
fromProp (ES5.PropId _ x) = EPropName x
fromProp (ES5.PropString _ x) = EPropName x
fromProp (ES5.PropNum _ x) = EPropName $ show x

fromPropString :: ES5.Prop a -> Maybe String
fromPropString (ES5.PropString _ x) = Just x
fromPropString _ = Nothing

-- -- ------------------------------------------------------------------------

translate :: [ES5.Statement (SourcePosSpan, [Comment])] -> Exp (Source, [Comment])
translate js = fmap (\(g, (span, comments)) -> (Source g span, comments))
               $ ELet (gen pos) poo (empty pos) $ foldStmts f js $ EVar (gen pos) poo
  where pos = (SourcePosGlobal, [])
        f = foldr collectVars (FuncScope Set.empty Set.empty) js
