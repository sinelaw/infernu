module Infernu.Parse
       (translate)
       where

import           Control.Arrow                    ((***))
import           Data.Maybe                       (mapMaybe, catMaybes)
import qualified Language.ECMAScript3.PrettyPrint as ES3PP
import qualified Language.ECMAScript3.Syntax      as ES3
import           Infernu.Types
import qualified Text.Parsec.Pos                  as Pos
import qualified Infernu.Log as Log
    
-- | A 'magic' impossible variable name that can never occur in valid JS syntax.
poo :: EVarName
poo = "_/_"

-- | A dummy expression that does nothing (but has a type).
empty :: a -> Exp (GenInfo, a)
empty z = ELit (gen z) LitUndefined -- EVar z poo
          
errorNotSupported :: (Show a, ES3PP.Pretty b) => String -> a -> b -> c
errorNotSupported featureName sourcePos expr = error $ "Not supported: '" ++ featureName ++ "' at " ++ show sourcePos ++ " in\n" ++ show (ES3PP.prettyPrint expr)

foldStmts :: Show a => [ES3.Statement a] -> Exp (GenInfo, a) -> Exp (GenInfo, a)
foldStmts [] expr = expr
foldStmts [x] expr = fromStatement x expr
foldStmts (x:xs) expr = fromStatement x (foldStmts xs expr)

-- Doesn't carry context over from one statement to the next (good for branching)
parallelStmts :: Show a => a -> [ES3.Statement a] -> Exp (GenInfo, a) -> Exp (GenInfo, a)
parallelStmts _ [] expr = expr
parallelStmts z stmts expr = ETuple (gen z) $ expr : map (flip fromStatement $ empty z) stmts
    
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
           
fromStatement :: Show a => ES3.Statement a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
fromStatement (ES3.BlockStmt _ stmts) = foldStmts stmts
fromStatement (ES3.EmptyStmt _) = id
fromStatement (ES3.ExprStmt z e) = singleStmt (gen z) $ fromExpression e
-- TODO: The if/while/do conversion is hacky
fromStatement s@(ES3.IfStmt z pred' thenS elseS) = --chainExprs z (EArray (gen z) [fromExpression pred', ELit (gen z) (LitBoolean False)]) $ parallelStmts z [thenS, elseS]
    case pred' of
        ES3.InfixExpr _ ES3.OpStrictEq (ES3.PrefixExpr _ ES3.PrefixTypeof (ES3.VarRef _ (ES3.Id _ n))) (ES3.StringLit _ typeName) -> \e -> ETypeCase (src z) n type' (fromStatement thenS e) (fromStatement elseS e)
            where type' = Fix $ TBody $ case typeName of
                                            "string" -> TString
                                            "number" -> TNumber
                                            _ -> errorNotSupported ("type guard: " ++ typeName) z s
        _ -> chainExprs z (EArray (gen z) [fromExpression pred', ELit (gen z) (LitBoolean False)]) $ parallelStmts z [thenS, elseS]
fromStatement (ES3.IfSingleStmt z pred' thenS) = chainExprs z (EArray (gen z) [fromExpression pred', ELit (gen z) (LitBoolean False)]) $ fromStatement thenS
fromStatement (ES3.WhileStmt z pred' loopS) = chainExprs z (EArray (gen z) [fromExpression pred', ELit (gen z) (LitBoolean False)]) $ fromStatement loopS
fromStatement (ES3.DoWhileStmt z loopS pred') = chainExprs z (EArray (gen z) [fromExpression pred', ELit (gen z) (LitBoolean False)]) $ fromStatement loopS
fromStatement (ES3.BreakStmt _ _) = id -- TODO verify we can ignore this
fromStatement (ES3.ContinueStmt _ _) = id -- TODO verify we can ignore this
-- try/catch/finally are indepdendent branches that shouldn't be sharing context. catch is a like an
-- abstraction over the (optional) exception-bound name.
fromStatement (ES3.TryStmt z stmt mCatch mFinally) = chainExprs z catchExpr $ parallelStmts z ([stmt] ++ finallyS)
  where catchExpr = case mCatch of
                        Just (ES3.CatchClause _ (ES3.Id z' e) s) -> EAbs (gen z') [e] (fromStatement s $ empty z')
                        Nothing -> empty z
        finallyS = case mFinally of
                    Just f -> [f]
                    Nothing -> []
fromStatement (ES3.ThrowStmt _ _) = id
fromStatement s@(ES3.WithStmt z _ _) = errorNotSupported "with" z s
fromStatement s@(ES3.ForInStmt z _ _ _) = errorNotSupported "for .. in" z s
fromStatement (ES3.LabelledStmt _ _ s) = fromStatement s
fromStatement (ES3.ForStmt z init' test increment body) = case init' of
                                                           ES3.NoInit -> forBody
                                                           ES3.VarInit varDecls -> chainDecls varDecls . forBody
                                                           ES3.ExprInit expr -> chainExprs z (fromExpression expr) forBody
    where forBody = chainExprs z test'' rest
          test'' = case test of
                    Nothing -> EVar (gen z) poo
                    Just test' -> EArray (gen z) [fromExpression test', ELit (gen z) (LitBoolean False)]
          body' = fromStatement body
          rest = case increment of
                   Nothing -> body'
                   Just increment' -> chainExprs z (fromExpression increment') body'

fromStatement (ES3.SwitchStmt z switch cases) = chainExprs z (EArray (gen z) tests) . parallelStmts z $ concatMap getCaseBody cases
    where tests = fromExpression switch : mapMaybe (fmap fromExpression . getCaseTest) cases
          getCaseTest (ES3.CaseDefault _ _) = Nothing
          getCaseTest (ES3.CaseClause _ test' _) = Just test'
          getCaseBody (ES3.CaseDefault _ body') = body'
          getCaseBody (ES3.CaseClause _ _ body') = body'

fromStatement (ES3.VarDeclStmt _ decls) = chainDecls decls
fromStatement (ES3.FunctionStmt z name args stmts) = toNamedAbs z args stmts name
fromStatement (ES3.ReturnStmt z x) = EIndexAssign (gen z) (EVar (gen z) "return") (ELit (gen z) $ LitNumber 0)
                                     $ case x of
                                        Nothing -> ELit (gen z) LitUndefined
                                        Just x' -> fromExpression x'

-- | Creates an EAbs (function abstraction)
toAbs :: Show a => a -> [ES3.Id c] -> [ES3.Statement a] -> Exp (GenInfo, a)
toAbs z args stmts = EAbs (src z) ("this" : map ES3.unId args) body'
  -- TODO: this can lead to problems if "return" was never called (there's a partial function here - dereferencing array element 0)
  where body' = case any hasReturn stmts of
                 True -> ELet (gen z) "return" (EArray (gen z) []) $ foldStmts stmts $ (EIndex (gen z) (EVar (gen z) "return") (ELit (gen z) $ LitNumber 0))
                 False -> ELet (gen z) "return" (empty z) $ foldStmts stmts $ (ELit (gen z) LitUndefined)


hasReturn :: ES3.Statement a -> Bool
hasReturn (ES3.BlockStmt _ stmts) = any hasReturn stmts
hasReturn (ES3.EmptyStmt _) = False
hasReturn (ES3.ExprStmt _ _) = False
hasReturn (ES3.IfStmt _ _ thenS elseS) = any hasReturn [thenS, elseS]
hasReturn (ES3.IfSingleStmt _ _ thenS) = hasReturn thenS
hasReturn (ES3.WhileStmt _ _ loopS) = hasReturn loopS
hasReturn (ES3.DoWhileStmt _ loopS _) = hasReturn loopS
hasReturn (ES3.BreakStmt _ _) = False
hasReturn (ES3.ContinueStmt _ _) = False
hasReturn (ES3.TryStmt _ stmt mCatch mFinally) = any hasReturn (stmt : finallyS ++ catchS)
  where catchS = case mCatch of
                  Just (ES3.CatchClause _ _ s) -> [s]
                  Nothing -> []
        finallyS = case mFinally of
                    Just f -> [f]
                    Nothing -> []
hasReturn (ES3.ThrowStmt _ _) = False
hasReturn (ES3.WithStmt _ _ s) = hasReturn s
hasReturn (ES3.ForInStmt _ _ _ s) = hasReturn s
hasReturn (ES3.LabelledStmt _ _ s) = hasReturn s
hasReturn (ES3.ForStmt _ _ _ _ body) = hasReturn body
hasReturn (ES3.SwitchStmt _ _ cases) = and $ map fromCase cases
  where fromCase (ES3.CaseClause _ _ s) = any hasReturn s
        fromCase (ES3.CaseDefault _ stmts) = any hasReturn stmts
hasReturn (ES3.VarDeclStmt _ _) = False
hasReturn (ES3.FunctionStmt _ _ _ _) = False
hasReturn (ES3.ReturnStmt _ _) = True

                                
addDecl z name expr = Log.trace ("addDecl: " ++ show res) res
    where res = mapTopAnnotation (const $ decl z name) expr 
                                 
toNamedAbs :: Show a => a -> [ES3.Id c] -> [ES3.Statement a] -> ES3.Id a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
toNamedAbs z args stmts (ES3.Id zn name) letBody = let abs' = addDecl zn name $ toAbs z args stmts
                                                   in ELet (gen z) name abs' letBody

chainDecls :: Show a => [ES3.VarDecl a] -> Exp (GenInfo, a) -> Exp (GenInfo, a)
chainDecls [] k = k
chainDecls (ES3.VarDecl z' (ES3.Id _ name) Nothing:xs) k = ELet (gen z') name (ELit (gen z') LitUndefined) (chainDecls xs k)
chainDecls (ES3.VarDecl z' (ES3.Id _ name) (Just v):xs) k = ELet (gen z') name (addDecl z' name $ fromExpression v) (chainDecls xs k)

makeThis :: Show a => a -> Exp a
makeThis z = ELit z $ LitNull -- TODO should be undefined

fromExpression :: Show a => ES3.Expression a -> Exp (GenInfo, a)
fromExpression (ES3.StringLit z s) = ELit (src z) $ LitString s
fromExpression (ES3.RegexpLit z s g i) = ELit (src z) $ LitRegex s g i
fromExpression (ES3.BoolLit z s) = ELit (src z) $ LitBoolean s
fromExpression (ES3.IntLit z s) = ELit (src z) (LitNumber $ fromIntegral s)
fromExpression (ES3.NumLit z s) = ELit (src z) $ LitNumber s
fromExpression (ES3.NullLit z) = ELit (src z) LitNull
fromExpression (ES3.ArrayLit z exprs) = EArray (src z) $ map fromExpression exprs
fromExpression (ES3.ObjectLit z props) = let stringProps = map (fromPropString . fst) props
                                         in if all (\x -> x /= Nothing) stringProps
                                            then EStringMap (src z) $ zip (catMaybes stringProps) (map (fromExpression . snd) props) 
                                            else ERow (src z) False $ map (fromProp *** fromExpression) props
fromExpression (ES3.BracketRef z arrExpr indexExpr) = EIndex (src z) (fromExpression arrExpr) (fromExpression indexExpr)
fromExpression (ES3.VarRef z name) = EVar (src z) $ ES3.unId name
fromExpression (ES3.CondExpr z ePred eThen eElse) = EIfThenElse (src z) (fromExpression ePred) (fromExpression eThen) (fromExpression eElse)
fromExpression (ES3.CallExpr z expr argExprs) =
  -- Instead of simply translating, here we also do some specific simplification by defining
  -- (adding an ELet) for the object expression if the function is a method call.
  -- The idea is to prevent duplicate expressions in the output tree (<complicated expr>.method
  -- (<complicated expr>, ...)) by binding the object expression to '__obj__'.
  -- So that we get: let __obj__ = <complicated expr> in __obj__.method(__obj__, ...)
  case expr of
   ES3.DotRef z' varExpr@(ES3.VarRef _ _) (ES3.Id _ propName) -> appExpr (Just propName) (EProp (src z') var propName) var
     where var = fromExpression varExpr
   ES3.DotRef z' objExpr (ES3.Id _ propName) -> ELet (gen z') objVarName obj $ appExpr (Just propName) (EProp (src z') objVar propName) objVar
     where obj = fromExpression objExpr
           objVar = EVar (gen z') objVarName
           objVarName = "_/obj/_"
   _ -> appExpr Nothing (fromExpression expr) (ELit (gen z) LitUndefined)
  where appExpr (Just "call") _ obj = (EApp (src z) obj (map fromExpression argExprs)) -- TODO: may be wrong if object expression is not a function!
        appExpr _ funcExpr thisExpr = (EApp (src z) funcExpr (thisExpr : map fromExpression argExprs))
  --error $ "Assetion failed: expecting at least 'this'"
fromExpression (ES3.AssignExpr z op target expr) = assignExpr
  where sz = src z
        (assignExpr, oldValue) = case target of
          ES3.LVar _ name -> (assignToVar z name value, EVar sz name)
          ES3.LDot _ objExpr name -> (assignToProperty z objExpr name value, EProp sz (fromExpression objExpr) name)
          ES3.LBracket _ objExpr idxExpr -> (assignToIndex z objExpr idxExpr value, EIndex sz (fromExpression objExpr) (fromExpression idxExpr))
        expr' = fromExpression expr
        value = case op of
          ES3.OpAssign -> expr'
          ES3.OpAssignAdd -> applyOpFunc z ES3.OpAdd [oldValue, expr']
          ES3.OpAssignSub -> applyOpFunc z ES3.OpSub [oldValue, expr']
          ES3.OpAssignMul -> applyOpFunc z ES3.OpMul [oldValue, expr']
          ES3.OpAssignDiv -> applyOpFunc z ES3.OpDiv [oldValue, expr']
          ES3.OpAssignMod -> applyOpFunc z ES3.OpMod [oldValue, expr']
          ES3.OpAssignLShift   -> applyOpFunc z ES3.OpLShift   [oldValue, expr']
          ES3.OpAssignSpRShift -> applyOpFunc z ES3.OpSpRShift [oldValue, expr']
          ES3.OpAssignZfRShift -> applyOpFunc z ES3.OpZfRShift [oldValue, expr']
          ES3.OpAssignBAnd     -> applyOpFunc z ES3.OpBAnd     [oldValue, expr']
          ES3.OpAssignBXor     -> applyOpFunc z ES3.OpBXor     [oldValue, expr']
          ES3.OpAssignBOr      -> applyOpFunc z ES3.OpBOr      [oldValue, expr']

fromExpression (ES3.FuncExpr z Nothing     argNames stmts) = toAbs z argNames stmts
fromExpression (ES3.FuncExpr z (Just name) argNames stmts) = toNamedAbs z argNames stmts name (EVar (gen z) $ ES3.unId name)

fromExpression e@(ES3.ListExpr z exprs) =
    case exprs of
      [] -> errorNotSupported "empty list (,) expression" z e
      [x] -> fromExpression x
      -- Should the let here use an allocated name here?
      xs -> ELet (gen z) poo (ETuple (gen z) (tail exprs')) (head exprs')
          where exprs' = reverse . map fromExpression $ xs
fromExpression (ES3.ThisRef z) = EVar (src z) "this"
fromExpression (ES3.DotRef z expr propId) = EProp (src z) (fromExpression expr) (ES3.unId propId)
fromExpression (ES3.NewExpr z expr argExprs) = ENew (src z) (fromExpression expr) (map fromExpression argExprs)
--  ELet z "__this__" (ERow z True []) (ELet z "_bla_" (EApp z (fromExpression expr) ((EVar z "__this__") : map fromExpression argExprs)) (EVar z "__this__"))
fromExpression e@(ES3.PrefixExpr z op expr) =
  case op of
    -- prefix +/- are converted to 0-x and 0+x
    ES3.PrefixPlus -> EApp (gen z) (opFunc z ES3.OpAdd) [makeThis (gen z), ELit (gen z) $ LitNumber 0, fromExpression expr]
    ES3.PrefixMinus -> EApp (gen z) (opFunc z ES3.OpSub) [makeThis (gen z), ELit (gen z) $ LitNumber 0, fromExpression expr]
    -- delete, void unsupported
    ES3.PrefixVoid -> errorNotSupported "void" z e
    ES3.PrefixDelete -> errorNotSupported "delete" z e
    -- all the rest are expected to exist as unary builtin functions
    _ -> EApp (src z) (EVar (gen z) $ show . ES3PP.prettyPrint $ op) [makeThis (gen z), fromExpression expr]
fromExpression (ES3.InfixExpr z op e1 e2) = EApp (gen z) (EVar (gen z) $ show . ES3PP.prettyPrint $ op) [makeThis (gen z), fromExpression e1, fromExpression e2]
fromExpression (ES3.UnaryAssignExpr z op (ES3.LVar _ name)) = assignToVar z name $ addConstant z op (EVar (src z) name)
fromExpression (ES3.UnaryAssignExpr z op (ES3.LDot _ objExpr name)) = assignToProperty z objExpr name $ addConstant z op (EProp (src z) objExpr' name)
  where objExpr' = fromExpression objExpr
fromExpression (ES3.UnaryAssignExpr z op (ES3.LBracket _ objExpr idxExpr)) = assignToIndex z objExpr idxExpr $ addConstant z op (EIndex (src z) objExpr' idxExpr')
  where objExpr' = fromExpression objExpr
        idxExpr' = fromExpression idxExpr

opFunc :: a -> ES3.InfixOp -> Exp (GenInfo, a)
opFunc z op = EVar (gen z) $ show . ES3PP.prettyPrint $ op

applyOpFunc :: Show a => a -> ES3.InfixOp -> [Exp (GenInfo, a)] -> Exp (GenInfo, a)
applyOpFunc z op exprs = EApp (gen z) (opFunc z op) (makeThis (gen z) : exprs)

-- TODO: the translation results in equivalent types, but currently ignore pre vs. postfix so the data flow is wrong.
addConstant :: Show a => a -> ES3.UnaryAssignOp -> Exp (GenInfo, a) -> Exp (GenInfo, a)
addConstant z op expr = EApp (gen z) (opFunc z ES3.OpAdd) [makeThis (gen z), expr, ELit (gen z) $ LitNumber x]
  where x = case op of
             ES3.PrefixInc -> 1
             ES3.PrefixDec -> -1
             ES3.PostfixInc -> 1
             ES3.PostfixDec -> -1

assignToVar :: Show a => a -> EVarName -> Exp (GenInfo, a) -> Exp (GenInfo, a)
assignToVar z name expr = EAssign (src z) name expr (EVar (src z) name)

assignToProperty :: Show a => a -> ES3.Expression a -> EPropName -> Exp (GenInfo, a) -> Exp (GenInfo, a)
assignToProperty  z objExpr name expr = EPropAssign (src z) objExpr' name expr (EProp (src z) objExpr' name)
  where objExpr' = fromExpression objExpr

assignToIndex :: Show a => a -> ES3.Expression a  -> ES3.Expression a -> Exp (GenInfo, a) -> Exp (GenInfo, a)
assignToIndex z objExpr idxExpr expr = EIndexAssign (src z) objExpr' idxExpr' expr (EIndex (src z) objExpr' idxExpr')
  where objExpr' = fromExpression objExpr
        idxExpr' = fromExpression idxExpr


fromProp :: ES3.Prop a -> String
fromProp (ES3.PropId _ (ES3.Id _ x)) = x
fromProp (ES3.PropString _ x) = x
fromProp (ES3.PropNum _ x) = show x

fromPropString :: ES3.Prop a -> Maybe String
fromPropString (ES3.PropString _ x) = Just x
fromPropString _ = Nothing
                  
-- -- ------------------------------------------------------------------------

translate :: [ES3.Statement Pos.SourcePos] -> Exp (GenInfo, Pos.SourcePos)
translate js = ELet (gen pos) poo (empty pos) $ foldStmts js $ EVar (gen pos) poo
  where pos = Pos.initialPos "<global>"
