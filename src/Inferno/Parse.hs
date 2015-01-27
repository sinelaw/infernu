module Inferno.Parse
       (translate)
       where

import           Control.Arrow                    ((***))
import           Data.Maybe                       (mapMaybe)
import qualified Language.ECMAScript3.PrettyPrint as ES3PP
import qualified Language.ECMAScript3.Syntax      as ES3
import           Inferno.Types
import qualified Text.Parsec.Pos                  as Pos

-- | A 'magic' impossible variable name that can never occur in valid JS syntax.
poo :: EVarName
poo = "_/_"

-- | A dummy expression that does nothing (but has a type).
empty :: a -> Exp a
empty z = ELit z LitUndefined -- EVar z poo

errorNotSupported :: (Show a, ES3PP.Pretty b) => String -> a -> b -> c
errorNotSupported featureName sourcePos expr = error $ "Not supported: '" ++ featureName ++ "' at " ++ show sourcePos ++ " in\n" ++ show (ES3PP.prettyPrint expr)

foldStmts :: Show a => [ES3.Statement a] -> Exp a -> Exp a
foldStmts [] k = k
foldStmts [x] k = fromStatement x k
foldStmts (x:xs) k = fromStatement x (foldStmts xs k)

chainExprs :: Show a => a -> Exp a -> (Exp a -> Exp a) -> Exp a -> Exp a
chainExprs a init' getK k = ELet a poo init' $ getK k

singleStmt :: Show a => a -> Exp a -> Exp a -> Exp a
singleStmt a exp' = ELet a poo exp'

fromStatement :: Show a => ES3.Statement a -> Exp a -> Exp a
fromStatement (ES3.BlockStmt _ stmts) = foldStmts stmts
fromStatement (ES3.EmptyStmt _) = id
fromStatement (ES3.ExprStmt z e) = singleStmt z $ fromExpression e
-- TODO: The if/while/do conversion is hacky
fromStatement (ES3.IfStmt z pred' thenS elseS) = chainExprs z (EArray z [fromExpression pred', ELit z (LitBoolean False)]) $ foldStmts [thenS, elseS]
fromStatement (ES3.IfSingleStmt z pred' thenS) = chainExprs z (EArray z [fromExpression pred', ELit z (LitBoolean False)]) $ fromStatement thenS
fromStatement (ES3.WhileStmt z pred' loopS) = chainExprs z (EArray z [fromExpression pred', ELit z (LitBoolean False)]) $ fromStatement loopS
fromStatement (ES3.DoWhileStmt z loopS pred') = chainExprs z (EArray z [fromExpression pred', ELit z (LitBoolean False)]) $ fromStatement loopS
fromStatement (ES3.BreakStmt _ _) = id -- TODO verify we can ignore this
fromStatement (ES3.ContinueStmt _ _) = id -- TODO verify we can ignore this
fromStatement (ES3.TryStmt _ stmt mCatch mFinally) = foldStmts $ [stmt] ++ catchS ++ finallyS
  where catchS = case mCatch of
                  Just (ES3.CatchClause _ _ s) -> [s]
                  Nothing -> []
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
                    Nothing -> EVar z poo
                    Just test' -> EArray z [fromExpression test', ELit z (LitBoolean False)]
          body' = fromStatement body
          rest = case increment of
                   Nothing -> body'
                   Just increment' -> chainExprs z (fromExpression increment') body'

fromStatement (ES3.SwitchStmt z switch cases) = chainExprs z (EArray z tests) . foldStmts $ concatMap getCaseBody cases
    where tests = fromExpression switch : mapMaybe (fmap fromExpression . getCaseTest) cases
          getCaseTest (ES3.CaseDefault _ _) = Nothing
          getCaseTest (ES3.CaseClause _ test' _) = Just test'
          getCaseBody (ES3.CaseDefault _ body') = body'
          getCaseBody (ES3.CaseClause _ _ body') = body'

fromStatement (ES3.VarDeclStmt _ decls) = chainDecls decls
fromStatement (ES3.FunctionStmt z name args stmts) = toNamedAbs z args stmts name
fromStatement (ES3.ReturnStmt z x) = EIndexAssign z (EVar z "return") (ELit z $ LitNumber 0)
                                     $ case x of
                                        Nothing -> ELit z LitUndefined
                                        Just x' -> fromExpression x'

-- | Creates an EAbs (function abstraction)
toAbs :: Show a => a -> [ES3.Id c] -> [ES3.Statement a] -> Exp a
toAbs z args stmts = EAbs z ("this" : map ES3.unId args) body'
  -- TODO: this can lead to problems if "return" was never called (there's a partial function here - dereferencing array element 0)
  where body' = case any hasReturn stmts of
                 True -> ELet z "return" (EArray z []) $ foldStmts stmts $ (EIndex z (EVar z "return") (ELit z $ LitNumber 0))
                 False -> ELet z "return" (empty z) $ foldStmts stmts $ (ELit z LitUndefined)


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

toNamedAbs :: Show a => a -> [ES3.Id c] -> [ES3.Statement a] -> ES3.Id b -> Exp a -> Exp a
toNamedAbs z args stmts name letBody = let abs' = toAbs z args stmts
                                       in ELet z (ES3.unId name) abs' letBody

chainDecls :: Show a => [ES3.VarDecl a] -> Exp a -> Exp a
chainDecls [] k = k
chainDecls (ES3.VarDecl z' (ES3.Id _ name) Nothing:xs) k = ELet z' name (ELit z' LitUndefined) (chainDecls xs k)
chainDecls (ES3.VarDecl z' (ES3.Id _ name) (Just v):xs) k = ELet z' name (fromExpression v) (chainDecls xs k)

makeThis :: Show a => a -> Exp a
makeThis z = ELit z $ LitNull -- TODO should be undefined

fromExpression :: Show a => ES3.Expression a -> Exp a
fromExpression (ES3.StringLit z s) = ELit z $ LitString s
fromExpression (ES3.RegexpLit z s g i) = ELit z $ LitRegex s g i
fromExpression (ES3.BoolLit z s) = ELit z $ LitBoolean s
fromExpression (ES3.IntLit z s) = ELit z (LitNumber $ fromIntegral s)
fromExpression (ES3.NumLit z s) = ELit z $ LitNumber s
fromExpression (ES3.NullLit z) = ELit z LitNull
fromExpression (ES3.ArrayLit z exprs) = EArray z $ map fromExpression exprs
fromExpression (ES3.ObjectLit z props) = ERow z False $ map (fromProp *** fromExpression) props
fromExpression (ES3.BracketRef z arrExpr indexExpr) = EIndex z (fromExpression arrExpr) (fromExpression indexExpr)
fromExpression (ES3.VarRef z name) = EVar z $ ES3.unId name
fromExpression (ES3.CondExpr z ePred eThen eElse) = EIfThenElse z (fromExpression ePred) (fromExpression eThen) (fromExpression eElse)
fromExpression (ES3.CallExpr z expr argExprs) =
  -- Instead of simply translating, here we also do some specific simplification by defining
  -- (adding an ELet) for the object expression if the function is a method call.
  -- The idea is to prevent duplicate expressions in the output tree (<complicated expr>.method
  -- (<complicated expr>, ...)) by binding the object expression to '__obj__'.
  -- So that we get: let __obj__ = <complicated expr> in __obj__.method(__obj__, ...)
  case expr of
   ES3.DotRef z' varExpr@(ES3.VarRef _ _) (ES3.Id _ propName) -> appExpr (Just propName) (EProp z' var propName) var
     where var = fromExpression varExpr
   ES3.DotRef z' objExpr (ES3.Id _ propName) -> EApp z (EAbs z [objVarName] $ appExpr (Just propName) (EProp z' objVar propName) objVar) [obj]
     where obj = fromExpression objExpr -- ensure monomorphic type (prevent generalization)
           objVar = EVar z objVarName
           objVarName = "__obj__"
   _ -> appExpr Nothing (fromExpression expr) (ELit z LitUndefined)
  where appExpr (Just "call") _ obj = (EApp z obj (map fromExpression argExprs)) -- TODO: may be wrong if object expression is not a function!
        appExpr _ funcExpr thisExpr = (EApp z funcExpr (thisExpr : map fromExpression argExprs))
  --error $ "Assetion failed: expecting at least 'this'"
fromExpression (ES3.AssignExpr z op target expr) = assignExpr
  where (assignExpr, oldValue) = case target of
          ES3.LVar _ name -> (assignToVar z name value, EVar z name)
          ES3.LDot _ objExpr name -> (assignToProperty z objExpr name value, EProp z (fromExpression objExpr) name)
          ES3.LBracket _ objExpr idxExpr -> (assignToIndex z objExpr idxExpr value, EIndex z (fromExpression objExpr) (fromExpression idxExpr))
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
fromExpression (ES3.FuncExpr z (Just name) argNames stmts) = toNamedAbs z argNames stmts name (EVar z $ ES3.unId name)

fromExpression e@(ES3.ListExpr z exprs) =
    case exprs of
      [] -> errorNotSupported "empty list (,) expression" z e
      [x] -> fromExpression x
      -- Should the let here use an allocated name here?
      xs -> ELet z poo (ETuple z (tail exprs')) (head exprs')
          where exprs' = reverse . map fromExpression $ xs
fromExpression (ES3.ThisRef z) = EVar z "this"
fromExpression (ES3.DotRef z expr propId) = EProp z (fromExpression expr) (ES3.unId propId)
fromExpression (ES3.NewExpr z expr argExprs) = ENew z (fromExpression expr) (map fromExpression argExprs)
--  ELet z "__this__" (ERow z True []) (ELet z "_bla_" (EApp z (fromExpression expr) ((EVar z "__this__") : map fromExpression argExprs)) (EVar z "__this__"))
fromExpression e@(ES3.PrefixExpr z op expr) =
  case op of
    -- prefix +/- are converted to 0-x and 0+x
    ES3.PrefixPlus -> EApp z (opFunc z ES3.OpAdd) [makeThis z, ELit z $ LitNumber 0, fromExpression expr]
    ES3.PrefixMinus -> EApp z (opFunc z ES3.OpSub) [makeThis z, ELit z $ LitNumber 0, fromExpression expr]
    -- delete, void unsupported
    ES3.PrefixVoid -> errorNotSupported "void" z e
    ES3.PrefixDelete -> errorNotSupported "delete" z e
    -- all the rest are expected to exist as unary builtin functions
    _ -> EApp z (EVar z $ show . ES3PP.prettyPrint $ op) [makeThis z, fromExpression expr]
fromExpression (ES3.InfixExpr z op e1 e2) = EApp z (EVar z $ show . ES3PP.prettyPrint $ op) [makeThis z, fromExpression e1, fromExpression e2]
fromExpression (ES3.UnaryAssignExpr z op (ES3.LVar _ name)) = assignToVar z name $ addConstant z op (EVar z name)
fromExpression (ES3.UnaryAssignExpr z op (ES3.LDot _ objExpr name)) = assignToProperty z objExpr name $ addConstant z op (EProp z objExpr' name)
  where objExpr' = fromExpression objExpr
fromExpression (ES3.UnaryAssignExpr z op (ES3.LBracket _ objExpr idxExpr)) = assignToIndex z objExpr idxExpr $ addConstant z op (EIndex z objExpr' idxExpr')
  where objExpr' = fromExpression objExpr
        idxExpr' = fromExpression idxExpr

opFunc :: a -> ES3.InfixOp -> Exp a
opFunc z op = EVar z $ show . ES3PP.prettyPrint $ op

applyOpFunc :: Show a => a -> ES3.InfixOp -> [Exp a] -> Exp a
applyOpFunc z op exprs = EApp z (opFunc z op) (makeThis z : exprs)

-- TODO: the translation results in equivalent types, but currently ignore pre vs. postfix so the data flow is wrong.
addConstant :: Show a => a -> ES3.UnaryAssignOp -> Exp a -> Exp a
addConstant z op expr = EApp z (opFunc z ES3.OpAdd) [makeThis z, expr, ELit z $ LitNumber x]
  where x = case op of
             ES3.PrefixInc -> 1
             ES3.PrefixDec -> -1
             ES3.PostfixInc -> 1
             ES3.PostfixDec -> -1

assignToVar :: Show a => a -> EVarName -> Exp a -> Exp a
assignToVar z name expr = EAssign z name expr (EVar z name)

assignToProperty :: Show a => a -> ES3.Expression a -> EPropName -> Exp a -> Exp a
assignToProperty  z objExpr name expr = EPropAssign z objExpr' name expr (EProp z objExpr' name)
  where objExpr' = fromExpression objExpr

assignToIndex :: Show a => a -> ES3.Expression a  -> ES3.Expression a -> Exp a -> Exp a
assignToIndex z objExpr idxExpr expr = EIndexAssign z objExpr' idxExpr' expr (EIndex z objExpr' idxExpr')
  where objExpr' = fromExpression objExpr
        idxExpr' = fromExpression idxExpr


fromProp :: ES3.Prop a -> String
fromProp (ES3.PropId _ (ES3.Id _ x)) = x
fromProp (ES3.PropString _ x) = x
fromProp (ES3.PropNum _ x) = show x

-- -- ------------------------------------------------------------------------

translate :: [ES3.Statement Pos.SourcePos] -> Exp Pos.SourcePos
translate js = ELet pos poo (empty pos) $ foldStmts js $ EVar pos poo
  where pos = Pos.initialPos "<global>"
