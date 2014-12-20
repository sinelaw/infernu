module SafeJS.Parse
       (translate)
       where

import           Control.Arrow                    ((***))
import qualified Language.ECMAScript3.PrettyPrint as ES3PP
import qualified Language.ECMAScript3.Syntax      as ES3
import qualified Text.Parsec.Pos                  as Pos
import Data.Maybe (mapMaybe)
import           SafeJS.Types

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
-- $ (ES3.Id z "this" : args)
fromStatement (ES3.FunctionStmt z name args stmts) = toNamedAbs z args stmts name
-- TODO: return statements must be added to the core language to be handled correctly.
fromStatement (ES3.ReturnStmt z x) = \k -> ELet z poo k $ maybe (empty z) fromExpression x

-- | Creates an EAbs (function abstraction)
toAbs :: Show a => a -> [ES3.Id c] -> [ES3.Statement a] -> Exp a
toAbs z args stmts = EAbs z ("this" : map ES3.unId args) body
  where body = foldStmts stmts $ ECloseRow z "this"

toNamedAbs :: Show a => a -> [ES3.Id c] -> [ES3.Statement a] -> ES3.Id b -> Exp a -> Exp a
toNamedAbs z args stmts name letBody = let abs' = toAbs z args stmts
                                       in ELet z (ES3.unId name) abs' letBody

chainDecls :: Show a => [ES3.VarDecl a] -> Exp a -> Exp a
chainDecls [] k = k
chainDecls (ES3.VarDecl z' (ES3.Id _ name) Nothing:xs) k = ELet z' name (ELit z' LitUndefined) (chainDecls xs k)
chainDecls (ES3.VarDecl z' (ES3.Id _ name) (Just v):xs) k = ELet z' name (fromExpression v) (chainDecls xs k)


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
fromExpression (ES3.CallExpr z expr argExprs) = EApp z (fromExpression expr) (map fromExpression $ thisArg : argExprs)
                                                where thisArg = case expr of
                                                        ES3.DotRef _ objExpr _ -> objExpr
                                                        _ -> ES3.NullLit z -- TODO should be 'undefined'
                                                --error $ "Assetion failed: expecting at least 'this'"
fromExpression (ES3.AssignExpr z ES3.OpAssign (ES3.LVar _ name) expr) = EAssign z name (fromExpression expr) (EVar z name)
fromExpression (ES3.AssignExpr z ES3.OpAssign (ES3.LDot _ objExpr name) expr) = EPropAssign z objExpr' name (fromExpression expr) (EProp z objExpr' name)
  where objExpr' = fromExpression objExpr
fromExpression (ES3.AssignExpr z ES3.OpAssign (ES3.LBracket _ objExpr idxExpr) expr) = EIndexAssign z objExpr' idxExpr' (fromExpression expr) (EIndex z objExpr' idxExpr')
  where objExpr' = fromExpression objExpr
        idxExpr' = fromExpression idxExpr
fromExpression (ES3.FuncExpr z Nothing     argNames stmts) = toAbs z argNames stmts
fromExpression (ES3.FuncExpr z (Just name) argNames stmts) = toNamedAbs z argNames stmts name (EVar z $ ES3.unId name)
fromExpression (ES3.ListExpr z exprs) =
    case exprs of
      [] -> error "Unexpected empty list expression"
      [x] -> fromExpression x
      -- Should the let here use an allocated name here?
      xs -> ELet z poo (ETuple z (tail exprs')) (head exprs')
          where exprs' = reverse . map fromExpression $ xs
fromExpression (ES3.ThisRef z) = EVar z "this"
fromExpression (ES3.DotRef z expr propId) = EProp z (fromExpression expr) (ES3.unId propId)
-- new Constr(args..) = var this = { ... }, Constr(this, args..), return this
-- TODO: this doesn't work. The only thing that collapses a variable to be of a more specific type is when we assign to it.
-- So to get the type of 'this' we have no choice but to assign to - which is why we must return it from every function!
fromExpression (ES3.NewExpr z expr argExprs) = ELet z tempThis (EArray z []) appExpr
  where appExpr = ELet z poo app crap
        crap = EIndex z (EVar z tempThis) (ELit z $ LitNumber 0)
        app = EApp z (fromExpression expr) (crap : map fromExpression argExprs)
        tempThis = "__this__" -- TODO replace with globally unique name
fromExpression e = error $ "Not implemented: expression: " ++ show e ++ ": " ++ show (ES3PP.prettyPrint e)

fromProp :: ES3.Prop a -> String
fromProp (ES3.PropId _ (ES3.Id _ x)) = x
fromProp (ES3.PropString _ x) = x
fromProp (ES3.PropNum _ x) = show x

-- -- ------------------------------------------------------------------------

translate :: [ES3.Statement Pos.SourcePos] -> Exp Pos.SourcePos
translate js = (foldStmts $ js) (empty $ Pos.initialPos "<global>")


-- ex :: Body (Expr ()) -> Expr ()
-- ex expr = Expr expr ()


-- st :: Body (Expr ()) -> Statement (Expr ())
-- st expr = Expression $ ex expr


-- fromStatement :: ES3.Statement a -> Statement (Expr ())
-- fromStatement (ES3.BlockStmt _ stmts) = Block $ map fromStatement stmts
-- fromStatement (ES3.EmptyStmt _) = Empty
-- fromStatement (ES3.ExprStmt _ e) = Expression $ fromExpression e
-- fromStatement (ES3.IfStmt _ pred' thenS elseS) = IfThenElse (fromExpression pred') (fromStatement thenS) (fromStatement elseS)
-- fromStatement (ES3.IfSingleStmt _ pred' thenS) = IfThenElse (fromExpression pred') (fromStatement thenS) Empty
-- fromStatement (ES3.WhileStmt _ pred' stmt) = While (fromExpression pred') (fromStatement stmt)
-- fromStatement (ES3.ReturnStmt _ x) = Return . fmap fromExpression $ x
-- --fromStatement (ES3.LabelledStmt _ _ s) =
-- --fromStatement (ES3.ForInStmt _ x) =
-- fromStatement (ES3.ForStmt _ forInit pred' incr stmt) =
--     Block [ fromForInit forInit
--           , While (maybe (ex $ LitBoolean True) fromExpression pred')
--                       (Block $ fromStatement stmt : incr'')
--           ]
--     where incr'' = maybe [] (\x -> [Expression $ fromExpression x]) incr
-- fromStatement (ES3.VarDeclStmt _ decls) = Block $ map fromVarDecl decls
-- fromStatement (ES3.FunctionStmt _ name args stmts) = Expression . ex $ LitFunc (Just . ES3.unId $ name) (map ES3.unId args) (concatMap collectVarDecls stmts) funcBody
--     where funcBody = Block $ map fromStatement stmts
-- fromStatement s = error $ "Not implemented statement: " ++ show (ES3PP.prettyPrint s)


-- collectVarDecls :: ES3.Statement a -> [String]
-- collectVarDecls (ES3.EmptyStmt _) = []
-- collectVarDecls (ES3.BlockStmt _ stmts) = concatMap collectVarDecls stmts
-- collectVarDecls (ES3.ExprStmt _ _) = []
-- collectVarDecls (ES3.IfStmt _ _ s1 s2) = (collectVarDecls s1) ++ (collectVarDecls s2)
-- collectVarDecls (ES3.IfSingleStmt _ _ s) = collectVarDecls s
-- collectVarDecls (ES3.SwitchStmt _ _ cases) = concatMap collectVarDecls $ concatMap getCaseStatements cases
-- collectVarDecls (ES3.WhileStmt _ _ s) = collectVarDecls s
-- collectVarDecls (ES3.DoWhileStmt _ s _) = collectVarDecls s
-- collectVarDecls (ES3.BreakStmt _ _) = []
-- collectVarDecls (ES3.ContinueStmt _ _) = []
-- collectVarDecls (ES3.LabelledStmt _ _ s) = collectVarDecls s
-- collectVarDecls (ES3.ForInStmt _ forInInit' _ s) = forInVars ++ collectVarDecls s
--     where forInVars = case forInInit' of
--                         ES3.ForInVar (ES3.Id _ name) -> [name]
--                         _ -> []
-- collectVarDecls (ES3.ForStmt _ forInit' _ _ s) = forVars ++ collectVarDecls s
--     where forVars = case forInit' of
--                       ES3.VarInit varDecls -> map getVarName varDecls
--                       _ -> []
-- collectVarDecls (ES3.TryStmt _ sTry msCatch msFinally) =
--     collectVarDecls sTry ++ maybe [] (collectVarDecls . getCatchStatement) msCatch ++ maybe [] collectVarDecls msFinally
--     where getCatchStatement (ES3.CatchClause _ _ s') = s'
-- collectVarDecls (ES3.ThrowStmt _ _) = []
-- collectVarDecls (ES3.ReturnStmt _ _) = []
-- collectVarDecls (ES3.WithStmt _ _ s) = collectVarDecls s
-- collectVarDecls (ES3.VarDeclStmt _ vars) = map getVarName vars
-- collectVarDecls (ES3.FunctionStmt _ (ES3.Id _ funcName) _ _) = [funcName]

-- getVarName :: ES3.VarDecl a -> String
-- getVarName (ES3.VarDecl _ (ES3.Id _ name) _) = name

-- getCaseStatements :: ES3.CaseClause a -> [ES3.Statement a]
-- getCaseStatements (ES3.CaseClause _ _ stmts) = stmts
-- getCaseStatements (ES3.CaseDefault _ stmts) = stmts

-- fromVarDecl :: ES3.VarDecl a -> Statement (Expr ())
-- fromVarDecl (ES3.VarDecl _ (ES3.Id _ varName) assignRValue) = assignS
--     where assignS = case assignRValue of
--                       Nothing -> Empty
--                       Just ex' -> Expression . ex $ Assign (ex $ Var varName) (fromExpression ex')

-- fromForInit :: ES3.ForInit a -> Statement (Expr ())
-- fromForInit ES3.NoInit = Empty
-- fromForInit (ES3.VarInit varDecls) = Block $ map fromVarDecl varDecls
-- fromForInit (ES3.ExprInit expr) = Expression $ fromExpression expr
-- --fromStatement

-- -- fromVarDecl :: ES3.VarDecl a -> Statement (Expr ())
-- -- fromVarDecl (VarDecl _ (Id _ name) expr) =
-- --     case expr of
-- --       Nothing ->

-- fromExpression :: ES3.Expression a -> Expr ()
-- fromExpression es3x =
--     ex $ case es3x of
--       ES3.StringLit _ s -> LitString s
--       ES3.RegexpLit _ s _ _ -> LitRegex s
--       ES3.NumLit _ x -> LitNumber x
--       ES3.IntLit _ x -> LitNumber $ fromIntegral x
--       ES3.BoolLit _ x -> LitBoolean x
--       ES3.ArrayLit _ xs -> LitArray $ map fromExpression xs
--       ES3.ObjectLit _ props -> LitObject $ map (fromProp *** fromExpression) props
--       ES3.FuncExpr _ name argNames stmts -> LitFunc (fmap ES3.unId name) (map ES3.unId argNames) (concatMap collectVarDecls stmts) funcBody
--           where funcBody = Block $ map fromStatement stmts
--       ES3.VarRef _ name -> Var $ ES3.unId name
--       ES3.DotRef _ expr name -> Property (fromExpression expr) (ES3.unId name)
--       ES3.AssignExpr _ ES3.OpAssign lvalue expr -> Assign (fromLValue lvalue) (fromExpression expr)
--       ES3.CallExpr _ expr argExprs -> Call (fromExpression expr) $ map fromExpression argExprs
--       _ -> error $ "not implemented: " ++ show (ES3PP.prettyPrint es3x)

-- fromLValue :: ES3.LValue a -> Expr ()
-- fromLValue (ES3.LVar _ name) = ex $ Var name
-- fromLValue (ES3.LDot _ expr str) = ex $ Property (fromExpression expr) str
-- fromLValue (ES3.LBracket _ x y) = ex $ Index (fromExpression x) (fromExpression y)



