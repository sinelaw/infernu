module Parse where

import qualified Language.ECMAScript3.PrettyPrint as ES3PP
import qualified Language.ECMAScript3.Syntax as ES3
import qualified Language.ECMAScript3.Parser as ES3Parser
import Infer

-- | A 'magic' impossible variable name that can never occur in valid JS syntax.
poo :: EVarName
poo = " "

-- | A dummy expression that does nothing (but has a type).
empty :: a -> Exp a
empty z = (EVar z poo)

errorNotSupported :: (Show a, ES3PP.Pretty b) => String -> a -> b -> c
errorNotSupported featureName sourcePos expr = error $ "Not supported: '" ++ featureName ++ "' at " ++ (show sourcePos) ++ " in\n" ++ (show $ ES3PP.prettyPrint expr)
        
foldStmts :: Show a => [ES3.Statement a] -> Exp a -> Exp a
foldStmts [] k = k
foldStmts [x] k = fromStatement x k
foldStmts (x:xs) k = fromStatement x (foldStmts xs k)
                  
fromStatement :: Show a => ES3.Statement a -> Exp a -> Exp a
fromStatement (ES3.BlockStmt _ stmts) = foldStmts stmts
fromStatement (ES3.EmptyStmt _) = id
fromStatement (ES3.ExprStmt z e) = ELet z poo (fromExpression e)
-- TODO: The if/while/do conversion is hacky and introduces dummy poo names into the scope.
fromStatement (ES3.IfStmt z pred' thenS elseS) = ELet z poo (EArray z [fromExpression pred', ELit z (LitBoolean False)]) . foldStmts [thenS, elseS]
fromStatement (ES3.IfSingleStmt z pred' thenS) = ELet z poo (EArray z [fromExpression pred', ELit z (LitBoolean False)]) . fromStatement thenS
fromStatement (ES3.WhileStmt z pred' loopS) = ELet z poo (EArray z [fromExpression pred', ELit z (LitBoolean False)]) . fromStatement loopS
fromStatement (ES3.DoWhileStmt z loopS pred') = ELet z poo (EArray z [fromExpression pred', ELit z (LitBoolean False)]) . fromStatement loopS
fromStatement (ES3.BreakStmt _ _) = id -- TODO verify we can ignore this
fromStatement (ES3.ContinueStmt _ _) = id -- TODO verify we can ignore this
fromStatement (ES3.ThrowStmt _ _) = id
fromStatement s@(ES3.WithStmt z _ _) = errorNotSupported "with" z s
fromStatement (ES3.LabelledStmt _ _ s) = fromStatement s
--fromStatement (ES3.ForInStmt z init' obj loopS) = 
fromStatement (ES3.VarDeclStmt _ decls) = chain decls
    where chain [] k = k
          chain (ES3.VarDecl z' (ES3.Id _ name) Nothing:xs) k = ELet z' name (ELit z' LitUndefined) (chain xs k)
          chain (ES3.VarDecl z' (ES3.Id _ name) (Just v):xs) k = ELet z' name (fromExpression v) (chain xs k)
fromStatement (ES3.FunctionStmt z name args stmts) = ELet z (ES3.unId name)
                                                     $ case args of
                                                         [] -> EAbs z poo body
                                                         xs -> foldl (\expr arg -> EAbs z (ES3.unId arg) expr) body xs
                                                         where body = (foldStmts stmts $ empty z)
-- TODO: return statements must be added to the core language to be handled correctly.                                                     
fromStatement (ES3.ReturnStmt z x) = \k -> maybe (EVar z poo) fromExpression x
                 
fromExpression :: Show a => ES3.Expression a -> Exp a
fromExpression (ES3.StringLit z s) = ELit z (LitString s)
fromExpression (ES3.RegexpLit z s g i) = ELit z (LitRegex s g i)
fromExpression (ES3.BoolLit z s) = ELit z (LitBoolean s)
fromExpression (ES3.IntLit z s) = ELit z (LitNumber $ fromIntegral s)
fromExpression (ES3.NumLit z s) = ELit z (LitNumber s)
fromExpression (ES3.NullLit z) = ELit z LitNull
fromExpression (ES3.ArrayLit z exprs) = EArray z (map fromExpression exprs)
fromExpression (ES3.VarRef z name) = EVar z $ ES3.unId name
fromExpression (ES3.CondExpr z ePred eThen eElse) = EIfThenElse z (fromExpression ePred) (fromExpression eThen) (fromExpression eElse)
fromExpression (ES3.CallExpr z expr argExprs) = chainApp argExprs
    where chainApp [] = EApp z (fromExpression expr) (empty z) -- TODO handle functions that take no parameter
          chainApp [x] = EApp z (fromExpression expr) (fromExpression x)
          chainApp (x:xs) = EApp z (chainApp xs) (fromExpression x)
fromExpression (ES3.AssignExpr z ES3.OpAssign (ES3.LVar _ name) expr) = EAssign z name (fromExpression expr) (EVar z name)
fromExpression (ES3.FuncExpr z Nothing argNames stmts) = chainAbs . reverse $ map ES3.unId argNames
    where chainAbs [] = EAbs z poo (foldStmts stmts $ empty z)
          chainAbs [x] = EAbs z x (foldStmts stmts $ empty z)
          chainAbs (x:xs) = EAbs z x (chainAbs xs)
--           where funcBody = Block $ map fromStatement stmts 
fromExpression (ES3.ListExpr z exprs) =
    case exprs of
      [] -> empty z
      [x] -> fromExpression x
      xs -> ELet z poo (ETuple z . map fromExpression $ tail revXs) (fromExpression $ head revXs)
          where revXs = reverse xs
fromExpression e@(ES3.ThisRef z) = errorNotSupported "this" z e
--fromExpression e = error $ "Not implemented: expression = " ++ show (ES3PP.prettyPrint e)
-- -- ------------------------------------------------------------------------

parseFile :: String -> IO (Either String (Type TBody))
parseFile arg = do
  js <- ES3Parser.parseFromFile arg 
  --putStrLn . show $ js
  let src (ES3.Script a _) = a
  let emptySrcPos = src js -- hack
  let expr = (foldStmts $ ES3.unJavaScript js) (empty emptySrcPos)
  --putStrLn "--"
  print expr
  putStrLn . pretty $ expr
  putStrLn . pretty $ test expr
  return $ runTypeInference expr
                           

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


-- fromProp :: ES3.Prop a -> String
-- fromProp (ES3.PropId _ (ES3.Id _ x)) = x
-- fromProp (ES3.PropString _ x) = x
-- fromProp (ES3.PropNum _ x) = show x

