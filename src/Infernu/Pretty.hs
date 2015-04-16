{-# LANGUAGE FlexibleInstances #-}
module Infernu.Pretty where


import           Infernu.Types

    
import           Data.Char       (chr, ord)
import qualified Data.Char       as Char
import qualified Data.Digits     as Digits
import           Data.List       (intercalate)
import qualified Data.Map.Lazy   as Map
import qualified Data.Graph.Inductive      as Graph

import qualified Data.Set        as Set
import qualified Text.Parsec.Pos as Pos

tab :: Int -> String
tab t = replicate (t*4) ' '

class Pretty a where
  prettyTab :: Int -> a -> String

instance Pretty a => Pretty (Maybe a) where
  prettyTab _ x = maybe "Nothing" pretty x

instance (Pretty a, Pretty b) => Pretty (a,b) where
  prettyTab _ (a,b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  prettyTab _ (a,b,c) = "(" ++ pretty a ++ ", " ++ pretty b ++ ", " ++ pretty c ++ ")"

prettyList :: Pretty a => [a] -> String
prettyList [] = "[]"
prettyList xs = "[" ++ intercalate "," (map pretty xs) ++ "]"

instance Pretty [String] where
  prettyTab _ = prettyList

instance Pretty [Type] where
  prettyTab _  = prettyList

instance (Pretty a, Pretty b) => Pretty [(a,b)] where
  prettyTab _ xs = "[" ++ intercalate "," (map pretty xs) ++ "]"

pretty :: Pretty a => a -> String
pretty = prettyTab 0

instance Pretty Source where
    prettyTab _ (Source (genInfo, pos)) = pretty pos ++ (if isGen genInfo then "*" else "") ++ (maybe "" (\x -> pretty x ++ " : ") $ declName genInfo)
         
instance Pretty LitVal where
  prettyTab _ (LitNumber x) = show x
  prettyTab _ (LitBoolean x) = show x
  prettyTab _ (LitString x) = show x
  prettyTab _ (LitRegex x g i) = "/" ++ x ++ "/" ++ (if g then "g" else "") ++ (if i then "i" else "") ++ (if g || i then "/" else "")
  prettyTab _ LitUndefined = "undefined"
  prettyTab _ LitNull = "null"

instance Pretty EVarName where
  prettyTab _ x = x


nakedSingleOrTuple :: [String] -> String
nakedSingleOrTuple [] = "()"
nakedSingleOrTuple [x] = x
nakedSingleOrTuple xs = "(" ++ intercalate ", " xs ++ ")"

instance Pretty (Exp a) where
  prettyTab t (EVar _ n) = prettyTab t n
  prettyTab t (EApp _ e1 args) = "(" ++ prettyTab t e1 ++ " " ++ nakedSingleOrTuple (map (prettyTab t) args) ++")"
  prettyTab t (EAbs _ args e) = "(\\" ++ nakedSingleOrTuple (map (prettyTab t) args) ++ " -> " ++ prettyTab (t+1) e ++ ")"
  prettyTab t (ELet _ n e1 e2) = "\n" ++ tab t ++ "let " ++ letLine n e1 e2
      where letLine n' e' eBody = prettyTab t n'
                                  ++ " = "
                                  ++ prettyTab (t+1) e'
                                  ++ "\n" ++ tab (t+1)
                                  ++ case eBody of
                                         (ELet _ nb e1b e2b) -> letLine nb e1b e2b
                                         _ -> pretty " in " ++ prettyTab (t+1) eBody
  prettyTab t (ELit _ l) = prettyTab t l
  prettyTab t (EAssign _ n e1 e2) = prettyTab t n ++ " := " ++ prettyTab t e1 ++ ";\n" ++ tab (t+1) ++ prettyTab (t+1) e2
  prettyTab t (EPropAssign _ obj n e1 e2) = prettyTab t obj ++ "." ++ prettyTab t n ++ " := " ++ prettyTab t e1 ++ ";\n" ++ tab (t+1) ++ prettyTab (t+1) e2
  prettyTab t (EIndexAssign _ obj i e1 e2) = prettyTab t obj ++ "[" ++ prettyTab t i ++ "] := " ++ prettyTab t e1 ++ ";\n" ++ tab (t+1) ++ prettyTab (t+1) e2
  prettyTab t (EArray _ es) = "[" ++ intercalate ", " (map (prettyTab t) es) ++ "]"
  prettyTab t (ETuple _ es) = "(" ++ intercalate ", " (map (prettyTab t) es) ++ ")"
  prettyTab t (ERow _ isOpen props) = "{" ++ intercalate ", " (map (\(n,v) -> prettyTab t n ++ ": " ++ prettyTab t v) props)  ++ (if isOpen then ", " else "") ++ "}"
  prettyTab t (ECase _ ep es) = "case " ++ prettyTab t ep ++  " of\n" ++ (concatMap formatBranch' es)
      where formatBranch' (pat, branch) = tab (t+1)
                                          ++ prettyTab (t+1) pat
                                          ++ " -> "
                                          ++ prettyTab (t+1) branch
  prettyTab t (EProp _ e n) = prettyTab t e ++ "." ++ pretty n
  prettyTab t (EIndex _ e1 e2) = prettyTab t e1 ++ "[" ++ prettyTab t e2 ++ "]"
  prettyTab t (ENew _ e args) = "new " ++ prettyTab t e ++ " " ++ nakedSingleOrTuple (map (prettyTab t) args)
  prettyTab t (EStringMap _ exprs) = "<" ++ intercalate ", " (map (\(n,v) -> prettyTab t n ++ " => " ++ prettyTab t v) exprs) ++ ">"

toChr :: Int -> Char
toChr n = chr (ord 'a' + (n - 1))

-- |
-- >>> prettyTab 0 (0 :: TVarName)
-- "a"
-- >>> prettyTab 0 (26 :: TVarName)
-- "aa"
instance Pretty TVarName where
  prettyTab _ n = foldr ((++) . (:[]) . toChr) [] (Digits.digits 26 (n + 1))

instance Pretty Bool where
  prettyTab _ x = show x

instance Pretty TypeId where
  prettyTab _ (TypeId n) = capitalize $ pretty n
    where capitalize [] = []
          capitalize (x:xs) = Char.toUpper x : xs

instance Pretty TBody where
  prettyTab t (TVar n) = prettyTab t n
  prettyTab _ x = case show x of
                      'T':xs -> xs
                      xs -> xs

instance Pretty TConsName where
  prettyTab _ = show

instance Pretty RowTVar where
  prettyTab _ t = ".." ++ pretty (getRowTVar t)

instance Pretty Type where
  prettyTab x = prettyType x . unFix

instance Pretty (FType Type) where
  prettyTab = prettyType
                
prettyType :: Int -> FType Type -> String
prettyType n (TBody t) = prettyTab n t
prettyType n (TFunc ts tres) = wrapThis this $ "(" ++ args ++ " -> " ++ prettyTab n tres ++ ")"
  where nonThisArgs = map (prettyTab n) . drop 1 $ ts
        (this, args) = case ts of
                [] -> (Nothing, nakedSingleOrTuple nonThisArgs)
                (this_:_) -> (Just this_, nakedSingleOrTuple nonThisArgs)
        wrapThis Nothing s = s
        wrapThis (Just (Fix (TBody TUndefined))) s = s -- undefined as a function's parameter type (including 'this') is allowed to unify with any input parameter.
        wrapThis (Just t) s = prettyTab n t ++ "." ++ s
-- prettyTab _ (TCons TFunc ts) = error $ "Malformed TFunc: " ++ intercalate ", " (map pretty ts)
prettyType n (TCons TArray [t]) = "[" ++ prettyTab n t ++ "]"
prettyType n (TCons TArray ts) = error $ "Malformed TArray: " ++ intercalate ", " (map (prettyTab n) ts)
prettyType n (TCons TTuple ts) = "(" ++ intercalate ", " (map (prettyTab n) ts) ++ ")"
prettyType _ (TCons (TName name) _) = "<" ++ pretty name ++ ">" -- : " ++ (unwords $ map (prettyTab n) ts) ++ ">"
prettyType n (TCons TStringMap [t]) = "Map " ++ prettyTab n t
prettyType n (TCons TStringMap ts) = error $ "Malformed TStringMap: " ++ intercalate ", " (map (prettyTab n) ts)  
prettyType t (TRow list) = "{"
                          ++ body'
                          ++ (case r of
                               FlatRowEndTVar r' -> maybe "" ((", "++) . pretty) r'
                               FlatRowEndRec tid ts -> ", " ++ prettyTab (t+1) (Fix $ TCons (TName tid) ts) -- TODO
                             )
                          ++ "}"
  where (props, r) = flattenRow list
        printProp' = (\(n,v) -> prettyTab (t+1) n ++ ": " ++ prettyTab (t+1) v)
        body' = case Map.toList props of
                    [] -> ""
                    [p] -> printProp' p
                    ps -> "\n" ++ tab (t+1) ++ intercalate (",\n" ++ tab (t+1)) (map printProp' ps) ++ "\n" ++ tab t

instance Pretty ClassName where
    prettyTab _ (ClassName c) = c
                    
instance (Pretty t) => Pretty (TPred t) where
    prettyTab _ (TPredIsIn cn t) = pretty cn ++ " " ++ pretty t
    
instance (Pretty t) => Pretty [TPred t] where
    prettyTab n p = intercalate ", " $ map (prettyTab n) p

instance (VarNames t, Pretty t) => Pretty (TQual t) where
    prettyTab n (TQual [] t) = prettyTab n t
    prettyTab n (TQual preds t) = prettyTab n preds ++ " => " ++ prettyTab n t

instance (Ord t, VarNames t, Pretty t) => Pretty (TScheme t) where
  prettyTab n (TScheme vars t) = forall ++ prettyTab n t
      where forall = if null vars then "" else "forall " ++ unwords (map (prettyTab n) vars) ++ ". "

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    prettyTab n (Left x) = "Error: " ++ prettyTab n x
    prettyTab n (Right x) = prettyTab n x

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  prettyTab n s = "Map (" ++ str' ++ ")"
    where str' = intercalate ", " . map (\(k,v) -> prettyTab n k ++ " => " ++ prettyTab n v) . Map.toList $ s

instance (Pretty k) => Pretty (Set.Set k) where
  prettyTab n s = "Set {" ++ str' ++ "}"
    where str' = intercalate ", " . map (prettyTab n) . Set.toList $ s

instance Pretty Pos.SourcePos where
    prettyTab _ p = Pos.sourceName p ++ ":" ++ show (Pos.sourceLine p) ++ ":" ++ show (Pos.sourceColumn p)

instance Pretty GenInfo where
    prettyTab _ g = show g
    
instance Pretty TypeError where
  prettyTab t (TypeError s m) = prettyTab t s ++ ": Error: " ++ prettyTab (t+1) m

instance Pretty NameSource where
  prettyTab _ = show

instance Pretty VarId where
  prettyTab _ = show

instance (Ord t, VarNames t, Pretty t) => Pretty (Class t) where
    prettyTab n c = "{ instances = [" ++ s' ++ "] }"
        where s' = intercalate ", " . map (prettyTab n) $ classInstances c

instance (Show a, Show b) => Pretty (Graph.Gr a b) where
    prettyTab _ = Graph.prettify
                  
instance Pretty InferState where
  prettyTab t (InferState ns sub vs vi tn cs pu) = "InferState { nameSource: "
                                             ++ pretty ns ++ newline
                                             ++ ", subst: " ++ pretty sub ++ newline
                                             ++ ", varSchemes: " ++ pretty vs ++ newline
                                             ++ ", varInstances: " ++ pretty vi ++ newline
                                             ++ ", namedTypes: " ++ pretty tn ++ newline
                                             ++ ", pendingUni: " ++ pretty pu ++ newline
                                             ++ ", classes: " ++ pretty cs ++ newline
                                             ++ "}"
    where newline = "\n" ++ tab (t+1)
