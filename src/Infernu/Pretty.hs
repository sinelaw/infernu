{-# LANGUAGE FlexibleInstances #-}
module Infernu.Pretty where


import           Infernu.Types
import qualified Infernu.Pred as Pred

    
import           Data.Char       (chr, ord)
import qualified Data.Char       as Char
import qualified Data.Digits     as Digits
import           Data.List       (intercalate)
import qualified Data.Map.Lazy   as Map

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
  prettyTab t (EAbs _ args e) = "(\\" ++ nakedSingleOrTuple (map (prettyTab t) args) ++ " -> " ++ prettyTab t e ++ ")"
  prettyTab t (ELet _ n e1 e2) = "let " ++ prettyTab t n ++ " = " ++ prettyTab (t+1) e1 ++ "\n" ++ tab t ++ " in " ++ prettyTab (t+1) e2
  prettyTab t (ELit _ l) = prettyTab t l
  prettyTab t (EAssign _ n e1 e2) = prettyTab t n ++ " := " ++ prettyTab t e1 ++ ";\n" ++ tab t ++ prettyTab t e2
  prettyTab t (EPropAssign _ obj n e1 e2) = prettyTab t obj ++ "." ++ prettyTab t n ++ " := " ++ prettyTab t e1 ++ ";\n" ++ tab t ++ prettyTab t e2
  prettyTab t (EIndexAssign _ obj i e1 e2) = prettyTab t obj ++ "[" ++ prettyTab t i ++ "] := " ++ prettyTab t e1 ++ ";\n" ++ tab t ++ prettyTab t e2
  prettyTab t (EArray _ es) = "[" ++ intercalate ", " (map (prettyTab t) es) ++ "]"
  prettyTab t (ETuple _ es) = "(" ++ intercalate ", " (map (prettyTab t) es) ++ ")"
  prettyTab t (ERow _ isOpen props) = "{" ++ intercalate ", " (map (\(n,v) -> prettyTab t n ++ ": " ++ prettyTab t v) props)  ++ (if isOpen then ", " else "") ++ "}"
  prettyTab t (EIfThenElse _ ep e1 e2) = "(" ++ prettyTab t ep ++  " ? " ++ prettyTab t e1 ++ " : " ++ prettyTab t e2 ++ ")"
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
prettyType n (TCons TFunc ts) = wrapThis this $ args ++ " -> " ++ prettyTab n (last ts)
  where nonThisArgs = map (prettyTab n) . drop 1 $ init ts
        (this, args) = case ts of
                [] -> (Nothing, nakedSingleOrTuple nonThisArgs)
                (this_:_) -> (Just this_, nakedSingleOrTuple nonThisArgs)
        wrapThis Nothing s = s
        wrapThis (Just (Fix (TBody TUndefined))) s = s
        wrapThis (Just t) s = prettyTab n t ++ ".(" ++ s ++ ")"
-- prettyTab _ (TCons TFunc ts) = error $ "Malformed TFunc: " ++ intercalate ", " (map pretty ts)
prettyType n (TCons TArray [t]) = "[" ++ prettyTab n t ++ "]"
prettyType n (TCons TArray ts) = error $ "Malformed TArray: " ++ intercalate ", " (map (prettyTab n) ts)
prettyType n (TCons TTuple ts) = "(" ++ intercalate ", " (map (prettyTab n) ts) ++ ")"
prettyType n (TCons (TName name) ts) = "<" ++ pretty name ++ ">" -- : " ++ (unwords $ map (prettyTab n) ts) ++ ">"
prettyType n (TCons TStringMap [t]) = "Map " ++ prettyTab n t
prettyType n (TCons TStringMap ts) = error $ "Malformed TStringMap: " ++ intercalate ", " (map (prettyTab n) ts)  
prettyType t (TRow list) = "{"
                          ++ intercalate ", " (map (\(n,v) -> prettyTab (t+1) n ++ ": " ++ prettyTab (t+1) v) (Map.toList props))
                          ++ (case r of
                               FlatRowEndTVar r' -> maybe "" ((", "++) . pretty) r'
                               FlatRowEndRec tid ts -> ", " ++ prettyTab t (Fix $ TCons (TName tid) ts) -- TODO
                             )
                          ++ "}"
  where (props, r) = flattenRow list

instance (Ord t, Pretty t) => Pretty (TPred t) where
    prettyTab n = prettyPred n . Pred.fixSimplify
    
prettyPred :: (Pretty a, Ord a) => Int -> TPred a -> [Char]
prettyPred n (TPredEq v t) = prettyTab n v ++ " = " ++ prettyTab n t
prettyPred n (TPredOr p1 p2) = "(" ++ prettyTab n p1 ++ " | " ++ prettyTab n p2 ++ ")"
prettyPred n (TPredAnd p1 p2) = "(" ++ prettyTab n p1 ++ " & " ++ prettyTab n p2 ++ ")"
prettyPred _ (TPredTrue) = "True"

instance (Ord t, Pretty t) => Pretty [TPred t] where
    prettyTab n p = intercalate ", " $ map (prettyTab n) p

predsStr :: (Ord t, Pretty t) => Int -> TPred t -> String
predsStr n preds =
    case preds of
        TPredTrue -> ""
        p -> prettyTab n p ++ " => "

instance (Eq t, Ord t, VarNames t, Pretty t) => Pretty (TQual t) where
    prettyTab n (TQual preds t) = (predsStr n $ removeUnusedTVars (freeTypeVars t) preds) ++ prettyTab n t

-- TODO remove Eq t constraint and add global unification for preds
removeUnusedTVars :: (VarNames t, Eq t) => Set.Set TVarName -> TPred t -> TPred t
removeUnusedTVars tvars p@(TPredEq n _) = if n `Set.member` tvars then p else TPredTrue
removeUnusedTVars tvars (TPredAnd p1 p2) = removeUnusedTVars tvars p1 `mkAnd` removeUnusedTVars tvars p2
removeUnusedTVars tvars (TPredOr p1 p2) = removeUnusedTVars tvars p1 `mkOr` removeUnusedTVars tvars p2
removeUnusedTVars _ TPredTrue = TPredTrue                                 

instance (Ord t, Pretty t) => Pretty (TScheme t) where
  prettyTab n (TScheme vars t preds) = predsStr n preds ++ forall ++ prettyTab n t
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

instance Pretty IsGen where
    prettyTab _ (IsGen g) = show g
    
instance Pretty TypeError where
  prettyTab _ (TypeError s m) = pretty s ++ ": Error: " ++ m

instance Pretty NameSource where
  prettyTab _ = show

instance Pretty VarId where
  prettyTab _ = show

instance Pretty InferState where
  prettyTab t (InferState ns sub vs vi tn) = "InferState { nameSource: "
                                             ++ pretty ns ++ newline
                                             ++ ", subst: " ++ pretty sub ++ newline
                                             ++ ", varSchemes: " ++ pretty vs ++ newline
                                             ++ ", varInstances: " ++ pretty vi ++ newline
                                             ++ ", namedTypes: " ++ pretty tn ++ newline
                                             ++ "}"
    where newline = "\n" ++ tab (t+1)
