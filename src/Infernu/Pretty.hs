{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Infernu.Pretty where



import           Infernu.Prelude
import           Infernu.Expr
import           Infernu.Source
import           Infernu.Types
import qualified Infernu.Builtins.Names as Names

import           Data.Char                    (chr, ord)
import qualified Data.Digits                  as Digits
import qualified Data.List                    as List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map

import qualified Data.Set                     as Set
import qualified Text.Parsec.Pos              as Pos

import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- tab :: Int -> String
-- tab t = replicate (t*4) ' '

-- class Pretty a where
--   prettyTab :: Int -> a -> String

-- instance Pretty a => Pretty (Maybe a) where
--   pretty x = maybe "Nothing" pretty x

-- instance (Pretty a, Pretty b) => Pretty (a,b) where
--   pretty (a,b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

-- instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
--   pretty (a,b,c) = "(" ++ pretty a ++ ", " ++ pretty b ++ ", " ++ pretty c ++ ")"

-- prettyList :: Pretty a => [a] -> String
-- prettyList [] = "[]"
-- prettyList xs = "[" ++ intercalate "," (map pretty xs) ++ "]"

-- instance Pretty [String] where
--   pretty = prettyList

-- instance Pretty [Type] where
--   pretty  = prettyList

-- instance (Pretty a, Pretty b) => Pretty [(a,b)] where
--   pretty xs = "[" ++ intercalate "," (map pretty xs) ++ "]"

-- pretty :: Pretty a => a -> String
-- pretty = prettyTab 0



instance Pretty Pos.SourcePos where
    pretty p = string (Pos.sourceName p) <> string ":" <> (string . show $ Pos.sourceLine p) <> string ":" <> (string . show $ Pos.sourceColumn p)

ifStr :: Bool -> String -> Doc
ifStr b s = if b then string s else empty

instance Pretty Source where
    pretty (Source (genInfo, pos)) =
        ifStr (isGen genInfo) "*"
        <> pretty pos
        <> maybe empty (\x -> string ":" <> string x <> string ":") (declName genInfo)


instance Pretty LitVal where
    pretty (LitNumber x) = pretty x
    pretty (LitBoolean x) = pretty x
    pretty (LitString x) = pretty x
    pretty (LitRegex x g i) = enclose (string "/") (string "/") (string x) <> (ifStr g "g") <> (ifStr i "i")
    pretty LitUndefined = string "undefined"
    pretty LitNull = string "null"
    pretty LitEmptyThis = string "(undefined 'this')"

nakedSingleOrTuple :: Pretty a => [a] -> Doc
nakedSingleOrTuple [] = string "()"
nakedSingleOrTuple [x] = pretty x
nakedSingleOrTuple xs = tupled $ map pretty xs

instance Pretty EPropName where
    pretty (EPropName x) = string x
    pretty EPropGetIndex = string "=[]"
    pretty EPropSetIndex = string "[]="
    pretty EPropFun = string "call()"

instance Pretty (Exp a) where
    pretty (EVar _ n) = string n
    pretty (EApp _ (EVar _ n) [arg1,arg2]) | n == Names.refAssignOp = parens $ pretty arg1 <+> string ":=" <+> pretty arg2
    pretty (EApp _ e1 args) = parens $ pretty e1 <+> nakedSingleOrTuple args
    pretty (EAbs _ args e) = parens $ string "\\" <> nakedSingleOrTuple args <+> string "->" <+> pretty e
    pretty (ELet _ n e1 e2) = string "let" <+> align (vsep $ letLine n e1 e2)
        where letLine n' e' eBody = curLine n' e' : rest eBody
              curLine n' e' = pretty n' <+> string "=" <+> pretty e'
              rest eBody = case eBody of
                               ELet _ nb e1b e2b -> letLine nb e1b e2b
                               _ -> [string "in" <+> align (pretty eBody)]

    pretty (ELit _ l) = pretty l
    pretty (EPropAssign _ obj n e1 e2) = align $ vsep [ pretty obj <> dot <> pretty n <+> string ":=" <+> pretty e1 <> string ";"
                                                      , pretty e2]
    pretty (EArray _ es) = encloseSep lbracket rbracket comma $ map pretty es
    pretty (ETuple _ es) = pretty es
    pretty (ERow _ _ props) = encloseSep lbrace rbrace comma
                              $ map (\(n,v) -> fill 6 (pretty n) <> string ":" <> space <> pretty v) props
--                                   <> ifStr isOpen " | ? "
    pretty (ECase _ ep es) = hang 4 $
                             vsep [ string "case" <+> pretty ep <+> string "of"
                                  , hang 4 $ align (vsep (map formatBranch' es))
                                  ]
        where formatBranch' (pat, branch) = fill 6 (pretty pat) <+> string "->" <+> align (pretty branch)
    pretty (EProp _ e n) = pretty e <> dot <> pretty n
    pretty (ENew _ e args) = string "new" <> space <> pretty e <> space <> nakedSingleOrTuple (map pretty args)
    pretty (EStringMap _ exprs) = encloseSep langle rangle comma $ map (\(n,v) -> pretty n <+> string "=>" <+> pretty v) exprs


-----------------------------------------------------------------------------

toChr :: Int -> Char
toChr n = chr (ord 'a' + (n - 1))

ptv :: Int -> String
ptv x = foldr ((++) . (:[]) . toChr)  [] (Digits.digits 26 (x + 1))

colorFuncs :: [Doc -> Doc]
colorFuncs = [ red
             , green
             , yellow
             , blue
             , magenta
             , cyan
             , white
             ]

colorBy :: Int -> Doc -> Doc
colorBy n = colorFuncs!!(n `mod` length colorFuncs)

prettyKind :: Kind -> Doc
prettyKind k = colon <> colon <+> pretty k

-- |
-- >>> pretty (0 :: TVarName)
-- "a"
-- >>> pretty (26 :: TVarName)
-- "aa"
instance Pretty TVarName where
    pretty tvn =
        bold $
        case tvn of
        Flex n k -> (colorBy n $ string $ ptv n) -- <+> prettyKind k
        Skolem n k -> (colorBy n $ string $ '!' : ptv n) -- <+> prettyKind k

-- instance Pretty Bool where
--   prettyTab _ x = show x

instance Pretty TypeId where
    pretty (TypeId n) = dullred $ text $ 'R' :  ptv n

instance Pretty Kind where
    pretty KStar = dullred $ text "*"
    pretty KRow = dullred $ text "{*}"
    pretty (KArrow k1 k2) = dullred $ pretty k1 <+> text "->" <+> pretty k2

instance Pretty TBody where
  pretty (TVar n) = pretty n
  pretty x = bold $ text $ case show x of
                               'T':xs -> xs
                               xs -> xs

instance Pretty TConsName where
  pretty = dullgreen . text . show

instance Pretty RowTVar where
  pretty t = dullblue $ pretty (getRowTVar t)

instance Show t => Pretty (FlatRowEnd t) where
  pretty = text . show

instance Pretty Type where
  pretty = prettyType . unFix

instance Pretty (FType Type) where
  pretty = prettyType

prettyType :: FType Type -> Doc
prettyType (TBody t) = pretty t
prettyType (TFunc ts tres) = wrapThis this $ parens $ args <+> string "->" <+> pretty tres
  where nonThisArgs = map pretty . drop 1 $ ts
        (this, args) = case ts of
                [] -> (Nothing, nakedSingleOrTuple nonThisArgs)
                (this_:_) -> (Just this_, nakedSingleOrTuple nonThisArgs)
        wrapThis Nothing s = s
        wrapThis (Just (Fix (TBody TUndefined))) s = s
        wrapThis (Just (Fix (TBody TEmptyThis))) s = s
        -- if "this" is a recursive type, only show the recursive type name (no params) - this is "lossy"
        wrapThis (Just (Fix (TCons (TName name k) _))) s = pretty name <> dot <> s
        wrapThis (Just (Fix (TBody (TVar n)))) s | not (n `Set.member` (freeTypeVars $ drop 1 ts)) = s
        wrapThis (Just t) s = pretty t <> dot <> s
-- prettyTab _ (TCons TFunc ts) = error $ "Malformed TFunc: " ++ intercalate ", " (map pretty ts)
prettyType (TCons TArray [t]) = brackets $ pretty t
prettyType (TCons TTuple ts) = pretty ts
prettyType (TCons (TName name k) ts) = angles $ pretty name <> colon <+> hsep (map pretty ts)
prettyType (TCons TStringMap [t]) = text "StringMap " <+> pretty t
prettyType (TCons TRef [t]) = text "Mut" <+> pretty t
prettyType (TCons tcn ts) = error $ "Malformed TCons: " ++ show (pretty tcn <+> pretty ts)
prettyType (TRow label rl) =
    hsep [ case label of
                 Just l' -> string l' <> string "="
                 Nothing -> empty
           , encloseSep (string "{ ") space (string ", ") body'
             <> case r of
                    FlatRowEndTVar r' -> maybe empty ((text "|" <+>) . pretty) r'
                    FlatRowEndRec tid ts -> comma <+> indent 4 (pretty (Fix $ TCons (TName tid KRow) ts)) -- TODO
             <> rbrace
           ]
  where (props, r) = flattenRow rl
        isGet (TPropGetName _) = True
        isGet _ = False
        isSet (TPropSetName _) = True
        isSet _ = False

        propKeysByName = map (\ps -> let name = tpropName . fst $ head ps
                                         keys = map fst ps
                                     in (name, snd $ head ps, (any isGet keys, any isSet keys)))
                         $ List.groupBy (\(x,xv) (y,yv) -> tpropName x == tpropName y && xv == yv) $ Map.toList props
        printProp' (n,v,getSet) = pn <> string ":" <+> align (pretty v)
            where pn = case getSet of
                          (True, True) -> pretty n
                          (True, False) -> string "get" <+> pretty n
                          (False, True) -> string "set" <+> pretty n
                          _ -> error "Expected at least one of get or set"
        body' = map printProp' propKeysByName

instance Pretty TProp where
    pretty (TPropSetName n) = text "set" <+> pretty n
    pretty (TPropGetName n) = text "get" <+> pretty n

instance Pretty ClassName where
    pretty (ClassName c) = text c

instance (Pretty t) => Pretty (TPred t) where
    pretty (TPredIsIn cn t) = pretty cn <+> pretty t

instance (VarNames t, Pretty t) => Pretty (TQual t) where
    pretty (TQual [] t) = pretty t
    pretty (TQual preds t) = pretty preds <+> pretty "=>" <+> align (pretty t)

instance (Ord t, VarNames t, Pretty t) => Pretty (TScheme t) where
    pretty (TScheme vars t) = forall <> align (pretty t)
        where forall = if null vars then empty else text "forall" <+> hsep (map pretty vars) <> pretty "." <> space

-- instance (Pretty a, Pretty b) => Pretty (Either a b) where
--     prettyTab n (Left x) = "Error: " ++ prettyTab n x
--     prettyTab n (Right x) = prettyTab n x

instance (Pretty k, Pretty v) => Pretty (Map k v) where
    pretty s = string "Map" <+> encloseSep lbrace rbrace comma (map (\(k,v) -> pretty k <+> pretty "=>" <+> pretty v) $ Map.toList s)

instance (Pretty k) => Pretty (Set.Set k) where
    pretty s = string "Set" <+> encloseSep lbrace rbrace comma (map pretty $ Set.toList s)

-- instance Pretty GenInfo where
--     prettyTab _ g = show g

instance Pretty TypeError where
    pretty (TypeError s m) = pretty s <> text ": Error:" <+> line <+> indent 4 (pretty m)

instance Pretty NameSource where
    pretty = string . show

instance Pretty VarId where
    pretty = string . show

instance (Ord t, VarNames t, Pretty t) => Pretty (Class t) where
    pretty c = braces $ string "instances = " <+> pretty (classInstances c)

instance Pretty InferState where
    pretty (InferState ns sub vs tn cs pu) =
        text "InferState"
          <+> (align . encloseSep lbrace rbrace comma
               $ [ fill 10 (string "nameSource: ") <+> pretty ns
                 , fill 10 (string "subst: ") <+> pretty sub
                 , fill 10 (string "varSchemes: ") <+> pretty vs
                 , fill 10 (string "namedTypes: ") <+> pretty tn
                 , fill 10 (string "pendingUni: ") <+> pretty pu
                 , fill 10 (string "classes: ") <+> pretty cs
                 ])
