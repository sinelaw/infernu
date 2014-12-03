{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | SJS
module Lambdabot.Plugin.SJS (sjsPlugin) where

import Lambdabot.Plugin
import Data.Functor ((<$>))
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as P
import Data.List(intersperse)
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.Syntax as ES3

import           SafeJS.Infer       (pretty, runTypeInference, getAnnotations)
--import           SafeJS.Util        (chec
import           SafeJS.Parse                (translate)


sjsPlugin :: Module ()
sjsPlugin = newModule
    { moduleCmds = return
        [ (command "js")
            { aliases = []
            , help = say "js <javascript expression>"
            , process = say . sayType
            }
        ]
    }


--sayType :: Monad m => String -> Cmd m ()
sayType :: String -> String
sayType rest = case  runTypeInference . translate . ES3.unJavaScript <$> ES3Parser.parseFromString rest of
                Left e -> show e
                Right res -> case getAnnotations <$> res of
                              Left e' -> pretty e'
                              Right [] -> show "There is nothing there."
                              Right xs -> pretty . snd $ head xs
                       
