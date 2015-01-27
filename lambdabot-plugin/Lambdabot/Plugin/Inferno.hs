{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Inferno
module Lambdabot.Plugin.Inferno (infernoPlugin) where

import qualified Data.ByteString.Char8       as P
import           Data.Functor                ((<$>))
import           Data.List                   (intersperse)
import qualified Data.Map                    as Map
import           Lambdabot.Plugin
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.Syntax as ES3

import           SafeJS.Infer                (getAnnotations, minifyVars,
                                              pretty, runTypeInference)
--import           SafeJS.Util        (chec
import           SafeJS.Parse                (translate)


infernoPlugin :: Module ()
infernoPlugin = newModule
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
                              Right xs -> pretty . minifyVars . snd $ head xs

