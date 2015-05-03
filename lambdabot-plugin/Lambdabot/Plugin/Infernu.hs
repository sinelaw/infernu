{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Infernu
module Lambdabot.Plugin.Infernu (infernuPlugin) where

import           Data.Maybe                  (catMaybes)
import qualified Data.ByteString.Char8       as P
import           Data.Functor                ((<$>))
import           Data.List                   (intersperse)
import qualified Data.Map                    as Map
import           Lambdabot.Plugin
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.Syntax as ES3

import           Infernu.Infer                (getAnnotations, minifyVars,
                                              pretty, runTypeInference)
import           Infernu.Parse                (translate)
import           Infernu.Types (GenInfo(..), Source(..))

infernuPlugin :: Module ()
infernuPlugin = newModule
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
sayType rest = case  runTypeInference . fmap Source . translate . ES3.unJavaScript <$> ES3Parser.parseFromString rest of
                Left e -> show e
                Right res -> case getAnnotations <$> res of
                              Left e' -> pretty e'
                              Right [] -> show "There is nothing there."
                              Right xs -> concat . intersperse "\n" $ filterGen xs
                                  where filterGen = catMaybes . map (\(Source (GenInfo g n, _), t) ->
                                                                         case (g,n) of
                                                                             (False, Just n) -> Just (pretty n ++ " : " ++ (pretty $ minifyVars t))
                                                                             _ -> Nothing
                                                                    )

