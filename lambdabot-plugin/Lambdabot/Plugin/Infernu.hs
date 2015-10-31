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
import qualified Language.ECMAScript5.Parser as ES5Parser
import qualified Language.ECMAScript5.Syntax as ES5

import           Infernu.Infer                (getAnnotations, minifyVars,
                                              pretty, runTypeInference)
import           Infernu.Parse.JS                (translate)
import           Infernu.Types (GenInfo(..), Source(..))

import Text.PrettyPrint.ANSI.Leijen (plain, displayS, renderPretty, Doc)

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

showWidth :: Int -> Doc -> String
showWidth w x   = displayS (renderPretty 0.4 w x) ""

showDoc = showWidth 120

--sayType :: Monad m => String -> Cmd m ()
sayType :: String -> String
sayType rest = case  runTypeInference . fmap Source . translate . ES5.unJavaScript <$> ES5Parser.parseFromString rest of
                Left e -> show e
                Right res -> case getAnnotations <$> res of
                              Left e' -> showDoc $ pretty e'
                              Right [] -> show "There is nothing there."
                              Right xs -> concat . intersperse "\n" $ filterGen xs
                                  where filterGen = catMaybes . map (\(Source (GenInfo g n, _), t) ->
                                                                         case (g,n) of
                                                                             (False, Just n) -> Just (showDoc (plain $ pretty n) ++ " : " ++ (showDoc $ plain $ pretty $ minifyVars t))
                                                                             _ -> Nothing
                                                                    )

