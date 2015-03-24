module Infernu.Options
       (Options(..), opts)
       where

import Options.Applicative 

data Options = Options
               { optShowCore :: Bool
               , optFileNames :: [String]
               }

opts = info (helper <*> parseOpts)
       ( fullDesc
         <> progDesc "Check types for the given FILES"
         <> header "infernu - static type checker for JavaScript using full type inference" )
               
parseOpts :: Parser Options
parseOpts = Options
  <$> switch
      ( long "core"
     <> help "Show internal core translation output" )
  <*> many (argument str (metavar "FILES..."))
      
    
