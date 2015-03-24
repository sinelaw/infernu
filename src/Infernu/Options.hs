module Infernu.Options
       (Options(..), defaultOptions, opts)
       where

import Options.Applicative 

data Options = Options
               { optQuiet :: Bool
               , optShowCore :: Bool
               , optShowParsed :: Bool
               , optFileNames :: [String]
               }

defaultOptions :: Options
defaultOptions = Options { optQuiet = False, optShowCore = False, optShowParsed = False, optFileNames = [] }
                 
opts :: ParserInfo Options
opts = info (helper <*> parseOpts)
       ( fullDesc
         <> progDesc "Infer types in the given JavaScript FILES and check for type errors. Unless -q is given, the source annotated with type signatures is outputted."
         <> header "infernu - static type checker for JavaScript using full type inference" )
               
parseOpts :: Parser Options
parseOpts = Options
            <$> switch (long "quiet"
                        <> short 'q'
                        <> help "Report only errors; don't output the annotated source with inferred types")
            <*> switch (long "dump-translation"
                        <> help "Dump internal translation (used for debugging infernu)" )
            <*> switch (long "dump-parsed"
                        <> help "Dump parsed JS syntax tree (used for debugging infernu)" )
            <*> some (argument str (metavar "FILES..."))
      
    
