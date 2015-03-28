module Infernu.Builtins.TypeClasses
       (typeClasses)
       where

import           Infernu.Types

typeClasses =
    [ (ClassName "Indexable", Class { classInstances =
                                              [ TScheme { schemeVars = [0]
                                                        , schemeType = qualEmpty
                                                                       $ Fix $ TCons TTuple
                                                                       [ Fix $ TCons TArray [Fix $ TBody $ TVar 0]
                                                                       , Fix $ TBody TNumber
                                                                       , Fix $ TBody $ TVar 0 ]
                                                        }
                                              , TScheme { schemeVars = [1]
                                                        , schemeType = qualEmpty
                                                                       $ Fix $ TCons TTuple
                                                                       [ Fix $ TCons TStringMap [Fix $ TBody $ TVar 1]
                                                                       , Fix $ TBody TString
                                                                       , Fix $ TBody $ TVar 1 ]
                                                        }
                                              , schemeEmpty $ Fix $ TCons TTuple
                                                [ Fix $ TBody TString
                                                , Fix $ TBody TNumber
                                                , Fix $ TBody TString ]
                                              ]})
    , (ClassName "Plus", Class { classInstances =
                                         [ schemeEmpty $ Fix $ TBody TNumber
                                         , schemeEmpty $ Fix $ TBody TString
                                         ]})
    ]
