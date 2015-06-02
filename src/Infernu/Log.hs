{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module Infernu.Log
       (trace, tracePretty, traceLog, traceLogVal)
       where

import           Infernu.Prelude
import           Infernu.Pretty
import Text.PrettyPrint.ANSI.Leijen (Pretty(..), text, (<+>), Doc)

#if TRACE
import           Debug.Trace                (trace)
#else
trace :: a -> b -> b
trace _ y = y
#endif

tracePretty :: Pretty a => Doc -> a -> a
tracePretty prefix x = trace (show $ prefix <+> pretty x) x

traceLogVal :: Applicative f => String -> a -> f a
traceLogVal !s !r = pure $! trace s r `seq` r

traceLog :: Applicative f => Doc -> f ()
traceLog !s = pure $! trace (show s) () `seq` ()
                    
