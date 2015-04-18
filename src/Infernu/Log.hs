{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module Infernu.Log
       (trace, tracePretty, traceLog, traceLogVal)
       where

import           Infernu.Prelude
import           Infernu.Pretty


#if TRACE
import           Debug.Trace                (trace)
#else
trace :: a -> b -> b
trace _ y = y
#endif

tracePretty :: Pretty a => String -> a -> a
tracePretty prefix x = trace (prefix ++ " " ++ pretty x) x

traceLogVal :: Applicative f => String -> a -> f a
traceLogVal !s !r = pure $! trace s r `seq` r

traceLog :: Applicative f => String -> f ()
traceLog !s = pure $! trace s () `seq` ()
                    
