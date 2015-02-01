{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module Inferno.Log
       (trace, tracePretty, traceLog)
       where

import           Inferno.Pretty


#if TRACE
import           Debug.Trace                (trace)
#else
trace :: a -> b -> b
trace _ y = y
#endif

tracePretty :: Pretty a => String -> a -> a
tracePretty prefix x = trace (prefix ++ " " ++ pretty x) x

traceLog :: Monad m => String -> a -> m a
traceLog !s !r = return $! trace s r `seq` r

