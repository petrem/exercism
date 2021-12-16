module Main where

import qualified CollatzConjecture as CCopt
import CollatzConjectureRecursive as CCrec
import CollatzConjectureHigherOrder as CChi

import Control.Monad
import Control.Exception (evaluate)
import System.Time.Extra (duration)

benchmark :: Show a => String -> a -> IO ()
benchmark title code = do
  (t, _) <- duration $ evaluate code
  putStrLn $ title ++ ": " ++show t

main :: IO ()
main = do
  let range = [1..100000000]
  benchmark "warm-up" $ forM_ range Just
  benchmark "recursive"           $ forM_ range CCrec.collatz
  benchmark "iterate"             $ forM_ range CChi.collatz
  benchmark "optimize steps away" $ forM_ range CCopt.collatz
