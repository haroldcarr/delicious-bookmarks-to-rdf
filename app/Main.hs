module Main where

import           DeliciousToRdf
import           System.Environment (getArgs)

main :: IO ()
main = do
  io <- getArgs
  case io of
    [i,o] -> hcDeliciousToRdf i o
    _     -> error "Usage: deliciousBookmarksToRdf <in-file> <out-file>"
