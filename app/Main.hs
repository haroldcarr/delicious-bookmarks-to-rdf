module Main where

import           Lib

main :: IO ()
main = do
  _ <- deliciousToRdf "foo" "bar"
  return ()
