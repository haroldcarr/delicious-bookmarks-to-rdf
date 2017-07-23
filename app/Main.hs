module Main where

import           Lib

main :: IO ()
main = do
  _ <- top "foo"
  return ()
