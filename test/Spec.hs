{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           ClassyPrelude
import           DeliciousToRdf

testDataDir :: String
testDataDir = "test/test-data/"

testData :: [String]
testData = map (testDataDir <>) [ "delicious-2016-03-11.html"
                                , "delicious-2016-07-15.html"
                                , "delicious-2017-07-14.html"
                                ]

main :: IO ()
main =
  parseTest

parseTest :: IO ()
parseTest = do
  putStrLn ""
  putStrLn ""
  forM_ testData $ \d -> do
    p <- parseDelicious d
    case p of
      Right d' -> print $ take 1 d'
      Left  e  -> error $ show e
  putStrLn ""
  putStrLn "PASSED"
