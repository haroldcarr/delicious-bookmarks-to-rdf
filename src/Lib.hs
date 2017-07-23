{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

-- does not handle multi-line notes in <DD>

{-
deliciousToRdf "test/test-data/delicious-2017-07-14.html" "/tmp/delicious-2017-07-14.ttl"
-}

import           ClassyPrelude as CP hiding (many, try, (<|>))
import           System.IO     as IO (IOMode (WriteMode), hPutStr, hPutStrLn,
                                      readFile, withFile)
import           Text.Parsec

------------------------------------------------------------------------------

deliciousToRdf :: FilePath -> FilePath -> IO ()
deliciousToRdf i o = do
  p <- parseDelicious i
  either (error . show) (writeRdf o) p

------------------------------------------------------------------------------

data Bookmark
  = Bookmark
  { href    :: String
  , addDate :: String
  , private :: String
  , tags    :: String
  , title   :: String
  , notes   :: String
  }
  deriving (Show)

------------------------------------------------------------------------------
-- write RDF

writeRdf :: FilePath -> [Bookmark] -> IO ()
writeRdf o bs =
  withFile o WriteMode $ \h ->
    forM_ bs $ writeBookmark h

writeBookmark :: Handle -> Bookmark -> IO ()
writeBookmark h b = do
  hPutStrLn h ""
  hPutStr h "<"; hPutStr h (href b); hPutStrLn h ">"
  writeProperty h "addDate" (addDate b) ";"
  writeProperty h "private" (private b) ";"
  writeProperty h "tags" (tags b) ";"
  if notes b == "" then
      writeProperty h "title" (show (title b)) "."
  else do
      writeProperty h "title" (show (title b)) ";"
      writeProperty h "notes" (show (notes b)) "."
  return ()

writeProperty :: Handle -> String -> String -> String -> IO ()
writeProperty h p v sep = do
  hPutStr h "    "
  hPutStr h p
  hPutStr h " "
  hPutStr h v
  hPutStr h " "
  hPutStrLn h sep


------------------------------------------------------------------------------
-- parse delicious

type Parser = Parsec String ()

parseDelicious :: FilePath -> IO (Either ParseError [Bookmark])
parseDelicious = parseFromFile delicious

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname = do
  i <- IO.readFile fname
  return (runParser p () fname i)

delicious :: Parser [Bookmark]
delicious = do
  delHeader
  manyTill bookmark (try $ string "</DL><p>" >> theEnd)

bookmark :: Parser Bookmark
bookmark = do
  void $ string "<DT><A HREF=\""
  h <- stringContents
  spaces
  void $ string "ADD_DATE=\""
  a <- stringContents
  spaces
  void $ string "PRIVATE=\""
  p <- stringContents
  spaces
  void $ string "TAGS=\""
  t <- stringContents
  spaces
  void $ char '>'
  ti <- manyTill anyChar (try (string "</A>"))
  spaces
  n <- bookmarkNote <|> return ""
  return $ Bookmark h a p t ti n

bookmarkNote :: Parser String
bookmarkNote = do
  void $ try (string "<DD>")
  manyTill anyChar endOfLine

stringContents :: Parser String
stringContents =
  manyTill anyChar (try (char '"'))

delHeader :: Parser ()
delHeader = do
  void $ string "<!DOCTYPE NETSCAPE-Bookmark-file-1>"
  void   endOfLine
  void $ string "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">"
  void   endOfLine
  void $ string "<!-- This is an automatically generated file."
  void   endOfLine
  void $ string "It will be read and overwritten."
  void   endOfLine
  void $ string "Do Not Edit! -->"
  void   endOfLine
  void $ string "<TITLE>Bookmarks</TITLE>"
  void   endOfLine
  void $ string "<H1>Bookmarks</H1>"
  void   endOfLine
  void $ string "<DL><p>"
  void   endOfLine

theEnd :: Parser String
theEnd = do
  void $ many anyChar
  eof
  return ""
