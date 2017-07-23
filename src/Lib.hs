module Lib where

-- does not handle multi-line notes in <DD>

import           ClassyPrelude (void)
import           Text.Parsec

type Parser = Parsec String ()

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

top :: String -> IO (Either ParseError [Bookmark])
top = parseFromFile delicious

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname = do
  input <- readFile fname
  return (runParser p () fname input)

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
