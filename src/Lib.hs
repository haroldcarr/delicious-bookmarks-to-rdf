{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

-- does not handle multi-line notes in <DD>

{-
parseDelicious "test/test-data/delicious-2017-07-14.html"
deliciousToRdf "test/test-data/delicious-2017-07-14.html" "/tmp/delicious-2017-07-14.ttl"
-}

import           ClassyPrelude         as CP hiding (many, try, (<|>))
import           Data.ByteString       as BS hiding (hPutStrLn)
import           Data.ByteString.Char8 as BS (hPutStrLn)
import           Data.Text             as T (pack, replace, splitOn, strip)
import           Data.Text.Encoding    as T (decodeUtf8, encodeUtf8)
import           System.IO             as IO (IOMode (WriteMode), withFile)
import           Text.Parsec

------------------------------------------------------------------------------

deliciousToRdf :: FilePath -> FilePath -> IO ()
deliciousToRdf i o = do
  p <- parseDelicious i
  either (error . show) (writeRdf o) p

------------------------------------------------------------------------------

data Bookmark
  = Bookmark
  { href    :: Text
  , addDate :: Text
  , private :: Text
  , tags    :: [Text]
  , title   :: Text
  , notes   :: Text
  }
  deriving (Show)

------------------------------------------------------------------------------
-- write RDF

hc :: Text
hc = "@prefix hc: <http://openhc.org/syntax-ns#> ."

writeRdf :: FilePath -> [Bookmark] -> IO ()
writeRdf o bs =
  withFile o WriteMode $ \h -> do
    BS.hPutStrLn h (T.encodeUtf8 hc)
    forM_ bs $ writeBookmark h

writeBookmark :: Handle -> Bookmark -> IO ()
writeBookmark h b = do
  BS.hPutStrLn h ""
  hPutStr h "<"; hPutStr h (T.encodeUtf8 (href b)); BS.hPutStrLn h ">"
  writeProperty h "hc:addDate" (addDate b) ";"
  writeProperty h "hc:private" (private b) ";"
  writeTags h (tags b)
  if notes b == "" then
      writeProperty h "hc:title" (lintTitle (title b)) "."
  else do
      writeProperty h "hc:title" (lintTitle (title b)) ";"
      writeProperty h "hc:notes" (notes b) "."
  return ()

-- some titles have " (meaning inches) in them
lintTitle :: Text -> Text
lintTitle = T.replace "\"" ""

-- some of my delicious files has "tag,,tag" - clean that up
writeTags :: Handle -> [Text] -> IO ()
writeTags h ts =
  forM_ ts $ \t ->
    when (t /= "")
      (writeProperty h "hc:tag" (lintTag t) ";")

lintTag :: Text -> Text
lintTag t =
  case T.strip t of
    "401K"          -> "401k"
    "aes"           -> "AES"
    "aeson"         -> "Aeson"
    "agda"          -> "Agda"
    "akka"          -> "Akka"
    "amazon"        -> "Amazon"
    "amazon_images" -> "Amazon_images"
    "android"       -> "Android"
    "android_app"   -> "Android"
    "argonaut"      -> "Argonaut"
    "asm"           -> "ASM"
    "awk"           -> "AWK"
    "bandcamp"      -> "Bandcamp"
    "beamer"        -> "Beamer"
    "bitcoin"       -> "Bitcoin"
    "calais"        -> "Calais"
    "casa_de_piedra_con_con_chile"
                    -> "Casa_de_Piedra_Con_Con_Chile"
    "chile"         -> "Chile"
    "cider"         -> "Cider"
    "clojure"       -> "Clojure"
    "typed_clojure" -> "Clojure_typed"
    "cloud_haskell" -> "Cloud_Haskell"
    "coen_brother"  -> "Coen_brothers"
    "coffeescript"  -> "CoffeeScript"
    "colorado"      -> "Colorado"
    "conkeror"      -> "Conkeror"
    "coq"           -> "Coq"
    "cpntools"      -> "CpnTools"
    "css"           -> "CSS"
    "datalet"       -> "datlet"
    "ditaa"         -> "Ditaa"
    "dns"           -> "DNS"
    "docbook"       -> "DocBook"
    "docker"        -> "Docker"
    "dropbox"       -> "Dropbox"
    "dropbox_alternative"
                    -> "Dropbox_alternative"
    "duckduckgo"    -> "DuckDuckGo"
    "elcim"         -> "Elcim"
    "elisp"         -> "Elisp"
    "elm"           -> "Elm"
    "emacs"         -> "Emacs"
    "spacemacs"     -> "Emacs_spacemacs"
    "erlang"        -> "Erlang"
    "ethereum"      -> "Ethereum"
    "frege"         -> "Frege"
    "ftp"           -> "FTP"
    "f#"            -> "F-sharp"
    "F#"            -> "F-sharp"
    "f_sharp"       -> "F-sharp"
    "f*"            -> "F-star"
    "facebook"      -> "Facebook"
    "facebook_connect"
                    -> "Facebook_connect"
    "facebook_friend_inviter"
                    -> "Facebook_friend_inviter"
    "fay"           -> "Fay"
    "fax"           -> "FAX"
    "firefox_extensions"
                    -> "Firefox_extensions"
    "frp"           -> "FRP"
    "galois"        -> "Galois"
    "gasprice"      -> "gas_price"
    "ghcjs"         -> "GHCJS"
    "ghc"           -> "GHC"
    "git"           -> "Git"
    "git_annex"     -> "Git_Annex"
    "github"        -> "Github"
    "gmail"         -> "Gmail"
    "gnus"          -> "Gnus"
    "google"        -> "Google"
    "google_analytics"-> "Google_analytics"
    "google_calendar"-> "Google_calendar"
    "google_maps"   -> "Google_maps"
    "google_refine" -> "Google_refine"
    "google_wave"   -> "Google_wave"
    "googlereader"  -> "Google_reader"
    "graphviz"      -> "GraphViz"
    "haddocks"      -> "Haddocks"
    "hakyll"        -> "Hakyll"
    "haskel "       -> "Haskell"
    "Haskel "       -> "Haskell"
    "haskel"        -> "Haskell"
    "haskell"       -> "Haskell"
    "haskell_example"
                    -> "Haskell_example"
    "haskell_extensions"
                    -> "Haskell_extensions"
    "haskell_job"   -> "Haskell_job"
    "haskell_mode"  -> "Haskell_mode"
    "haskell-mode"  -> "Haskell-mode"
    "haskell_pipes" -> "Haskell_pipes"
    "haskell_reflection"
                    -> "Haskell_reflection"
    "haskell_sandbox"
                    -> "Haskell_sandbox"
    "haskell_stack"
                    -> "Haskell_stack"
    "stackage"      -> "Haskell_stackage"
    "hackage"       -> "Haskell_hackage"
    "template_haskell" -> "Haskell_template"
    "hawaii"        -> "Hawaii"
    "haxe"          -> "Haxe"
    "html"          -> "HTML"
    "html5"         -> "HTML5"
    "http"          -> "HTTP"
    "httpd"         -> "HTTPd"
    "idris"         -> "Idris"
    "isabelle"      -> "Isabelle"
    "java"          -> "Java"
    "javadoc"       -> "Javadoc"
    "java_generics" -> "Java_generics"
    "java_lambda"   -> "Java_lambda"
    "java_reflection"-> "Java_reflection"
    "java_snow_leopard"   -> "Java_snow_leopard"
    "javascript"    -> "Javascript"
    "javascript_MVC_framework"    -> "Javascript_MVC_framework"
    "javaslang"     -> "Javaslang"
    "jini"          -> "Jini"
    "jitsi"         -> "Jitsi"
    "json"          -> "JSON"
    "jvm"           -> "JVM"
    "kadena"        -> "Kadena"
    "lambda_conf"   -> "LambdaConf"
    "latex"         -> "Latex"
    "ledger"        -> "Ledger"
    "linux"         -> "Linux"
    "liquid_haskell"-> "Liquid_Haskell"
    "lisp"          -> "Lisp"
    "little_snitch" -> "Little_Snitch"
    "llvm"          -> "LLVM"
    "microsoft"     -> "Microsoft"
    "ml"            -> "ML"
    "nixos"         -> "NixOS"
    "nix"           -> "Nix"
    "ocaml"         -> "OCaml"
    "paxos"         -> "Paxos"
    "pdf"           -> "PDF"
    "purescript"    -> "PureScript"
    "python"        -> "Python"
    "quickcheck"    -> "QuickCheck"
    "redex"         -> "Redex"
    "racket"        -> "Racket"
    "recusion"      -> "recursion"
    "rest"          -> "REST"
    "roku"          -> "Roku"
    "ruby"          -> "Ruby"
    "scala"         -> "Scala"
    "scalaz"        -> "ScalaZ"
    "scheme"        -> "Scheme"
    "sed"           -> "SED"
    "servant"       -> "Servant"
    "sicp"          -> "SICP"
    "skills_matter" -> "Skill_Matter"
    "skype"         -> "Skype"
    "slime"         -> "Slime"
    "smalltalk"     -> "Smalltalk"
    "tex"           -> "Tex"
    "tikz"          -> "TikZ"
    "tla"           -> "TLA"
    "unix"          -> "Unix"
    "ur/web"        -> "Ur/Web"
    "utah"          -> "Utah"
    "vpn"           -> "VPN"
    "zotero"        -> "Zotero"
    x               -> x

writeProperty :: Handle -> Text -> Text -> Text -> IO ()
writeProperty h p v sep = do
  hPutStr h "    "
  hPutStr h (T.encodeUtf8 p)
  hPutStr h " "
  hPutStr h "\""; hPutStr h (T.encodeUtf8 v); hPutStr h "\""
  hPutStr h " "
  BS.hPutStrLn h (T.encodeUtf8 sep)

------------------------------------------------------------------------------
-- parse delicious

type Parser = Parsec Text ()

parseDelicious :: FilePath -> IO (Either ParseError [Bookmark])
parseDelicious = parseFromFile delicious

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname = do
  i <- BS.readFile fname
  return (runParser p () fname (T.decodeUtf8 i))

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
  let ts = splitOn "," t
  spaces
  void $ char '>'
  ti <- manyTill anyChar (try (string "</A>"))
  spaces
  n <- bookmarkNote <|> return ""
  return $ Bookmark h a p ts (T.pack ti) n

bookmarkNote :: Parser Text
bookmarkNote = do
  void $ try (string "<DD>")
  fmap T.pack (manyTill anyChar endOfLine)

stringContents :: Parser Text
stringContents =
  fmap T.pack (manyTill anyChar (try (char '"')))

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

theEnd :: Parser Text
theEnd = do
  void $ many anyChar
  eof
  return ""
