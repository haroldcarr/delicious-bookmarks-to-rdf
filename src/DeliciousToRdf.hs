{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DeliciousToRdf where

-- NOTE: does not handle multi-line notes in <DD>

{-
parseDelicious "test/test-data/delicious-2017-07-14.html"
hcDeliciousToRdf "test/test-data/delicious-2017-07-14.html" "/tmp/delicious-2017-07-14.ttl"
-}

import           ClassyPrelude         as CP hiding (many, try, (<|>))
import           Data.ByteString       as BS hiding (hPutStrLn)
import           Data.ByteString.Char8 as BS (hPutStrLn)
import           Data.Text             as T (pack, replace, splitOn, strip,
                                             unpack)
import           Data.Text.Encoding    as T (decodeUtf8, encodeUtf8)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format      as F (defaultTimeLocale, formatTime)
import qualified Prelude               as P (read)
import           System.IO             as IO (IOMode (WriteMode), withFile)
import           Text.Parsec

------------------------------------------------------------------------------

deliciousToRdf :: ([Bookmark] -> [Bookmark]) -> FilePath -> FilePath -> IO ()
deliciousToRdf filterBookmarks i o = do
  p <- parseDelicious i
  let bs = either (error . show) filterBookmarks p
  writeRdf o bs

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
  href0 <- stringContents
  spaces
  void $ string "ADD_DATE=\""
  addDate0 <- stringContents
  spaces
  void $ string "PRIVATE=\""
  private0 <- stringContents
  let private' = if private0 == "0" then "false" else "true"
  spaces
  void $ string "TAGS=\""
  tags0 <- stringContents
  let tags' = CP.filter (/="") $ CP.map strip (splitOn "," tags0) -- get rid of empty tags
  spaces
  void $ char '>'
  title0 <- manyTill anyChar (try (string "</A>"))
  spaces
  n <- bookmarkNote <|> return ""
  return $ Bookmark href0 addDate0 private' tags' (T.pack title0) n

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

------------------------------------------------------------------------------
-- write RDF

hc :: Text
hc = "@prefix hc: <http://openhc.org/syntax-ns#> ."

xsd :: Text
xsd = "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ."

writeRdf :: FilePath -> [Bookmark] -> IO ()
writeRdf o bs =
  withFile o WriteMode $ \h -> do
    BS.hPutStrLn h (T.encodeUtf8 hc)
    BS.hPutStrLn h (T.encodeUtf8 xsd)
    forM_ bs $ writeBookmark h

writeBookmark :: Handle -> Bookmark -> IO ()
writeBookmark h b = do
  BS.hPutStrLn h ""
  hPutStr h "<"; hPutStr h (T.encodeUtf8 (href b)); BS.hPutStrLn h ">"
  writePropertyAndValue h "hc:addDate" (addDate b) ";" False -- NOTE: True/False viz hcTime
  writePropertyAndValue h "hc:private" (private b) ";" False
  writeTags h (tags b)
  if notes b == "" then
      writePropertyAndValue h "hc:title" (title b) "." True
  else do
      writePropertyAndValue h "hc:title" (title b) ";" True
      writePropertyAndValue h "hc:notes" (notes b) "." True
  return ()

writeTags :: Handle -> [Text] -> IO ()
writeTags h ts = forM_ ts $ \t -> writePropertyAndValue h "hc:tag" t ";" True

writePropertyAndValue :: Handle -> Text -> Text -> Text -> Bool -> IO ()
writePropertyAndValue h p v sep b = do
  hPutStr h "    "
  hPutStr h (T.encodeUtf8 p)
  hPutStr h " "
  when b $ hPutStr h "\"";
  hPutStr h (T.encodeUtf8 v);
  when b $ hPutStr h "\""
  hPutStr h " "
  BS.hPutStrLn h (T.encodeUtf8 sep)

------------------------------------------------------------------------------
-- transformation/lint customized for HC

hcDeliciousToRdf :: FilePath -> FilePath -> IO ()
hcDeliciousToRdf = deliciousToRdf hcFilterBookmarks

hcFilterBookmarks :: [Bookmark] -> [Bookmark]
hcFilterBookmarks = CP.map f
  where f b@Bookmark{..} = b { addDate = hcTime addDate
                             , title   = hcLintTitle title
                             , tags    = CP.map hcLintTag tags
                             }

hcTime :: Text -> Text
hcTime x =
  let fTime = formatTime
                F.defaultTimeLocale
                "%FT%T %Z"
                $ posixSecondsToUTCTime
                $ realToFrac
                  (P.read (T.unpack x) :: Int)
  -- TODO : This commented out line is a correct format for RDF dateTime.
  --        But the RDF browser I am using has a bug.
  --        So use the "incorrect" format to workaround bug for now.
  -- in "\"" <> T.replace " UTC" "Z" (T.pack fTime) <> "\"" <>  "^^xsd:dateTime"
  in "\"" <> T.pack fTime <> "\""

-- some titles have " (meaning inches) in them
hcLintTitle :: Text -> Text
hcLintTitle = T.replace "\"" ""

hcLintTag :: Text -> Text
hcLintTag t =
  case t of
    "401K"          -> "401k"
    "access_manager"-> "Sun_Access_Manager"
    "adsense"       -> "AdSense"
    "aes"           -> "AES"
    "aeson"         -> "Aeson"
    "agda"          -> "Agda"
    "ajax"          -> "AJAX"
    "akka"          -> "Akka"
    "amazon"        -> "Amazon"
    "amazon_images" -> "Amazon_images"
    "angular.js"    -> "Angular.js"
    "android"       -> "Android"
    "android_app"   -> "Android"
    "appcelerator"  -> "Appcelerator"
    "apple-chile"   -> "Chile_Apple"
    "argonaut"      -> "Argonaut"
    "arrow"         -> "arrows"
    "asm"           -> "ASM"
    "ats"           -> "ATS"
    "awk"           -> "AWK"
    "backbone.js"   -> "Backbone.js"
    "bandcamp"      -> "Bandcamp"
    "bash"          -> "BASH"
    "beamer"        -> "Beamer"
    "beanshell"     -> "BeanShell"
    "bible"         -> "Bible"
    "bitcoin"       -> "Bitcoin"
    "bookl"         -> "booklist"
    "cabal"         -> "Cabal"
    "calais"        -> "Calais"
    "opencalais"    -> "Calais_open"
    "casa_de_piedra_con_con_chile"
                    -> "Casa_de_Piedra_Con_Con_Chile"
    "chile"         -> "Chile"
    "chromebook"    -> "ChromeBook"
    "cider"         -> "Cider"
    "cis194"        -> "CIS194"
    "clojure"       -> "Clojure"
    "typed_clojure" -> "Clojure_typed"
    "color_blind"   -> "colorblind"
    "comonad"       -> "comonads"
    "conferences"   -> "conference"
    "confluent"     -> "Confluent"
    "coen_brothers" -> "Coen_brothers"
    "coffeescript"  -> "CoffeeScript"
    "colorado"      -> "Colorado"
    "conkeror"      -> "Conkeror"
    "coq"           -> "Coq"
    "cpntools"      -> "CpnTools"
    "css"           -> "CSS"
    "csv"           -> "CSV"
    "curl"          -> "CURL"
    "datalet"       -> "datlet"
    "db"            -> "DB"
    "dbpedia"       -> "DBpedia"
    "delicious"     -> "Delicious"
    "digidesign"    -> "DigiDesign"
    "ditaa"         -> "Ditaa"
    "dns"           -> "DNS"
    "docbook"       -> "DocBook"
    "docker"        -> "Docker"
    "dom4j"         -> "Dom4J"
    "dropbox"       -> "Dropbox"
    "dropbox_alternative"
                    -> "Dropbox_alternative"
    "dublin"        -> "Dublin"
    "duckduckgo"    -> "DuckDuckGo"
    "elcim"         -> "Elcim"
    "elisp"         -> "Elisp"
    "elm"           -> "Elm"
    "emacs"         -> "Emacs"
    "create-emacs-mode"
                    -> "Emacs_create_a_mode"
    "eclim"         -> "Emacs_eclim"
    "impatient-mode"-> "Emacs_impatient-mode"
    "magit"         -> "Emacs_Magit"
    "malabar-mode"  -> "Emacs_malabar-mode"
    "mode-line"     -> "Emacs_mode-line"
    "org"           -> "Emacs_org-mode"
    "org-mode"      -> "Emacs_org-mode"
    "org-plot"      -> "Emacs_org-plot"
    "org2blog"      -> "Emacs_org2blog"
    "spacemacs"     -> "Emacs_spacemacs"
    "use-package"   -> "Emacs_use-package"
    "yasnippet"     -> "Emacs_yasnippet"
    "ebook"         -> "ebooks"
    "edu"           -> "education"
    "ent"           -> "ENT"
    "erlang"        -> "Erlang"
    "lisp_flavored_erlang"
                    -> "Erlang_lisp_flavored"
    "ethereum"      -> "Ethereum"
    "evernote"      -> "Evernote"
    "exchange_rater"-> "exchange_rates"
    "fink"          -> "Fink"
    "flickr"        -> "Flickr"
    "free_monad"    -> "free_monads"
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
    "fake_book"     -> "jazz_fake_book"
    "fax"           -> "FAX"
    "firefox_extensions"
                    -> "Firefox_extensions"
    "foldl"         -> "fold"
    "fortress"      -> "Fortress"
    "fpcomplete"    -> "FPComplete"
    "freebase"      -> "Freebase"
    "frp"           -> "functional_reactive_programming"
    "gadt"          -> "GADT"
    "galaxy_nexus"  -> "Galaxy_Nexus"
    "galois"        -> "Galois"
    "gasprice"      -> "gas_price"
    "ghcjs"         -> "GHCJS"
    "ghc"           -> "GHC"
    "git"           -> "Git"
    "git-annex"     -> "Git-Annex"
    "github"        -> "Github"
    "gmail"         -> "Gmail"
    "gnuplot"       -> "Gnuplot"
    "gnus"          -> "Gnus"
    "google"        -> "Google"
    "google_analytics"
                    -> "Google_analytics"
    "google_calendar"
                    -> "Google_calendar"
    "google_maps"   -> "Google_maps"
    "google_refine" -> "Google_refine"
    "google_wave"   -> "Google_wave"
    "googlereader"  -> "Google_reader"
    "gpg"           -> "GPG"
    "gradle"        -> "Gradle"
    "graphviz"      -> "GraphViz"
    "gtd"           -> "GTD"
    "hackernews"    -> "Hacker_News"
    "hask"          -> "Haskell"
    "haskel "       -> "Haskell"
    "Haskel "       -> "Haskell"
    "haskel"        -> "Haskell"
    "haskell"       -> "Haskell"
    "haskell_example"
                    -> "Haskell_example"
    "haskell_extensions"
                    -> "Haskell_extensions"
    "fay"           -> "Haskell_Fay"
    "hoogle"        -> "Haskell_hoogle"
    "haddocks"      -> "Haskell_Haddocks"
    "Hakyll"        -> "Haskell_Hakyll"
    "hakyll"        -> "Haskell_Hakyll"
    "distributed_haskell"
                    -> "Haskell_distributed"
    "TypedHoles"    -> "Haskell_type_holes"
    "type_classes"  -> "Haskell_type_class"
    "typeclass"     -> "Haskell_type_class"
    "type_class"    -> "Haskell_type_class"
    "type_level"    -> "Haskell_type_level"
    "type_family"   -> "Haskell_type_level"
    "haskell_job"   -> "Haskell_job"
    "haskell_mode"  -> "Haskell-mode"
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
    "doctest"       -> "Haskell_testing"
    "template_haskell"
                    -> "Haskell_template"
    "cloud_haskell" -> "Haskell_Cloud"
    "liquid_haskell"-> "Haskell_Liquid"
    "haxl"          -> "Haskell_Haxl"
    "hspec"         -> "Haskell_testing"
    "opaleye"       -> "Haskell_sql"
    "parsec"        -> "Haskell_parsing"
    "parser"        -> "Haskell_parsing"
    "reflex"        -> "Haskell_Reflex"
    "servant"       -> "Haskell_Servant"
    "shake"         -> "Haskell_Shake"
    "STRef"         -> "Haskell_STRef"
    "shell-conduit" -> "Haskell_Shell-Conduit"
    "tasty"         -> "Haskell_testing"
    "typeclassopedia"
                    -> "Haskell_typeclassopedia"
    "yesod"         -> "Haskell_Yesod"
    "hawaii"        -> "Hawaii"
    "haxe"          -> "Haxe"
    "health_care"   -> "healthcare"
    "html"          -> "HTML"
    "html5"         -> "HTML5"
    "htpc"          -> "HTPC"
    "http"          -> "HTTP"
    "httpd"         -> "HTTPd"
    "ide"           -> "IDE"
    "idris"         -> "Idris"
    "ilc"           -> "Lisp_international_conference"
    "indigo"        -> "Indigo"
    "indo_european" -> "Indo-European"
    "infocard"      -> "InfoCard"
    "infrarrealismo"-> "Infrarrealismo"
    "instagram"     -> "Instagram"
    "intellij"      -> "IntelliJ"
    "inter"         -> "interview_question"
    "interv"        -> "interview"
    "ipad"          -> "iPad"
    "isabelle"      -> "Isabelle"
    "ispell"        -> "Ispell"
    "ivory"         -> "Ivory_language"
    "j2s"           -> "Java2Script"
    "java"          -> "Java"
    "javadoc"       -> "Javadoc"
    "java_generics" -> "Java_generics"
    "java_lambda"   -> "Java_lambda"
    "java_reflection"
                    -> "Java_reflection"
    "java_snow_leopard"
                    -> "Java_snow_leopard"
    "dynamic_java"  -> "Java_dynamic"
    "javascript"    -> "Javascript"
    "javascript_MVC_framework"
                    -> "Javascript_MVC_framework"
    "javaslang"     -> "Javaslang"
    "las_vegas_jazz"-> "jazz_Las_Vegas"
    "jazz_san_francisco"
                    -> "jazz_San_Francisco"
    "jdee"          -> "JDEE"
    "jet_lag"       -> "jetlag"
    "jini"          -> "Jini"
    "jitsi"         -> "Jitsi"
    "json"          -> "JSON"
    "jquery"        -> "jQuery"
    "json-schema"   -> "JSON-Schema"
    "jvm"           -> "JVM"
    "kadena"        -> "Kadena"
    "kindle"        -> "Kindle"
    "knee"          -> "knees"
    "kynetx"        -> "Kynetx"
    "lambda_conf"   -> "LambdaConf"
    "latex"         -> "Latex"
    "latex_cv"      -> "Latex_cv"
    "tex"           -> "Latex_Tex"
    "tikz"          -> "Latex_TikZ"
    "modernvm"      -> "Latex_cv"
    "ledger"        -> "Ledger"
    "lenses"        -> "lens"
    "linked_daa"    -> "linked_data"
    "linux"         -> "Linux"
    "lisp"          -> "Lisp"
    "list"          -> "lists"
    "little_snitch" -> "Little_Snitch"
    "llvm"          -> "LLVM"
    "lulu"          -> "Lulu"
    "macbook"       -> "MacBook"
    "macbook_external_display"
                    -> "MacBook_external_display"
    "mendeley"      -> "Mendeley"
    "microsoft"     -> "Microsoft"
    "midi"          -> "MIDI"
    "mind_map"      -> "mindmap"
    "mindraider"    -> "MindRaider"
    "ml"            -> "ML"
    "mooc"          -> "MOOC"
    "mozart"        -> "Mozart"
    "mu4e"          -> "Mu4e"
    "musicbrainz"   -> "MusicBrainz"
    "myspace"       -> "MySpace"
    "myth"          -> "mythology"
    "n3"            -> "RDF_N3"
    "neil_mitchell" -> "Neil_Mitchell"
    "neitzche"      -> "Friedrich_Nietzsche"
    "netbeans"      -> "NetBeans"
    "neuralnets"    -> "neural_networks"
    "nixos"         -> "NixOS"
    "nix"           -> "Nix"
    "node"          -> "Node.js"
    "nodejs"        -> "Node.js"
    "node.js"       -> "Node.js"
    "nosql"         -> "NoSQL"
    "no_sql"        -> "NoSQL"
    "nuskin"        -> "Nuskin"
    "ocaml"         -> "OCaml"
    "oleg"          -> "Oleg_Kiselyov"
    "osx"           -> "MacOS"
    "os_x"          -> "MacOS"
    "pandoc"        -> "Pandoc"
    "paxos"         -> "Paxos"
    "pdf"           -> "PDF"
    "perl"          -> "Perl"
    "phonegap"      -> "PhoneGap"
    "pi"            -> "Pi"
    "picasa"        -> "Picasa"
    "plantuml"      -> "PlantUML"
    "poetry_ma"     -> "poetry_magazine"
    "portland_software_company"
                    -> "Portland_software_company"
    "postgres"      -> "PostgreSQL"
    "prolog"        -> "Prolog"
    "proof_general" -> "Proof_General"
    "psygnisfive"   -> "Psygnisfive"
    "programming-language"
                    -> "programming_language"
    "powerpoint"    -> "Powerpoint"
    "Purescript"    -> "PureScript"
    "purescript"    -> "PureScript"
    "python"        -> "Python"
    "qi"            -> "Qi"
    "quickcheck"    -> "QuickCheck"
    "racket"        -> "Racket"
    "rad"           -> "RAD"
    "radiolab"      -> "RadioLab"
    "raspberry_pi"  -> "Raspberry_Pi"
    "rdf"           -> "RDF"
    "rdf-sql-rest"  -> "RDF_SQL_REST"
    "rdf_reification"
                    -> "RDF_reification"
    "triple_store"  -> "RDF_triple_store"
    "rdfs"          -> "RDFS"
    "rdma"          -> "RDMA"
    "redex"         -> "Redex"
    "recusion"      -> "recursion"
    "reddit"        -> "Reddit"
    "rest"          -> "REST"
    "reveal.js"     -> "Reveal.js"
    "roku"          -> "Roku"
    "ruby"          -> "Ruby"
    "s3"            -> "S3"
    "safari"        -> "Safari"
    "samba"         -> "Samba"
    "sapolsky"      -> "Sapolsky"
    "sawsdl"        -> "SAWSDL"
    "scala"         -> "Scala"
    "scalaz"        -> "ScalaZ"
    "shapeless"     -> "Scala_Shapeless"
    "scheme"        -> "Scheme"
    "sealevel"      -> "sea_level"
    "secondlife"    -> "Second Life"
    "sed"           -> "SED"
    "sench"         -> "Sencha"
    "sencha"        -> "Sencha"
    "sencha_touch"  -> "Sencha"
    "sibelius"      -> "Sibelius"
    "sicp"          -> "SICP"
    "simile"        -> "Simile"
    "skij"          -> "Skij"
    "skos"          -> "Skos"
    "skills_matter" -> "Skill_Matter"
    "skype"         -> "Skype"
    "slime"         -> "Slime"
    "slimserver"    -> "SlimServer"
    "smalltalk"     -> "Smalltalk"
    "softw"         -> "software"
    "spanish"       -> "Spanish"
    "sparql"        -> "Sparql"
    "spine.js"      -> "Spine.js"
    "squeak"        -> "Squeak"
    "sql"           -> "SQL"
    "ssh"           -> "SSH"
    "sshuttle"      -> "SSHuttle"
    "sswap"         -> "SSWAP"
    "standard_ml"   -> "ML"
    "strange_loop"  -> "Strange_Loop"
    "strongtalk"    -> "StrongTalk"
    "stonehenge"    -> "Stonehenge"
    "subversion"    -> "Subversion"
    "summer_jazz"   -> "jazz_summer"
    "suni"          -> "Suni"
    "sweet_tools"   -> "Sweet_Tools"
    "swift"         -> "Swift"
    "tapestry"      -> "Tapestry"
    "telegram"      -> "Telegram"
    "telehash"      -> "Telehash"
    "tla"           -> "TLA"
    "traceroute"    -> "Traceroute"
    "tracking_"     -> "tracking"
    "travis_ci"     -> "Travis_ci"
    "truecrypt"     -> "Truecrypt"
    "tv"            -> "TV"
    "twine"         -> "Twine"
    "twitter"       -> "Twitter"
    "ulix"          -> "Ulix"
    "unison"        -> "Unison"
    "unix"          -> "Unix"
    "ur/web"        -> "Ur/Web"
    "utah"          -> "Utah"
    "virtualbox"    -> "VirtualBox"
    "virtuoso"      -> "Virtuoso"
    "visual"        -> "visualization"
    "vpn"           -> "VPN"
    "webdesign"     -> "web_design"
    "wikimedia"     -> "Wikimedia"
    "wikipedia"     -> "Wikipedia"
    "windows"       -> "Windows"
    "windows_xp_recovery"
                    -> "Windows_xp_recovery"
    "win98"         -> "Windows_98"
    "wired_magazine"-> "Wired_magazine"
    "wireshark"     -> "Wireshark"
    "wordnet"       -> "WordNet"
    "wordpress"     -> "WordPress"
    "xmission"      -> "XMission"
    "xmonad"        -> "XMonad"
    "xquery"        -> "XQuery"
    "xtend"         -> "XTend"
    "yampa"         -> "Yampa"
    "zalora"        -> "Zalora"
    "zemanta"       -> "Zemanta"
    "zfs"           -> "ZFS"
    "zitgist"       -> "Zitgist"
    "zoneminder"    -> "ZoneMinder"
    "zotero"        -> "Zotero"
    x               -> x
