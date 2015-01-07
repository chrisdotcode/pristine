module Pristine.File
    ( File(..)
    , gitFiles
    , makeReadme
    , pathToFile
    ) where

import Control.Applicative ((<$>))
import Control.Exception (IOException, try)
import Data.Char (toLower)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Map (Map, fromList, lookup)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(utctDay))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Filesystem.Path.CurrentOS
    ( (</>)
    , encodeString
    , decodeString
    , extension
    )
import System.Directory (getDirectoryContents)
import System.Posix.Files
    ( fileSize
    , getFileStatus
    , isDirectory
    , modificationTimeHiRes
    )
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (MarkdownSettings(msXssProtect), def, markdown)

fileIconMap :: Map Text String
fileIconMap = fromList $
    (map (\x -> (x, "apple")) ["app", "ipa", "ipsw", "saver"])
    ++ (map (\x -> (x , "archive"))
        [ "7z", "ace", "adf", "air", "apk", "arj", "bz2", "bzip", "cab", "d64"
        , "dmg", "git", "hdf", "ipf", "iso", "fdi", "gz", "jar", "lha", "lzh"
        , "lz", "lzma", "pak", "phar", "pkg", "pimp", "rar", "safariextz", "sfx"
        , "sit", "sitx", "sqx", "sublime-package", "swm", "tar", "tgz", "wim"
        , "wsz", "xar", "zip"
        ])
    ++ (map (\x -> (x, "book"))
        [ "aeh", "azw", "ceb", "chm", "epub", "fb2", "ibooks", "kf8", "lit"
        , "lrf", "lrx", "mobi", "pdb", "pdg", "prc", "xeb"
        ])
    ++ (map (\x -> (x, "calendar")) ["icbu","ics"])
    ++ (map (\x -> (x, "camera"))
        [ "3fr", "ari", "arw", "bay", "cap", "cr2", "crw", "dcs", "dcr", "dnf"
        , "dng", "eip", "erf", "fff", "iiq", "k25", "kdc", "mdc", "mef", "mof"
        , "mrw", "nef", "nrw", "obm", "orf", "pef", "ptx", "pxn", "r3d", "raf"
        , "raw", "rwl", "rw2", "rwz", "sr2", "srf", "srw", "x3f"
        ])
    ++ (map (\x -> (x, "code"))
        [ "ahk", "as", "asp", "aspx", "bat", "c", "cfm", "clj", "cmd", "cpp"
        , "css", "el", "erb", "g", "hml", "java", "js", "json", "jsp", "less"
        , "nsh", "nsi", "php", "php3", "pl", "py", "rb", "rhtml", "rss", "sass"
        , "scala", "scm", "scpt", "scptd", "scss", "sh", "shtml", "wsh", "xhtml"
        , "xml", "yml"
        ])
    ++ (map (\x -> (x, "cogs"))
        [ "cfg", "conf", "config", "ini", "htaccess", "htpasswd", "plist"
        , "sublime-settings", "xpy"
        ])
    ++ (map (\x -> (x, "database"))
        [ "bde", "crp", "db", "db2", "db3", "dbb", "dbf", "dbk", "dbs", "dbx"
        , "edb", "fdb", "frm", "fw", "fw2", "fw3", "gdb", "itdb", "mdb", "ndb"
        , "nsf", "rdb", "sas7mdb", "sql", "sqlite", "tdb", "wdb"
        ])
    ++ (map (\x -> (x, "download"))
        [ "!bt", "!qb", "!ut", "crdownload", "download", "opdownload", "part" ])
    ++ (map (\x -> (x, "envelope")) ["eml", "emlx", "mbox", "msg", "pst"])
    ++ (map (\x -> (x, "file-text-o"))
        [ "ans", "asc", "ascii", "csv", "diz", "latex", "log", "markdown", "md"
        , "nfo", "rst", "rtf", "tex", "text", "txt"
        ])
    ++ (map (\x -> (x, "film"))
        [ "3g2", "3gp", "3gp2", "3gpp", "asf", "avi", "bik", "bup", "divx"
        , "flv", "ifo", "m4v", "mkv", "mkv", "mov", "mp4", "mpeg", "mpg", "rm"
        , "rv", "ogv", "qt", "smk", "swf", "vob", "webm", "wmv", "xvid"
        ])
    ++ (map (\x -> (x, "font")) ["eot", "fon", "otf", "pfm", "ttf", "woff"])
    ++ (map (\x -> (x, "globe")) ["htm", "html", "xht", "xhtml"])
    ++ (map (\x -> (x, "group"))
        [ "abbu", "contact", "oab", "pab", "vcard", "vcf"
        ])
    ++ (map (\x -> (x, "link")) ["lnk", "url", "webloc"])
    ++ (map (\x -> (x, "linux")) ["bin", "deb", "rpm"])
    ++ (map (\x -> (x, "music"))
        [ "aac", "ac3", "aif", "aiff", "au", "caf", "flac", "it", "m4a", "m4p"
        , "med" , "mid", "mo3", "mod", "mp1", "mp2", "mp3", "mpc", "ned", "ra"
        , "ram", "ogg", "oma" , "s3m", "sid", "umx", "wav", "wv", "xm"
        ])
    ++ (map (\x -> (x, "picture-o"))
        [ "ai", "bmp", "cdr", "emf", "eps", "gif", "icns", "ico", "jp2", "jpe"
        , "jpeg", "jpg", "jpx", "pcx", "pict", "png", "psd", "psp", "svg", "tga"
        , "tif", "tiff", "webp", "wmf"])
    ++ (map (\x -> (x, "tasks")) ["ase", "clm", "clr", "gpl"])
    ++ (map (\x -> (x, "text"))
        [ "abw", "doc", "docm", "docs", "docx", "dot", "key", "numbers", "odb"
        , "odf", "odg", "odp", "odt", "ods", "otg", "otp", "ots", "ott", "pages"
        , "pdf", "pot", "ppt", "pptx", "sdb", "sdc", "sdd", "sdw", "sxi", "wp"
        , "wp4", "wp5", "wp6", "wp7", "wpd", "xls", "xlsx", "xps"
        ])
    ++ (map (\x -> (x, "windows"))
        [ "dll", "exe", "msi", "pif", "ps1", "scr", "sys"
        ])

folderIcon :: String
folderIcon = "folder"

defaultIcon :: String
defaultIcon = "file-text-o"

data File = File
    { name     :: String
    , icon     :: String
    , size     :: Integer
    , modified :: Day
    } deriving (Read, Show)

instance ToJSON File where
    toJSON (File n i s m) = object
        [ "name"     .= n
        , "icon"     .= i
        , "size"     .= s
        , "modified" .= show m
        ]

 -- The most clever pun in the world.
gitFiles :: FilePath -> IO [File]
gitFiles path = do
    files <- getDirectoryContents path
    -- Remove files listed in .gitignore, if it exists.
    ignored <- (try $
        readFile $ encodeString ((decodeString path) </> ".gitignore"))
        :: IO (Either IOException String)
    mapM pathToFile $ case ignored of
        Left _         -> files
        Right ignored' -> files \\ words ignored'

pathToFile :: FilePath -> IO File
pathToFile path = do
    fileStatus <- getFileStatus path

    let icon' = if isDirectory fileStatus
        then folderIcon
        else do
          let ext = extension $ decodeString path
          fromMaybe defaultIcon (ext >>= flip lookup fileIconMap)

    return File
        { icon     = icon'
        , name     = path
        , size     = toInteger $ fileSize fileStatus
        , modified = utctDay $ posixSecondsToUTCTime $
            modificationTimeHiRes fileStatus
        }

defaultParser :: String -> String
defaultParser = unpack . toStrict . renderHtml .
    markdown def { msXssProtect = False } . fromStrict . pack

markdownParser :: String -> String
markdownParser = defaultParser

readmeParsers :: Map Text (String -> String)
readmeParsers = fromList
    [ ("markdown", markdownParser)
    , ("md"      , markdownParser)
    ]

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

matchesReadme :: FilePath -> Bool
matchesReadme x = let x' = map toLower x in
    x' == "readme" ||
    x' == "readme.markdown" ||
    x' == "readme.md"

-- A valid README contains the text "readme", followed by a supported
-- extension, case insensitive.
-- Only markdown is currently supported.
findReadme :: FilePath -> IO (Maybe FilePath)
findReadme path = safeHead . filter matchesReadme <$> getDirectoryContents path

parseReadme :: FilePath -> IO String
parseReadme readme = do
    let ext = extension $ decodeString readme
    let readmeParser = fromMaybe
            defaultParser (ext >>= flip lookup readmeParsers)
    readmeParser <$> readFile readme

makeReadme :: FilePath -> IO String
makeReadme path =
    findReadme path >>= maybe (return "No README supplied.") parseReadme
