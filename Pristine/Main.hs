module Main (main) where

import Control.Applicative ((<$>), (<*>), optional)
import Data.Char (toLower)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import Prelude hiding (putStrLn, writeFile)

import Data.Aeson ((.=), ToJSON(..), Value, object)
import qualified Data.Set as Set (size)
import Data.Text (Text, pack, toTitle)
import Data.Text.IO (putStrLn, writeFile)
import Filesystem.Path.CurrentOS
    ( encodeString
    , decodeString
    , filename
    )
import Git
    ( commitAuthor
    , commitLog
    , signatureEmail
    , signatureName
    , signatureWhen
    )
import Options.Applicative
  ( Mod
  , OptionFields
  , ParserInfo
  , Parser
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , infoOption
  , long
  , metavar
  , option
  , progDesc
  , short
  , str
  , strArgument
  , strOption
  , value
  )
import System.Directory (canonicalizePath)
import Web.Simple.Templates.Language (compileTemplate, renderTemplate)

import Pristine.File
    ( File(..)
    , gitFiles
    , makeReadme
    )
import Pristine.Git
    ( Repo(..)
    , branches    
    , contributors
    , latestCommit
    , numCommits  
    , processRepo
    , tags        
    )
import Paths_pristine (version)

pristineHTML :: FilePath
pristineHTML = "web/pristine.html"

data License = None
             | GPL2
             | GPL3
             | LGPL2
             | LGPL3
             | AGPL3
             | BSD2
             | BSD3
             | MIT
             | MPL2
             | Apache2
             | PublicDomain
             | AllRightsReserved
             | Other String
             deriving (Read, Show)

strToLicense :: String -> License
strToLicense s
  | l == "none"     = None
  | l == "gpl2"     = GPL2
  | l == "gpl3"     = GPL3
  | l == "lgpl2"    = LGPL2
  | l == "lgpl3"    = LGPL3
  | l == "agpl3"    = AGPL3
  | l == "bsd2"     = BSD2
  | l == "bsd3"     = BSD3
  | l == "mit"      = MIT
  | l == "mpl2"     = MPL2
  | l == "apache2"  = Apache2
  | l == "public"   = PublicDomain
  | l == "reserved" = AllRightsReserved
  | otherwise       = Other s
  where
    l = map toLower s

instance ToJSON License where
    toJSON None              = "None"     
    toJSON GPL2              = "GPL2"     
    toJSON GPL3              = "GPL3"     
    toJSON LGPL2             = "LGPL2"    
    toJSON LGPL3             = "LGPL3"    
    toJSON AGPL3             = "AGPL3"    
    toJSON BSD2              = "BSD2"     
    toJSON BSD3              = "BSD3"     
    toJSON MIT               = "MIT"      
    toJSON MPL2              = "MPL2"     
    toJSON Apache2           = "Apache 2"  
    toJSON PublicDomain      = "Public Domain"   
    toJSON AllRightsReserved = "All Rights Reserved" 
    toJSON (Other s)         = toJSON s

data Flags = Flags
    { dir          :: FilePath
    , name         :: Maybe Text
    , license      :: Maybe License
    , output       :: Maybe FilePath
    , maintainer   :: Maybe Text
    , cloneLink    :: Maybe Text
    , downloadLink :: Maybe Text
    } deriving (Read, Show)

textOption :: Mod OptionFields String -> Parser Text
textOption = (pack <$>) . strOption

flags :: Parser Flags
flags = Flags
    <$> strArgument
        (  help "The directory containing the project's git repo."
        <> metavar "DIRECTORY"
        <> value "."
        )
    <*> optional (textOption $
           help ("The project's name" ++
                 " (defaults to the repository's directory name).")
        <> long "name"
        <> short 'n'
        )
    <*> optional (option (strToLicense <$> str) $
           help (mconcat
            [ "The license for the project. This is one of:"
            , "\n    none"
            , "\n    gpl2"
            , "\n    gpl3"
            , "\n    lgpl2"
            , "\n    lgpl3"
            , "\n    agpl3"
            , "\n    bsd2"
            , "\n    bsd3"
            , "\n    mit"
            , "\n    mpl2"
            , "\n    apache2"
            , "\n    public"
            , "\n    reserved"
            ])
        <> long "license"
        <> metavar "LICENSE"
        <> short 'l'
        )
    <*> optional (strOption $
           help "The directory to ouput the generated file."
        <> long "output"
        <> metavar "OUTPUT"
        <> short 'o'
        )
    <*> optional (textOption $
           help "The maintainer of the project."
        <> long "maintainer"
        <> metavar "MAINTAINER"
        <> short 'm'
        )
    <*> optional (textOption $
           help "The project's git clone location."
        <> long "clone-link"
        <> metavar "CLONELINK"
        <> short 'e'
        )
    <*> optional (textOption $
           help "The project's download/home page, if one is offered."
        <> long "download-link"
        <> metavar "DOWNLOADLINK"
        <> short 'd'
        )

versionInfo :: Parser (a -> a)
versionInfo = infoOption (showVersion version)
    (  long "version"
    <> help "Print version information."
    )

options :: ParserInfo Flags
options = info (helper <*> versionInfo <*> flags)
    (  fullDesc
    <> header "Pristine - The git respository static summary generator."
    <> progDesc "Generates static summaries of git repositories."
    )

main :: IO ()
main = do
    flags'   <- do
        flgs <- execParser options
        path <- canonicalizePath $ dir flgs
        return $ flgs { dir = path }
    repo     <- processRepo $ dir flags'
    files    <- gitFiles $ dir flags'
    readme   <- makeReadme $ dir flags'
    pristine <- readFile pristineHTML

    let context = makeContext flags' repo readme files
    case (compileTemplate $ pack pristine) of
        Left e -> error $
            "Error in pristine template. Please file a bug report, supplying" ++
            " the following information:\n" ++ e
        Right template ->
            maybe putStrLn writeFile (output flags') $
                (renderTemplate template mempty context)

makeContext :: Flags -> Repo -> String -> [File] -> Value
makeContext flags' repo readme files = object
    [ "numContributors" .= Set.size (contributors repo)
    , "numBranches"     .= Set.size (branches repo)
    , "numTags"         .= Set.size (tags repo)
    , "readme"          .= readme
    , "version"         .= showVersion version
    , "flags" .= object
        [ "dir"          .= dir flags'
        , "name"         .= name'
        , "cloneLink"    .= cloneLink flags'
        , "downloadLink" .= downloadLink flags'
        , "license"      .= license flags'
        ]
    , "repo" .= object
        [ "maintainer"   .= maintainer flags'
        , "numCommits"   .= numCommits repo
        , "contributors" .= contributors repo
        , "branches"     .= branches repo
        , "tags"         .= tags repo
        , "latestCommit" .= object
            [ "commitAuthor" .= object
                [ "signatureName"  .= signatureName author
                , "signatureEmail" .= signatureEmail author
                , "signatureWhen"  .= signatureWhen author
                ]
            , "commitMessage" .= commitLog (latestCommit repo)
            ]
        , "files" .= files
        ]
    ]
  where
    author = commitAuthor $ latestCommit repo
    name' = case (Main.name flags') of
        Just n  -> n
        Nothing -> toTitle $ pack $ encodeString $ filename $ decodeString $
            dir flags'
