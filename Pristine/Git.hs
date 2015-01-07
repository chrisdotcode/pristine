module Pristine.Git
    ( processRepo
    , Repo(..)
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import qualified Data.Set as Set (Set, empty, insert, map)
import Data.Git.Repository (RefName(refNameRaw), branchList, tagList)
import Data.Git.Storage (openRepo)
import Data.Tagged (Tagged(Tagged))
import Git
    ( Commit(commitAuthor)
    , CommitAuthor
    , CommitEmail
    , Signature(signatureEmail, signatureName)
    , lookupCommit
    , resolveReference
    , traverseCommits
    , withRepository
    )
import Git.Libgit2 (LgRepo, lgFactory)
import Filesystem.Path.CurrentOS ((</>), encodeString, decodeString)

data Contributor = Contributor
    { author :: CommitAuthor
    , email  :: CommitEmail
    } deriving (Eq, Ord, Show)

instance ToJSON Contributor where
    toJSON (Contributor a e) = object [ "author" .= a, "email" .= e ]

data Repo = Repo
    { numCommits   :: Int
    , latestCommit :: Commit LgRepo
    , branches     :: Set.Set String
    , tags         :: Set.Set String
    , contributors :: Set.Set Contributor
    }

empty :: Repo
empty = Repo
    { numCommits   = 0
    , latestCommit = undefined -- Living dangerously.
    , branches     = Set.empty
    , tags         = Set.empty
    , contributors = Set.empty
    }

processRepo :: FilePath -> IO Repo
processRepo path = do
    repository <- openRepo $ (decodeString $ path) </> ".git"
    branches'  <- branchList repository
    tags'      <- tagList repository
    withRepository lgFactory path $ do
        mref <- resolveReference "HEAD"
        case mref of
            Just ref -> do
                commits <- traverseCommits lookupCommit $ Tagged ref
                return $ processCommits empty
                    { branches = Set.map refNameRaw branches'
                    , tags     = Set.map refNameRaw tags'
                    } commits
            Nothing -> error $
                (encodeString $ (decodeString path) </> ".git" </> "HEAD") ++
                " not found."

processCommits :: Repo -> [Commit LgRepo] -> Repo
processCommits repo [] = repo
processCommits repo (commit:[]) =
    processCommits
        (addContrib commit $ incCommits repo){ latestCommit = commit } []
processCommits repo (commit:commits) =
    processCommits (addContrib commit $ incCommits repo) commits

incCommits :: Repo -> Repo
incCommits repo' = repo' { numCommits = numCommits repo' + 1 }

addContrib :: Commit LgRepo -> Repo -> Repo
addContrib commit' repo'' = repo''
    { contributors = Set.insert Contributor
        { author = signatureName $ commitAuthor commit'
        , email  = signatureEmail $ commitAuthor commit'
        } (contributors repo'')
    }
