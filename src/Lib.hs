{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( getRepo,
    formatRepo,
    getConfig,
    pushCommitAndMakePR,
    buildContent
    ) where

import Data.Text         (Text, pack)
import qualified Data.Text.IO        as T
import qualified Data.ByteString.Char8 as B
import Data.String.Interpolate (i)
import Data.Time (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Generics

import qualified GitHub.Endpoints.Repos as Repos
import qualified GitHub.Endpoints.Repos.Contents as Contents
import qualified GitHub.Endpoints.GitData.References as Refs
import qualified GitHub.Endpoints.GitData.Commits as Commits
import qualified GitHub.Endpoints.GitData.Trees as Trees
import qualified GitHub.Endpoints.PullRequests as PR
import qualified GitHub.Data.Definitions as G
import qualified Data.Vector as V
import  GitHub.Data.Name
import  GitHub.Data.Content
import qualified GitHub.Auth as Auth
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Data.Either.Combinators (mapLeft)
import Data.ByteString.Base64 as B64
import Control.Arrow as A
import Data.Text.Encoding
import Data.Yaml (decodeEither', FromJSON)

data Error = 
     GitHubError G.Error
    | IOError Text
    | NoCommit
    | ConfigFileMalformed
    deriving (Show)


data RepoConf = RepoConf {
    branch :: Text
} deriving(Generic, Show)

instance FromJSON RepoConf

defaultConf :: RepoConf
defaultConf :: RepoConf "master"

getRepo :: Text -> Text -> IO (Either Repos.Error Repos.Repo)
getRepo name owner = 
    let auth = Auth.OAuth $ B.pack "71d3449c9551dfa363ad5dc365c125b7f19fd509"
    in  Repos.repository' (Just auth) (N name) (N owner)

formatRepo :: Repos.Repo -> Text
formatRepo = Repos.getUrl . Repos.repoUrl

printG :: Show a => a -> EitherT Error IO ()
printG = (handleIOEitherT  (\_ -> IOError "Could not print to screen!")) . T.putStrLn . pack . show

getContent :: Content -> B.ByteString
getContent (ContentFile (ContentFileData _ _ _ contentFileContent)) = 
    B64.decodeLenient $ encodeUtf8 contentFileContent
getContent _ = ""

getConfig :: String -> Text -> Text -> EitherT Error  IO RepoConf
getConfig apiKey name owner =
    let 
        auth = Auth.OAuth $ B.pack apiKey 
        mkGhRq = newEitherT . fmap (mapLeft GitHubError)
    in
        do
            content <- mkGhRq $ Contents.contentsFor' (Just auth) (N name) (N owner) "cyment.yaml" Nothing
            hoistEither $ A.left (\_->ConfigFileMalformed) $ decodeEither' $ getContent content

            


pushCommitAndMakePR :: String -> Text-> Text -> RepoConf -> Text -> Text -> Text -> Text -> EitherT Error  IO ()
pushCommitAndMakePR apiKey name owner conf commentId path commenter content =
    let 
        auth = Auth.OAuth $ B.pack apiKey
        mkGhRq = newEitherT . fmap (mapLeft GitHubError)
        commitMsg = [i|#{commenter}'s comment at #{path}|]
        commentHead=[i|refs/heads/comment-#{commentId}|]
        commentPath=[i|comments/#{path}/#{commentId}.markdown|]
        targetBranch = branch conf
    in
    do
        ref <- mkGhRq $ Refs.reference' (Just auth) (N name) (N owner) (N [i|heads/#{targetBranch}|])
        printG ref
        lastCommit <- mkGhRq $ Commits.commit' (Just auth) (N name) (N owner) (N $ Refs.gitObjectSha $ Refs.gitReferenceObject ref)
        printG lastCommit
        let newTree = Trees.NewTree (Commits.treeSha $ Commits.gitCommitTree lastCommit) (V.singleton $ Trees.NewGitTree commentPath "100644" content)
        tree <- mkGhRq $ Trees.createTree auth (N name) (N owner) newTree
        printG tree
        commitSha <- hoistMaybe NoCommit $ Commits.gitCommitSha lastCommit
        let newCommit = Commits.NewGitCommit commitMsg (Trees.treeSha tree) (V.singleton $ commitSha)
        commit <- mkGhRq $ Commits.createCommit auth (N name) (N owner) newCommit
        printG commit
        newCommitSha <- hoistMaybe NoCommit $ Commits.gitCommitSha commit
        let newReference = Refs.NewGitReference commentHead  $ Refs.untagName newCommitSha
        ref <- mkGhRq $ Refs.createReference auth (N name) (N owner) newReference
        printG ref
        let newPullRequest = PR.CreatePullRequest commitMsg "Please moderate comment." commentHead targetBranch
        pr <- mkGhRq $ PR.createPullRequest auth (N name) (N owner) newPullRequest
        printG pr

buildContent :: Text -> Text -> UTCTime ->Text
buildContent commenter comment time = [i|---
author: #{commenter}
published: #{formatTime defaultTimeLocale "%F %T" time}
---
#{comment}|]