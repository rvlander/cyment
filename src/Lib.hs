{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getRepo,
    formatRepo,
    pushCommit
    ) where

import Data.Text         (Text, pack)
import qualified Data.Text.IO        as T
import qualified Data.ByteString.Char8 as B
        

import qualified GitHub.Endpoints.Repos as Repos
import qualified GitHub.Endpoints.GitData.References as Refs
import qualified GitHub.Endpoints.GitData.Commits as Commits
import qualified GitHub.Endpoints.GitData.Trees as Trees
import qualified GitHub.Data.Definitions as G
import qualified Data.Vector as V
import  GitHub.Data.Name
import qualified GitHub.Auth as Auth
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Data.Either.Combinators (mapLeft)

data Error = 
     GitHubError G.Error
    | IOError Text
    | NoCommit
    deriving (Show)


getRepo :: Text -> Text -> IO (Either Repos.Error Repos.Repo)
getRepo name owner = 
    let auth = Auth.OAuth $ B.pack "71d3449c9551dfa363ad5dc365c125b7f19fd509"
    in  Repos.repository' (Just auth) (N name) (N owner)

formatRepo :: Repos.Repo -> Text
formatRepo = Repos.getUrl . Repos.repoUrl

printG :: Show a => a -> EitherT Error IO ()
printG = (handleIOEitherT  (\_ -> IOError "Could not print to screen!")) . T.putStrLn . pack . show

pushCommit :: Text-> Text -> Text -> IO(Either Error ())
pushCommit name owner content =
    let 
        auth = Auth.OAuth $ B.pack "71d3449c9551dfa363ad5dc365c125b7f19fd509" 
        mkGhRq = newEitherT . fmap (mapLeft GitHubError)
        
    in
    runEitherT $ 
    do
        ref <- mkGhRq $ Refs.reference' (Just auth) (N name) (N owner) (N "heads/master")
        printG ref
        lastCommit <- mkGhRq $ Commits.commit' (Just auth) (N name) (N owner) (N $ Refs.gitObjectSha $ Refs.gitReferenceObject ref)
        printG lastCommit
        let newTree = Trees.NewTree (Commits.treeSha $ Commits.gitCommitTree lastCommit) (V.singleton $ Trees.NewGitTree "test.commit" "100644" "A gited commit")
        tree <- mkGhRq $ Trees.createTree auth (N name) (N owner) newTree
        printG tree
        commitSha <- hoistMaybe NoCommit $ Commits.gitCommitSha lastCommit
        let newCommit = Commits.NewGitCommit "wonderfull commit" (Trees.treeSha tree) (V.singleton $ commitSha)
        commit <- mkGhRq $ Commits.createCommit auth (N name) (N owner) newCommit
        printG commit
        newCommitSha <- hoistMaybe NoCommit $ Commits.gitCommitSha commit
        let newReference = Refs.NewGitReference "refs/heads/un_test" $ Refs.untagName newCommitSha
        ref <- mkGhRq $ Refs.createReference auth (N name) (N owner) newReference
        printG ref