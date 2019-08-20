{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getRepo,
    formatRepo
    ) where

import Data.Text         (Text)
import qualified Data.ByteString.Char8 as B
        

import qualified GitHub.Endpoints.Repos as Repos
import  GitHub.Data.Name
import qualified GitHub.Auth as Auth

getRepo :: Text -> Text -> IO (Either Repos.Error Repos.Repo)
getRepo name owner = 
    let auth = Auth.OAuth $ B.pack "71d3449c9551dfa363ad5dc365c125b7f19fd509"
    in  Repos.repository' (Just auth) (N name) (N owner)

formatRepo :: Repos.Repo -> Text
formatRepo = Repos.getUrl . Repos.repoUrl