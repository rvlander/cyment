{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Lib

import qualified Data.Text.IO         as T
import Data.Text (pack)
import Network.HTTP.Types.Status
import Text.StringRandom
import Data.Text         (Text)
import Data.Time.Clock (getCurrentTime)
import System.Environment (getEnv, lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Either

main =
  let 
    idPattern = pack . concat $  replicate 8 "[0-9a-z]"
  in 
    do
      githubToken <- getEnv "GITHUB_TOKEN"
      port <- lookupEnv "PORT"
      scotty (fromMaybe 3000 $ (port >>= readMaybe))  $ do
        post "/:namespace/:repo" $ rescue (do
          namespace <- param "namespace"
          repo <- param "repo"
          path <- param "path"
          commenter <- param "commenter"
          comment <- param "comment"
         --  redirectUrl <- param "redirect" 
          liftAndCatchIO $ do 
             commentId <- stringRandomIO idPattern
             datetime <- getCurrentTime
             res <- runEitherT $ do
                conf <- getConfig githubToken namespace repo
                pushCommitAndMakePR githubToken namespace repo conf commentId path commenter $ buildContent commenter comment datetime
             either (putStrLn . show) (putStrLn . show) res
          html "Thanks, your comment is waiting to be approved!") (\msg -> status badRequest400)
